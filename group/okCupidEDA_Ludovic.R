
library(okcupiddata)
library(ggplot2)
library(dplyr)
library(DataExplorer)
library(maps)
library(ggmap)
library(ggthemes)

options(scipen = 999)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# Imported from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Load 'profiles' data
data('profiles')
head(profiles)

# Load 'cities' and 'counties' data
cities <- us.cities[us.cities$country.etc == 'CA',] %>% mutate(city = tolower(sub(' CA', '', name))) %>% group_by(city) %>% summarize(pop, long, lat) %>% as.data.frame()
counties <- map_data('county', region = 'california') %>% filter(subregion %in% c('san francisco', 'san mateo', 'santa cruz', 'santa clara', 'alameda', 'contra costa', 'marin', 'sonoma', 'napa', 'solano'))

# Limit ourselves to people in California, they represent 99.85% of the data anyway.
print('Distribution of people living in and out of California'); table(grepl(", california", profiles$location))
profiles <- profiles[grepl(", california", profiles$location),]
profiles$location <- sub(", california", "", profiles$location)

# Add city information, some are missing coordinates in the 'cities' dataset so let's approximate them with a neightbor city
profiles$city_coord <- profiles$location
profiles$city_coord[profiles$city == 'emeryville'] <- 'oakland'
profiles$city_coord[profiles$city == 'burlingame'] <- 'san mateo'
profiles$city_coord[profiles$city == 'albany'] <- 'berkeley'
profiles$city_coord[profiles$city == 'belmont'] <- 'san mateo'
profiles$city_coord[profiles$city == 'pacifica'] <- 'south san francisco'
profiles$city_coord[profiles$city == 'stanford'] <- 'palo alto'
profiles$city_coord[profiles$city == 'martinez'] <- 'concord'
profiles$city_coord[profiles$city == 'san carlos'] <- 'redwood city'
profiles$city_coord[profiles$city == 'menlo park'] <- 'palo alto'
profiles$city_coord[profiles$city == 'benicia'] <- 'vallejo'
profiles$city_coord[profiles$city == 'pleasant hill'] <- 'concord'
profiles$city_coord[profiles$city == 'el cerrito'] <- 'richmond'
profiles$city_coord[profiles$city == 'san pablo'] <- 'richmond'

# Add cities specific coordinates
profiles <- profiles %>% left_join(cities, by = c('city_coord' = 'city'))

# Select profiles with limited distance to San Francisco
profiles <- profiles[sqrt((profiles$long - (-122.4194)) ^ 2 + (profiles$lat - (37.7749)) ^ 2) < 1,]

# Extracted top 10 'languages' from profiles
languages <- c("english", "spanish", "french", "chinese", "german", "japanese", "italian", "c++", "russian", "portuguese")
speaks <- languages %>% stringr::str_replace_all(' ', '_') %>% stringr::str_replace_all(stringr::fixed('+'), 'x') %>% as.data.frame %>% stringr::str_glue_data("speaks_{.}") %>% as.vector

# Transform 'speaks' into dummy variables
for (l in languages) {
  lk <- l %>% stringr::str_replace_all(' ', '_') %>% stringr::str_replace_all(stringr::fixed('+'), 'x')
  profiles[,sprintf('speaks_%s', lk)] <- stringr::str_detect(profiles$speaks, stringr::fixed(l)) %>% as.numeric
  profiles[,sprintf('speaks_%s_poorly', lk)] <- stringr::str_detect(profiles$speaks, stringr::fixed(sprintf('%s (poorly)', l))) %>% as.numeric
}

# Transform 'pets' into dummy variables
profiles$pets_has_dogs <- stringr::str_detect(profiles$pets, 'has dogs') %>% as.numeric
profiles$pets_like_dogs <- stringr::str_detect(profiles$pets, '^likes dogs') %>% as.numeric
profiles$pets_dislike_dogs <- stringr::str_detect(profiles$pets, 'dislikes dogs') %>% as.numeric
profiles$pets_has_cats <- stringr::str_detect(profiles$pets, 'has cats') %>% as.numeric
profiles$pets_like_cats <- stringr::str_detect(profiles$pets, '^likes cats') %>% as.numeric
profiles$pets_dislike_cats <- stringr::str_detect(profiles$pets, 'dislikes cats') %>% as.numeric
profiles$pets_no_info <- is.na(profiles$pets) %>% as.numeric

# Bin 'age' into 18-24, 25-29, 30-34, 35-39, 40-49, 50-59, 60+
profiles$age_bin <- as.factor(ifelse(profiles$age <= 25,'18-24',
                              ifelse(profiles$age <= 29,'25-29',
                              ifelse(profiles$age <= 34,'30-34',
                              ifelse(profiles$age <= 39,'35-39',
                              ifelse(profiles$age <= 49,'40-49',
                              ifelse(profiles$age <= 59,'50-59','60+')))))))

# Generate generic report on data
DataExplorer::create_report(profiles)

for (v in names(profiles)) {
  printf('Percentage of missing record from profiles$%s: %.2f%%, (%d)', v, sum(is.na(profiles[,v])) / nrow(profiles) * 100, sum(is.na(profiles[,v])))
}

# Only 4% of the data doesn't have city information
printf('Percent of data without coordinates: %f%%', nrow(profiles[is.na(profiles$long) | is.na(profiles$lat),]) / nrow(profiles) * 100);

print('Distribution of profiles by Location')
ggplot(profiles) +
  geom_polygon(data = counties, aes(x = long, y = lat, group = group)) +
  geom_point(data = profiles %>% group_by(long, lat) %>% summarize(n = n()), aes(x = long, y = lat, size = n))

print('Distribution of Sex:'); table(profiles$sex)
ggplot(profiles %>%  filter(!is.na(age), !is.na(sex), age > 18, age < 80) %>% as_data_frame()) +
  geom_bar(aes(sex, fill = sex), position = 'dodge', stat = 'count') +
  ggtitle('Distribution of Sex') +
  xlab("Age") + ylab("Count")

print('Distribution of Age:'); table(profiles$age %/% 4 * 4)
ggplot(profiles %>%  filter(!is.na(age), !is.na(sex), age > 18, age < 80) %>% as_data_frame() %>% select(sex, age) %>% group_by(sex, age) %>% summarise(n_age = n())) +
  geom_line(aes(age, n_age, color = sex)) +
  ggtitle('Distribution of Age') + theme(legend.position = 'none') +
  xlab("Age") + ylab("Count")

print('Distribution of Income:'); table(profiles$income);

print('Orientation by Sex:'); t <- table(profiles$sex, profiles$orientation) / rowSums(table(profiles$sex, profiles$orientation)) * 100; t

print('Income by Sex:'); t <- table(profiles$sex, profiles$income) / rowSums(table(profiles$sex, profiles$income)) * 100; t

print('Drinks by Sex:'); t <- table(profiles$sex, profiles$drinks) / rowSums(table(profiles$sex, profiles$drinks)) * 100; t

print('Drinks by Income:'); t <- table(profiles$income, profiles$drinks) / rowSums(table(profiles$income, profiles$drinks)) * 100; t

print('Ethnicities:'); ethnicities <- unique(unlist(strsplit(profiles$ethnicity, ", "))); ethnicities

print('Distribution of Age by Location')
df <- profiles %>%
  filter (!is.na(age)) %>%
  group_by(location, long, lat) %>%
  summarize(total = n(),
            mage = median(age),
            under = sum(age <= 30),
            over = sum(age > 30),
            ratio = log((under + 1) / (over + 1))) %>%
  as.data.frame(); df[,!(names(df) %in% c('long', 'lat'))]
multiplot(
  ggplot() +
    geom_path(data = counties, aes(x = long, y = lat, group = group)) +
    geom_point(data = df, aes(x = long, y = lat, size = mage, color = ratio)) +
    scale_color_gradient(low = 'red', high = 'green'),
  cols = 2)
  

#######################
# EDA on Speaks
#######################

print('Distribution of people with language info'); table(!is.na(profiles$speaks))
print('Distribution of people talking at least 1 of the top 10 languages'); table(rowSums(profiles[,names(profiles) %in% speaks]) >= 1)
print('Distribution of people talking at least 2 of the top 10 languages'); table(rowSums(profiles[,names(profiles) %in% speaks]) >= 2)
print('Distribution of people talking at least 1 of the top 10 languages out of English'); table(rowSums(profiles[,names(profiles) %in% speaks[2:length(speaks)]]) >= 1)
print('Distribution of people talking at least 2 of the top 10 languages out of English'); table(rowSums(profiles[,names(profiles) %in% speaks[2:length(speaks)]]) >= 2)
print('Distribution of people talking each of the top 10 languages'); colSums(profiles[,names(profiles) %in% speaks], na.rm = TRUE)

print('Distribution of Top 5 Languages spoken by Location - english, spanish, french, chinese and german')
df <- profiles %>%
  filter(!is.na(speaks)) %>%
  group_by(location, long, lat) %>%
  summarize(total = n(),
            english = sum(speaks_english) / total,
            spanish = sum(speaks_spanish) / total,
            french = sum(speaks_french) / total,
            chinese = sum(speaks_chinese) / total,
            german = sum(speaks_german) / total) %>%
  as.data.frame(); df[,!(names(df) %in% c('long', 'lat'))]
multiplot(
  ggplot() +
    geom_path(data = counties, aes(x = long, y = lat, group = group)) +
    geom_point(data = df, aes(x = long, y = lat, size = total, color = english)) +
    scale_color_gradient(low = 'red', high = 'green', limits = c(0, 1)) +
    ggtitle(label = 'Distribution of English') +
    theme_classic(),
  ggplot() +
    geom_path(data = counties, aes(x = long, y = lat, group = group)) +
    geom_point(data = df, aes(x = long, y = lat, size = total, color = spanish)) +
    scale_color_gradient(low = 'red', high = 'green', limits = c(0, 1)) +
    ggtitle(label = 'Distribution of Spanish') +
    theme_classic(),
  ggplot() +
    geom_path(data = counties, aes(x = long, y = lat, group = group)) +
    geom_point(data = df, aes(x = long, y = lat, size = total, color = french)) +
    scale_color_gradient(low = 'red', high = 'green', limits = c(0, 1)) +
    ggtitle(label = 'Distribution of French') +
    theme_classic(),
  ggplot() +
    geom_path(data = counties, aes(x = long, y = lat, group = group)) +
    geom_point(data = df, aes(x = long, y = lat, size = total, color = chinese)) +
    scale_color_gradient(low = 'red', high = 'green', limits = c(0, 1)) +
    ggtitle(label = 'Distribution of Chinese') +
    theme_classic(),
  ggplot() +
    geom_path(data = counties, aes(x = long, y = lat, group = group)) +
    geom_point(data = df, aes(x = long, y = lat, size = total, color = german)) +
    scale_color_gradient(low = 'red', high = 'green', limits = c(0, 1)) +
    ggtitle(label = 'Distribution of German') +
    theme_classic(),
  cols = 3)

#######################
# EDA on Pets
#######################

print('Distribution of Pets by Income'); table(profiles$pets, profiles$income)

print('Distribution of Pets lovers by Location')
df <- profiles %>%
  filter (!is.na(pets)) %>%
  group_by(location, long, lat) %>%
  summarize(total = n(),
            income = median(income, na.rm = TRUE),
            like = sum(pets_like_dogs | pets_has_dogs | pets_like_cats | pets_has_cats),
            dislike = sum(pets_dislike_dogs | pets_dislike_cats),
            bias = (like - dislike) / (like + dislike)) %>%
  as.data.frame(); df[,!(names(df) %in% c('long', 'lat'))]
ggplot() +
  geom_path(data = counties, aes(x = long, y = lat, group = group)) +
  geom_point(data = df, aes(x = long, y = lat, size = total, color = bias)) +
  scale_color_gradient(low = 'red', high = 'green', limits = c(-1, 1)) +
  ggtitle(label = 'Number of Pets lover') +
  theme_classic()

# print('Distribution of Dog vs Cats lovers by Location')
# df <- profiles %>%
#   filter (!is.na(pets)) %>%
#   group_by(location, long, lat) %>%
#   summarize(total = n(),
#             cats = sum(pets_like_cats | pets_has_cats),
#             dogs = sum(pets_like_dogs | pets_has_dogs),
#             bias = (dogs - cats) / (dogs + cats)) %>%
#   as.data.frame(); df[,!(names(df) %in% c('long', 'lat'))]
# ggplot() +
#   geom_path(data = counties, aes(x = long, y = lat, group = group)) +
#   geom_point(data = df, aes(x = long, y = lat, size = total, color = bias)) +
#   scale_color_gradient(low = 'red', high = 'green', limits = c(-1, 1)) +
#   ggtitle(label = 'Distribution of Dog vs Cats lovers by Location') +
#   theme_classic ()

print('Distribution of Dog vs Cats lovers by Language')
df <- bind_rows(
  profiles %>%
    filter (!is.na(pets), !is.na(speaks), speaks_english == 1) %>%
    mutate(language = 'english',
           petsfactor = ifelse(pets_has_dogs | pets_has_cats,'has pets',ifelse(pets_like_dogs | pets_like_cats,'like pets','dislike pets')) %>% as.factor()) %>%
    # group_by(sex) %>%
    select(sex, language, petsfactor),
  profiles %>%
    filter (!is.na(pets), !is.na(speaks), speaks_spanish == 1) %>%
    mutate(language = 'spanish',
           petsfactor = ifelse(pets_has_dogs | pets_has_cats,'has pets',ifelse(pets_like_dogs | pets_like_cats,'like pets','dislike pets')) %>% as.factor()) %>%
    # group_by(sex) %>%
    select(sex, language, petsfactor),
  profiles %>%
    filter (!is.na(pets), !is.na(speaks), speaks_french == 1) %>%
    mutate(language = 'french',
           petsfactor = ifelse(pets_has_dogs | pets_has_cats,'has pets',ifelse(pets_like_dogs | pets_like_cats,'like pets','dislike pets')) %>% as.factor()) %>%
    # group_by(sex) %>%
    select(sex, language, petsfactor),
  profiles %>%
    filter (!is.na(pets), !is.na(speaks), speaks_chinese == 1) %>%
    mutate(language = 'chinese',
           petsfactor = ifelse(pets_has_dogs | pets_has_cats,'has pets',ifelse(pets_like_dogs | pets_like_cats,'like pets','dislike pets')) %>% as.factor()) %>%
    # group_by(sex) %>%
    select(sex, language, petsfactor),
  profiles %>%
    filter (!is.na(pets), !is.na(speaks), speaks_german == 1) %>%
    mutate(language = 'german',
           petsfactor = ifelse(pets_has_dogs | pets_has_cats,'has pets',ifelse(pets_like_dogs | pets_like_cats,'like pets','dislike pets')) %>% as.factor()) %>%
    # group_by(sex) %>%
    select(sex, language, petsfactor)
) %>%
  as.data.frame(); df
ggplot(df) +
  geom_bar(aes(x = language, fill = petsfactor, group = petsfactor), stat = 'count', position = 'fill') +
  coord_flip()

print ('Bias of Cats vs Dogs by Age')
df <- profiles %>%
  filter(!is.na(pets), !is.na(sex), !is.na(age_bin), pets_like_dogs | pets_has_dogs | pets_like_cats | pets_has_cats) %>%
  group_by (sex, age_bin) %>%
  summarize(cats = sum(pets_like_cats | pets_has_cats),
            dogs = sum(pets_like_dogs | pets_has_dogs),
            bias = (dogs - cats) / (dogs + cats)) %>%
  as.data.frame(); df
ggplot(df) +
  geom_bar(aes(x = age_bin, y = bias, group = bias), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print('Bias of like|has Pets vs dislike Pets')
df <- profiles %>%
  filter(!is.na(pets), !is.na(sex), !is.na(age_bin)) %>%
  group_by (sex, age_bin, like = pets_like_cats | pets_has_cats | pets_like_dogs | pets_has_dogs) %>%
  summarize(total = n()) %>%
  mutate(total = ifelse(like,1,-1) * total) %>%
  as.data.frame(); df
ggplot(df) +
  geom_bar(aes(x = age_bin, y = total, group = like, fill = like), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Reach of population for Pets
df <- profiles %>%
  mutate(fitcondition = age <= 34 & (speaks_english | speaks_spanish | speaks_french) & (pets_has_dogs | pets_like_dogs)) %>%
  group_by(fitcondition) %>%
  summarize(total = n()) %>%
  as.data.frame(); df

# Reach of population for Drinking
df <- profiles %>%
  mutate(fitcondition = 21 <= age & age <= 35 & (stringr::word(religion, 1) == 'agnosticism' | stringr::word(religion, 1) == 'atheism')) %>%
  group_by(fitcondition) %>%
  summarize(total = n()) %>%
  as.data.frame(); df

# df <- profiles %>%
#   filter(!is.na(pets), !is.na(sex), !is.na(age_bin), pets_dislike_cats | pets_like_cats | pets_has_cats, sex == 'f') %>%
#   group_by (age_bin) %>%
#   summarize(like = sum(pets_like_cats | pets_has_cats),
#             dislike = sum(pets_dislike_cats),
#             bias = (like - dislike) / (like + dislike)) %>%
#   as.data.frame(); df
# ggplot(df) +
#   geom_bar(aes(x = age_bin, y = bias), stat = 'identity') +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# df <- profiles %>%
#   filter(!is.na(pets), !is.na(sex), !is.na(age_bin), pets_dislike_cats | pets_like_cats | pets_has_cats) %>%
#   group_by (sex, age_bin) %>%
#   summarize(like = sum(pets_has_cats),
#             dislike = sum(pets_like_cats | pets_dislike_cats),
#             bias = (like - dislike) / (like + dislike)) %>%
#   as.data.frame(); df
# ggplot(df) +
#   geom_bar(aes(x = age_bin, y = bias, fill = sex), stat = 'identity') +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

