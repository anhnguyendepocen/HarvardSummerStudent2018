
library(okcupiddata)
library(ggplot2)
library(dplyr)
library(DataExplorer)
library(maps)
library(ggmap)

options(scipen = 999)

printf <- function(...) invisible(print(sprintf(...)))

strsplit.to.columns <- function(df, column, split) {
  df.column <- strsplit(df[,column], split)
  for (v in unique(unlist(df.column))) {
    df[,sprintf('%s / %s', column, v)] <- ifelse(v %in% df.column,1,0)
  }
  # df[,column] <- NULL
  return(df)
}

str.to.columns <- function(df, column) {
  df.column <- df[,column]
  for (v in unique(df.column)) {
    df[,sprintf('%s / %s', column, v)] <- ifelse(ifelse(is.na(df.column),ifelse(is.na(v),TRUE,FALSE),v == df.column),1,0)
  }
  # df[,column] <- NULL
  return(df)
}

setwd('~/HarvardSummerStudent2018/group/')

# Load 'profiles' data
data('profiles')
head(profiles)

# Load 'cities' and 'counties' data
cities <- us.cities[us.cities$country.etc == 'CA',] %>% mutate(city = tolower(sub(' CA', '', name))) %>% group_by(city) %>% summarize(pop, long, lat) %>% as.data.frame()
counties <- map_data('county', region = 'california') %>% filter(subregion %in% c('san francisco', 'san mateo', 'santa cruz', 'santa clara', 'alameda', 'contra costa', 'marin', 'sonoma', 'napa', 'solano'))

# Limit ourselves to people in California, they represent 99.85% of the data anyway.
print('Distribution of people living in and out of California'); table(grepl(", california", profiles$location))
profiles <- profiles[grepl(", california", profiles$location),]

# Add city information, some are missing coordinates in the 'cities' dataset so let's approximate them with a neightbor city
profiles$city_coord <- sub(', .*', '', profiles$location)
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
# ggplot(profiles %>%  filter(!is.na(age), !is.na(sex), age > 18, age < 80) %>% as_data_frame()) +
#   geom_bar(aes(sex, fill = sex), position = 'dodge', stat = 'count') +
#   ggtitle('Distribution of Sex') +
#   xlab("Age") + ylab("Count")

print('Distribution of Age:'); table(profiles$age %/% 4 * 4)
# ggplot(profiles %>%  filter(!is.na(age), !is.na(sex), age > 18, age < 80) %>% as_data_frame() %>% select(sex, age) %>% group_by(sex, age) %>% summarise(n_age = n())) +
#   geom_line(aes(age, n_age, color = sex)) +
#   ggtitle('Distribution of Age') + theme(legend.position = 'none') +
#   xlab("Age") + ylab("Count")

print('Distribution of Income:'); table(profiles$income);
# ggplot(profiles) +
#   geom_histogram(mapping = aes(income), stat = 'count') +
#   scale_x_log10()

print('Orientation by Sex:'); t <- table(profiles$sex, profiles$orientation) / rowSums(table(profiles$sex, profiles$orientation)) * 100; t
# ggplot(as.data.frame(t)) +
#   geom_col(mapping = aes(Var2, Freq, fill = Var1), position = 'dodge') +
#   xlab('Orientation') +
#   ylab('')

print('Income by Sex:'); t <- table(profiles$sex, profiles$income) / rowSums(table(profiles$sex, profiles$income)) * 100; t
# ggplot(as.data.frame(t)) +
#   geom_col(mapping = aes(Var2, Freq, fill = Var1), position = 'dodge', stat = 'identity') +
#   xlab('Income') +
#   ylab('')

print('Drinks by Sex:'); t <- table(profiles$sex, profiles$drinks) / rowSums(table(profiles$sex, profiles$drinks)) * 100; t
# ggplot(as.data.frame(t)) + geom_col(mapping = aes(Var2, Freq, fill = Var1), position = 'dodge', stat = 'identity') + xlab('Drinks') + ylab('')

print('Drinks by Income:'); t <- table(profiles$income, profiles$drinks) / rowSums(table(profiles$income, profiles$drinks)) * 100; t
# ggplot(as.data.frame(t)) + geom_col(mapping = aes(Var1, Freq, fill = Var2), stat = 'identity') + xlab('Income') + ylab('')

print('Ethnicities:'); ethnicities <- unique(unlist(strsplit(profiles$ethnicity, ", "))); ethnicities

print('Distribution of Age by Location')
df <- profiles %>%
  filter (!is.na(age)) %>%
  group_by(city_coord, long, lat) %>%
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
  group_by(city_coord, long, lat) %>%
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
    ggtitle(label = 'Distribution of English'),
  ggplot() +
    geom_path(data = counties, aes(x = long, y = lat, group = group)) +
    geom_point(data = df, aes(x = long, y = lat, size = total, color = spanish)) +
    scale_color_gradient(low = 'red', high = 'green', limits = c(0, 1)) +
    ggtitle(label = 'Distribution of Spanish'),
  ggplot() +
    geom_path(data = counties, aes(x = long, y = lat, group = group)) +
    geom_point(data = df, aes(x = long, y = lat, size = total, color = french)) +
    scale_color_gradient(low = 'red', high = 'green', limits = c(0, 1)) +
    ggtitle(label = 'Distribution of French'),
  ggplot() +
    geom_path(data = counties, aes(x = long, y = lat, group = group)) +
    geom_point(data = df, aes(x = long, y = lat, size = total, color = chinese)) +
    scale_color_gradient(low = 'red', high = 'green', limits = c(0, 1)) +
    ggtitle(label = 'Distribution of Chinese'),
  ggplot() +
    geom_path(data = counties, aes(x = long, y = lat, group = group)) +
    geom_point(data = df, aes(x = long, y = lat, size = total, color = german)) +
    scale_color_gradient(low = 'red', high = 'green', limits = c(0, 1)) +
    ggtitle(label = 'Distribution of German'),
  cols = 2)

#######################
# EDA on Pets
#######################

print('Distribution of Pets by Income'); table(profiles$pets, profiles$income)

print('Distribution of Pets lovers and haters by Location')
df <- profiles %>%
  filter (!is.na(pets)) %>%
  group_by(city_coord, long, lat) %>%
  summarize(total = n(),
            income = median(income, na.rm = TRUE),
            like = sum(pets_like_dogs | pets_has_dogs | pets_like_cats | pets_has_cats),
            dislike = sum(pets_dislike_dogs | pets_dislike_cats),
            ratio = log((like + 1) / (dislike + 1))) %>%
  as.data.frame(); df[,!(names(df) %in% c('long', 'lat'))]
multiplot(
  ggplot() +
    geom_path(data = counties, aes(x = long, y = lat, group = group)) +
    geom_point(data = df, aes(x = long, y = lat, size = total, color = ratio)) +
    scale_color_gradient(low = 'red', high = 'green') +
    ggtitle(label = 'Number of Dogs+Cats lover'),
  ggplot() +
    geom_path(data = counties, aes(x = long, y = lat, group = group)) +
    geom_point(data = df, aes(x = long, y = lat, size = income, color = ratio)) +
    scale_color_gradient(low = 'red', high = 'green') +
    ggtitle(label = 'Mean Income of Dogs+Cats lover'),
  cols = 2)

print('Distribution of Dog vs Cats lovers by Location')
df <- profiles %>%
  filter (!is.na(pets)) %>%
  group_by(city_coord, long, lat) %>%
  summarize(total = n(),
            dog = sum(pets_like_dogs | pets_has_dogs),
            cat = sum(pets_like_cats | pets_has_cats),
            ratio = log((dog + 1) / (cat + 1))) %>%
  as.data.frame(); df[,!(names(df) %in% c('long', 'lat'))]
ggplot() +
  geom_path(data = counties, aes(x = long, y = lat, group = group)) +
  geom_point(data = df, aes(x = long, y = lat, size = total, color = ratio)) +
  scale_color_gradient(low = 'red', high = 'green')

print('Distribution of Pets lovers by Language and Sex')
df <- bind_rows(
  profiles %>%
    filter (!is.na(pets), !is.na(speaks)) %>%
    group_by(sex) %>%
    summarize(language = 'english',
              pets_has = sum(speaks_english & (pets_like_dogs | pets_like_cats)),
              pets_like = sum(speaks_english & (pets_like_dogs | pets_like_cats)),
              pets_dislike = sum(speaks_english & (pets_dislike_dogs | pets_dislike_cats))),
  profiles %>%
    filter (!is.na(pets), !is.na(speaks)) %>%
    group_by(sex) %>%
    summarize(language = 'spanish',
              pets_has = sum(speaks_spanish & (pets_has_dogs | pets_has_cats)),
              pets_like = sum(speaks_spanish & (pets_like_dogs | pets_like_cats)),
              pets_dislike = sum(speaks_spanish & (pets_dislike_dogs | pets_dislike_cats))),
  profiles %>%
    filter (!is.na(pets), !is.na(speaks)) %>%
    group_by(sex) %>%
    summarize(language = 'chinese',
              pets_has = sum(speaks_chinese & (pets_has_dogs | pets_has_cats)),
              pets_like = sum(speaks_chinese & (pets_like_dogs | pets_like_cats)),
              pets_dislike = sum(speaks_chinese & (pets_dislike_dogs | pets_dislike_cats))),
  profiles %>%
    filter (!is.na(pets), !is.na(speaks)) %>%
    group_by(sex) %>%
    summarize(language = 'french',
              pets_has = sum(speaks_french & (pets_has_dogs | pets_has_cats)),
              pets_like = sum(speaks_french & (pets_like_dogs | pets_like_cats)),
              pets_dislike = sum(speaks_french & (pets_dislike_dogs | pets_dislike_cats))),
  profiles %>%
    filter (!is.na(pets), !is.na(speaks)) %>%
    group_by(sex) %>%
    summarize(language = 'german',
              pets_has = sum(speaks_german & (pets_has_dogs | pets_has_cats)),
              pets_like = sum(speaks_german & (pets_like_dogs | pets_like_cats)),
              pets_dislike = sum(speaks_german & (pets_dislike_dogs | pets_dislike_cats))),
  ) %>%
  as.data.frame(); df
ggplot(df) +
  geom_bar(aes(language, fill = pets_has, group = language), stat = 'identity', position = 'fill') +
  coord_flip()

analyzedRel<-data.frame(profiles$religion, profiles$drinks)
drinks_count<-group_by(analyzedRel[, c("data.drinks", "data.religion")], data.drinks, data.religion) %>% summarize(n_drinks = n())
religion_count<-group_by(analyzedRel[c("data.religion")],data.religion) %>% summarize(n_religion = n())
analyzedRel <- left_join(drinks_count, religion_count, by = "data.religion") %>% mutate(freq = n_drinks/n_religion, 
                                                                                        freq = ifelse(is.na(freq), 0, freq), delta_percent = 200 * (freq - 0.5))
library(RColorBrewer)
n.color <- length(unique(analyzedRel$data.religion))
getPalette = colorRampPalette(brewer.pal(6, "Dark2"))
p <- ggplot(analyzedRel, aes(x = data.drinks, y = n_drinks, fill = data.religion,group = data.drinks)) + 
  geom_bar(stat = "identity") + ggtitle("Religion&Drinking") + 
  scale_fill_manual(values = getPalette(n.color)) + ylab("") + xlab("") + 
  coord_flip()+theme(legend.position = "bottom") + guides(fill = guide_legend(nrow = 2))
print(p)

train()

#######################
# Treat 'profiles'
#######################

## Treat profiles$ethnicity: split into columns
profiles <- strsplit.to.columns(profiles, 'ethnicity', ', ')

## Treat profiles$job: split into columns
profiles <- strsplit.to.columns(profiles, 'job', ' / ')

## Treat profiles$education: split into columns
profiles <- strsplit.to.columns(profiles, 'education', ' / ')

## Treat profiles$religion: split into columns
profiles <- str.to.columns(profiles, 'religion')

dp <- vtreat::designTreatmentsZ(profiles, names(profiles))
df <- prepare(dp, profiles)

names(df)



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
