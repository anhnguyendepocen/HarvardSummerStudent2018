
library(okcupiddata)
library(ggplot2)
library(dplyr)
library(DataExplorer)

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

# set.seed(1); profiles <- profiles[sample.int(nrow(profiles), 1000),]

DataExplorer::create_report(profiles)

for (v in names(profiles)) {
  printf('Percentage of missing record from profiles$%s: %.2f%%, (%d)', v, sum(is.na(profiles[,v])) / nrow(profiles) * 100, sum(is.na(profiles[,v])))
}

print('Distribution of Sex:'); t <- table(profiles$sex); t
ggplot(profiles %>%  filter(!is.na(age), !is.na(sex), age > 18, age < 80) %>% as_data_frame()) +
  geom_bar(aes(sex, fill = sex), position = 'dodge', stat = 'count') +
  ggtitle('Distribution of Sex') +
  xlab("Age") + ylab("Count")

print('Distribution of Age:'); t <- table(profiles$age); t
ggplot(profiles %>%  filter(!is.na(age), !is.na(sex), age > 18, age < 80) %>% as_data_frame() %>% select(sex, age) %>% group_by(sex, age) %>% summarise(n_age = n())) +
  geom_line(aes(age, n_age, color = sex)) +
  ggtitle('Distribution of Age') + theme(legend.position = 'none') +
  xlab("Age") + ylab("Count")

print('Distribution of Income:'); t <- table(profiles$income); t
# ggplot(profiles) + geom_histogram(mapping = aes(income), stat = 'count')

print('Orientation by Sex:'); t <- table(profiles$sex, profiles$orientation) / rowSums(table(profiles$sex, profiles$orientation)) * 100; t
# ggplot(as.data.frame(t)) + geom_col(mapping = aes(Var2, Freq, fill = Var1), position = 'dodge') + xlab('Orientation') + ylab('')

print('Income by Sex:'); t <- table(profiles$sex, profiles$income) / rowSums(table(profiles$sex, profiles$income)) * 100; t
# ggplot(as.data.frame(t)) + geom_col(mapping = aes(Var2, Freq, fill = Var1), position = 'dodge', stat = 'identity') + xlab('Income') + ylab('')

print('Drinks by Sex:'); t <- table(profiles$sex, profiles$drinks) / rowSums(table(profiles$sex, profiles$drinks)) * 100; t
# ggplot(as.data.frame(t)) + geom_col(mapping = aes(Var2, Freq, fill = Var1), position = 'dodge', stat = 'identity') + xlab('Drinks') + ylab('')

print('Drinks by Income:'); t <- table(profiles$income, profiles$drinks) / rowSums(table(profiles$income, profiles$drinks)) * 100; t
# ggplot(as.data.frame(t)) + geom_col(mapping = aes(Var1, Freq, fill = Var2), stat = 'identity') + xlab('Income') + ylab('')

print('Ethnicities:'); ethnicities <- unique(unlist(strsplit(profiles$ethnicity, ", "))); ethnicities

#######################
# Treat 'profiles'
#######################

## Treat profiles$ethnicity: split into columns
profiles <- strsplit.to.columns(profiles, 'ethnicity', ', ')

## Treat profiles$speaks: split into columns
profiles <- strsplit.to.columns(profiles, 'speaks', ', ')

## Treat profiles$job: split into columns
profiles <- strsplit.to.columns(profiles, 'job', ' / ')

## Treat profiles$education: split into columns
profiles <- strsplit.to.columns(profiles, 'education', ' / ')

## Treat profiles$religion: split into columns
profiles <- str.to.columns(profiles, 'religion')

dp <- vtreat::designTreatmentsZ(profiles, names(profiles))
df <- prepare(dp, profiles)

names(df)
