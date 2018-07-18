
library('ggplot2')

setwd('~/HarvardSummerStudent2018/lessons/2_July17-DataPrep')

df<-read.csv('~/HarvardSummerStudent2018/book datasets/ToyotaCorolla.csv')

# ggplot(df, aes(Age_08_04, KM)) +
#   geom_point() +
#   geom_smooth()
# 
# ggplot(df, aes(Age_08_04, Price)) +
#   geom_point() +
#   geom_smooth()
# 
# ggplot(df, aes(KM, Price)) +
#   geom_point() +
#   geom_smooth()
# 
# ggplot(df, aes(Fuel_Type, HP)) +
#   geom_point() +
#   geom_boxplot()
# 
# ggplot(df, aes(Fuel_Type, CC)) +
#   geom_point() +
#   geom_boxplot()
# 
# ggplot(df, aes(CC, HP)) +
#   geom_point() +
#   geom_density2d()
# 
# ggplot(df, aes(Fuel_Type, KM)) +
#   geom_point() +
#   geom_boxplot()
# 
# ggplot(df, aes(Fuel_Type, Age_08_04)) +
#   geom_point() +
#   geom_boxplot()
# 
# ggplot(df, aes(Doors, Weight, group = Doors)) +
#   geom_point() +
#   geom_boxplot()

df$Fuel_Type_Dummy <- df$Fuel_Type %>% (function(v) return(match(v, unique(df$Fuel_Type))))
df$Color_Dummy <- df$Color %>% (function(v) return(match(v, unique(df$Color))))

set.seed(1)

df.random     <- sample(df)
df.training   <- df.random[(1):(floor(.5 * nrow(df))),]
df.validation <- df.random[(floor(.5 * nrow(df)) + 1):(floor(.8 * nrow(df))),]
df.testing    <- df.random[(floor(.8 * nrow(df)) + 1):(nrow(df)),]
