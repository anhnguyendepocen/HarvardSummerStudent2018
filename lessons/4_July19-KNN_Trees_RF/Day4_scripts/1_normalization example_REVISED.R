#' Author: Ted Kwartler
#' Data: 5-17-2018
#' Purpose: Show normalization in action
#' 

# Normalize Example
options(scipen=999)


# Make 2 fake vectors; one has larger values.
df <- data.frame(vec1 = c(1,2,3,4,5,6),
                 vec2 = c(10,20,30,40,50,60))
df

# Center and scale - subtract the mean of each vector and divide by the standard deviation
df.scaled <- scale(df, center=T)



