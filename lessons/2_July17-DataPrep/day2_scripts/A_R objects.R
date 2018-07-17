#' Author: Ted Kwartler
#' Date: 7-4-2018
#' Purpose: R Data Types
#' 


# Numeric Vector
c(1,10,12,3.47)

# Boolean Vector
c(T, T, F, T, F)
c(TRUE,TRUE, FALSE, TRUE,FALSE)
c(T,TRUE, F, TRUE,FALSE)

# Factor Vectors
as.factor(c('MALE','FEMALE','FEMALE'))

# String Vectors
c('MALE','FEMALE','FEMALE')

# Matrix
as.matrix(warpbreaks[1:10, ])

# Data Frame
warpbreaks[1:10, ]

# Attributes of an object contain meta-information and can have unique identifiers
attributes(warpbreaks[1:10,])


# List craziness!
singleVal <- 123
singleDF <- data.frame(vec=c(1,2,3), vec2=c(4,5,6))
singleVec <-c(T,T,F,F,F)
nestedList<- list(nestedDF = data.frame(vec=c(1,2,3), vec2=c(4,5,6)),
                  nestedMatrix = matrix(c(1,2,3,4,5,6),ncol=3))

# Construct list
listA <- list(singleVal=singleVal,
              singleDF=singleDF,
              singleVec=singleVec,
              nestedList=nestedList)

# Get the first element of the list
listA[[1]]

# Get the 2nd element of the list
listA[[2]]

# Get the 3rd element of the list
listA[[3]]

# Get the 4th element of the list
listA[[4]]

# Get the 4th element, first object
listA[[4]][1]

# Get the 4th element, second object
listA[[4]][2]

# Its a best practice to name each element as the list complexity grows
listA$singleVec
listA$nestedList$nestedDF


# End