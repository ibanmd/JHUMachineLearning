# JHU Machine Learning, August 2014
## Course Project (Cleaning)
## Same process as the .Rmd file but less verbose 

## Read in the datasets from working directory
train <- read.csv("pml-training.csv", header=TRUE)
test <- read.csv("pml-testing.csv", header=TRUE)

## Finds number of NA values in each column
numOfNAs <- 1:dim(train)[2]
for(i in 1:dim(train)[2]){
  numOfNAs[i] <- sum(is.na(train[,i]))
}

## Print out the results
## table(numOfNAs)

## Subset to exclude the columns that primarily consist of NA values, and do this for test as well
train <- train[,numOfNAs==0]
test <- test[,numOfNAs==0]

## Display the first few column classes
## str(train[,1:8])

## Remove the date columns and "X" column
train <- train[,-c(1, 3, 4, 5)]
test <- test[,-c(1, 3, 4, 5)]

## Creates a logical vector telling which columns are of the class "integer"
colIsInt <- sapply(train, class)=="integer"
## Goes along the integer columns and makes them numeric
train[,colIsInt] <- lapply(train[,colIsInt],as.numeric)
test[,colIsInt] <- lapply(test[,colIsInt],as.numeric)

## Displays the number of columns with the class "factor" and "numeric"
## table(sapply(train,class))

## table(train$min_yaw_belt)

## Finds number of "" values in each column
numOfBlanks <- 1:dim(train)[2]
for(i in 1:dim(train)[2]){
  numOfBlanks[i] <- sum("" == train[,i])
}

## Print out the results
## numOfBlanks

colClass <- sapply(train,class)
quickcheck <- data.frame(numOfBlanks,colClass)
## quickcheck

## We use just the 3rd to the 35th value because the 1st, 2nd, and 36th are ones we want to keep
colIsBadFactor <- grep("factor",colClass)[3:35]

## Subsetting out the "factor" columns that are full of blanks
train <- train[,-colIsBadFactor]
test <- test[,-colIsBadFactor]

## Names of the second and third column
## names(train[,2:3])

## What's in these columns
## table(train$new_window)
## table(train$num_window[1:500])

## Remove columns 2 and 3
train <- train[,-c(2,3)]
test <- test[,-c(2,3)]

rm(quickcheck)
rm(colClass)
rm(colIsBadFactor)
rm(colIsInt)
rm(i)
rm(numOfBlanks)
rm(numOfNAs)

write.csv(train, "traincleaned.csv", row.names=FALSE)
write.csv(test, "testcleaned.csv", row.names=FALSE)
