# Random Forests

library(randomForest)

train <- read.csv("train_aj.csv")
test <- read.csv("test_v2.csv")
subm <- read.csv('s_sub.csv')
train_new <- train[complete.cases(train),]

######################################################################
train_x <- subset(train_new, select = -c(stroke,id))
train_y <- train_new$stroke
test_x <- subset(test, select = -c(id))
#################################################################
str(train_new)
set.seed(100)
train_new <- subset(train_new, select = -id)
train_rf <- sample(nrow(train_new), 0.7*nrow(train_new), replace = FALSE)
TrainSet <- train_new[train_rf,]
ValidSet <- train_new[-train_rf,]
summary(TrainSet)
summary(ValidSet)

model1 <- randomForest(stroke ~ ., data = TrainSet, importance = TRUE)
model1
