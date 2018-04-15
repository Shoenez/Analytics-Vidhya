library(dplyr)
library(tidyr)
library(leaps)

train <- read.csv("train_aj.csv")
test <- read.csv("test_v2.csv")
subm <- read.csv('s_sub.csv')
train <- train[complete.cases(train),]


Initial_model <- lm(stroke ~., train_new)
anova(Initial_model)
x1 <- train[2:10]
y1 <- train[11]
b <- regsubsets(stroke ~., data = train_new)
plot(b, scale = "adjr2")

Better_model <- lm(stroke ~. age + hypertension + heart_disease , data = train)
anova(Better_model)
stroke <- predict(Better_model, test, na.action = omit )
test_ids <- test[1]
sample_subm <- cbind(test_ids, stroke)
sample_subm <- sample_subm %>% replace(is.na(.), 0)
write.csv(sample_subm, 'First_solns.csv', row.names = FALSE)
