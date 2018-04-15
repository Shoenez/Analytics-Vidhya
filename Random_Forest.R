# Random Forests

library(randomForest)

train <- read.csv("train_aj.csv")
test <- read.csv("test_v2.csv")
subm <- read.csv('s_sub.csv')
train_new <- train[complete.cases(train),]

# Binary variables for gender
train_new$Female <- ifelse(train_new$gender=="Female",1,0)
train_new$Male <- ifelse(train_new$gender=="Male",1,0)
train_new$Other <- ifelse(train_new$gender=="Other",1,0)
train_new <- subset(train_new, select = -gender)

# 0 = no, 1 = yes
levels(train_new$ever_married) <- c(0,1)

# Creating more binary variable columns for work_type
train_new$children <- ifelse(train_new$work_type=="children",1,0)
train_new$Govt_job <- ifelse(train_new$work_type=="Govt_job",1,0)
train_new$Never_worked <- ifelse(train_new$work_type=="Never_worked",1,0)
train_new$Private <- ifelse(train_new$work_type=="Private",1,0)
train_new$Self_employed <- ifelse(train_new$work_type=="Self-employed",1,0)
#Drop original column
train_new <- subset(train_new, select = -work_type)

# Bin cols for Residence_type
train_new$Urban <- ifelse(train_new$Residence_type=="Urban",1,0)
train_new$Rural <- ifelse(train_new$Residence_type=="Rural",1,0)
train_new <- subset(train_new, select = -Residence_type)


# Bin cols for smoking status
train_new$formerly_smoked <- ifelse(train_new$smoking_status=="formerly smoked",1,0)
train_new$never_smoked <- ifelse(train_new$smoking_status=="never smoked",1,0)
train_new$smokes <- ifelse(train_new$smoking_status=="smokes",1,0)
train_new <- subset(train_new, select = -smoking_status)



# Same for test data
test$Female <- ifelse(test$gender=="Female",1,0)
test$Male <- ifelse(test$gender=="Male",1,0)
test$Other <- ifelse(test$gender=="Other",1,0)
test <- subset(test, select = -gender)
levels(test$ever_married) <- c(0,1)
test$children <- ifelse(test$work_type=="children",1,0)
test$Govt_job <- ifelse(test$work_type=="Govt_job",1,0)
test$Never_worked <- ifelse(test$work_type=="Never_worked",1,0)
test$Private <- ifelse(test$work_type=="Private",1,0)
test$Self_employed <- ifelse(test$work_type=="Self-employed",1,0)
test <- subset(test, select = -work_type)
test$Urban <- ifelse(test$Residence_type=="Urban",1,0)
test$Rural <- ifelse(test$Residence_type=="Rural",1,0)
test <- subset(test, select = -Residence_type)
test$formerly_smoked <- ifelse(test$smoking_status=="formerly smoked",1,0)
test$never_smoked <- ifelse(test$smoking_status=="never smoked",1,0)
test$smokes <- ifelse(test$smoking_status=="smokes",1,0)
test <- subset(test, select = -smoking_status)
test <- test %>% replace(is.na(.), 0)
######################################################################
train_x <- subset(train_new, select = -c(stroke,id))
train_y <- train_new$stroke
test_x <- subset(test, select = -c(id))
#################################################################

set.seed(100)
train_rf <- sample(nrow(train_new), 0.7*nrow(train_new), replace = FALSE)
TrainSet <- train_new[train_rf,]
ValidSet <- train_new[-train_rf,]

model1 <- randomForest(stroke ~ ., data = TrainSet, importance = TRUE)
model1
