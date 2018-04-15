library(keras)
library(tidyverse)
library(qdapRegex)
library(data.table)
library(doParallel)
registerDoParallel(4)

train <- read.csv("train_aj.csv")
test <- read.csv("test_v2.csv")
subm <- read.csv('s_sub.csv')
train_new <- train[complete.cases(train),]
test_new <- test

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
test_new$Female <- ifelse(test_new$gender=="Female",1,0)
test_new$Male <- ifelse(test_new$gender=="Male",1,0)
test_new$Other <- ifelse(test_new$gender=="Other",1,0)
test_new <- subset(test_new, select = -gender)
levels(test_new$ever_married) <- c(0,1)
test_new$children <- ifelse(test_new$work_type=="children",1,0)
test_new$Govt_job <- ifelse(test_new$work_type=="Govt_job",1,0)
test_new$Never_worked <- ifelse(test_new$work_type=="Never_worked",1,0)
test_new$Private <- ifelse(test_new$work_type=="Private",1,0)
test_new$Self_employed <- ifelse(test_new$work_type=="Self-employed",1,0)
test_new <- subset(test_new, select = -work_type)
test_new$Urban <- ifelse(test_new$Residence_type=="Urban",1,0)
test_new$Rural <- ifelse(test_new$Residence_type=="Rural",1,0)
test_new <- subset(test_new, select = -Residence_type)
test_new$formerly_smoked <- ifelse(test_new$smoking_status=="formerly smoked",1,0)
test_new$never_smoked <- ifelse(test_new$smoking_status=="never smoked",1,0)
test_new$smokes <- ifelse(test_new$smoking_status=="smokes",1,0)
test_new <- subset(test_new, select = -smoking_status)
test_new <- test_new %>% replace(is.na(.), 0)

write.csv(train_new, 'train_new.csv', row.names = FALSE)
write.csv(test_new, 'test_new.csv', row.names = FALSE)
######################################################################################
train_x <- subset(train_new, select = -c(stroke,id))
train_y <- train_new$stroke
test_x <- subset(test_new, select = -c(id))
######################################################################################

model_keras <- keras_model_sequential()

model_keras %>% 
  
  # First hidden layer
  layer_dense(units= 21, kernel_initializer = "uniform", activation= "relu", input_shape = ncol(train_x)) %>% 
  layer_dropout(rate = 0.2) %>%
  layer_dense(units= 10, kernel_initializer = "uniform", activation= "relu") %>% 
  layer_dropout(rate = 0.3) %>%
  layer_dense(units= 1, kernel_initializer = "uniform",  activation = "sigmoid") %>% 
  
  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )

history <- fit(
  object           = model_keras, 
  x                = as.matrix(train_x), 
  y                = train_y,
  batch_size       = 50, 
  epochs           = 10,
  validation_split = 0.05,
  callbacks = list(
    callback_model_checkpoint(paste0("stroke_model.h5"), save_best_only = TRUE),
    callback_early_stopping(monitor = "val_loss", min_delta = 0, patience = 0,
                            verbose = 0, mode = c("auto", "min", "max"))
              )
)

model = load_model_hdf5(paste0("stroke_model.h5"))
###########################################
#
# PREDICTION & SUBMISSON
#
###########################################

stroke = model %>%
  predict(as.matrix(test_x), batch_size = 1024) %>%
  as.data.frame()
names(stroke)[1]<-"stroke"
test_ids <- test[1]
sub_keras <- cbind(test_ids, stroke)

write.csv(sub_keras, 'keras_solns.csv', row.names = FALSE)
