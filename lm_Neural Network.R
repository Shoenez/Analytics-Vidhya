library(keras)
library(tidyverse)
library(qdapRegex)
library(data.table)
library(doParallel)
registerDoParallel(4)

train <- read.csv("train_aj.csv")
test <- read.csv("test_v2.csv")
subm <- read.csv('s_sub.csv')
train_new2 <- train[complete.cases(train),]
test2 <- test

# Binary variables for gender
train_new2 <- subset(train_new2, select = -gender)

# 0 = no, 1 = yes
levels(train_new2$ever_married) <- c(0,1)

# Creating more binary variable columns for work_type
train_new2$children <- ifelse(train_new2$work_type=="children",1,0)
train_new2$Govt_job <- ifelse(train_new2$work_type=="Govt_job",1,0)

#Drop original column
train_new2 <- subset(train_new2, select = -work_type)

# Bin cols for Residence_type
train_new2 <- subset(train_new2, select = -Residence_type)


# Bin cols for smoking status
train_new2 <- subset(train_new2, select = -smoking_status)



# Same for test data
test2 <- subset(test, select = -gender)
levels(test2$ever_married) <- c(0,1)
test2$children <- ifelse(test2$work_type=="children",1,0)
test2$Govt_job <- ifelse(test2$work_type=="Govt_job",1,0)
test2 <- subset(test2, select = -work_type)
test2 <- subset(test2, select = -Residence_type)
test2 <- subset(test2, select = -smoking_status)
test2 <- test2 %>% replace(is.na(.), 0)

write.csv(train_new2, 'train_new2.csv', row.names = FALSE)
write.csv(test2, 'test_new2.csv', row.names = FALSE)
######################################################################################
train2_x <- subset(train_new2, select = -c(stroke,id))
train2_y <- train_new2$stroke
test2_x <- subset(test2, select = -c(id))
######################################################################################

model_keras2 <- keras_model_sequential()

model_keras2 %>% 
  
  # First hidden layer
  layer_dense(units= 1000, kernel_initializer = "uniform", activation= "relu", input_shape = ncol(train2_x)) %>% 
  layer_dropout(rate = 0.1) %>%
  layer_dense( units= 100, kernel_initializer = "uniform", activation= "relu") %>% 
  layer_dropout(rate = 0.2) %>%
  layer_dense( units= 50, kernel_initializer = "uniform", activation= "relu") %>% 
  layer_dropout(rate = 0.3) %>%
  layer_dense( units= 25, kernel_initializer = "uniform", activation= "relu") %>% 
  layer_dropout(rate = 0.3) %>%
  layer_dense(units= 1, kernel_initializer = "uniform",  activation = "sigmoid") %>% 
  
  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )

history <- fit(
  object           = model_keras2, 
  x                = as.matrix(train2_x), 
  y                = train2_y,
  batch_size       = 50, 
  epochs           = 10,
  validation_split = 0.1,
  callbacks = list(
    callback_model_checkpoint(paste0("stroke_model2.h5"), save_best_only = TRUE),
    callback_early_stopping(monitor = "val_loss", min_delta = 0, patience = 0,
                            verbose = 0, mode = c("auto", "min", "max"))
              )
)

model = load_model_hdf5(paste0("stroke_model2.h5"))
###########################################
#
# PREDICTION & SUBMISSON
#
###########################################

stroke = model %>%
  predict(as.matrix(test2_x), batch_size = 1024) %>%
  as.data.frame()
names(stroke)[1]<-"stroke"
test_ids <- test[1]
sub_keras <- cbind(test_ids, stroke)

write.csv(sub_keras, 'keras_solns2.csv', row.names = FALSE)
