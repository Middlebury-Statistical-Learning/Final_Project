# Jewel Chen
# Statistical Learning
# Final Project

library(tidyverse)
library(stringr)
library(caret)
library(ggvis)
library(gmodels)
options(java.parameters = "-Xmx2048m")
require('rJava')
library(XLConnect)

# load in data sets
train <- read.csv("/Users/Jewel/Desktop/College/Senior Year 2016-2017/Statistical Learning/Final Project/train.csv")
test <- read.csv("/Users/Jewel/Desktop/College/Senior Year 2016-2017/Statistical Learning/Final Project/test.csv")
submission <- "/Users/Jewel/Desktop/College/Senior Year 2016-2017/Statistical Learning/Final Project/submit.xlsx"

# check if the data needs to be normalized -> yes
summary(train)
summary(test)

# write normalization function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# clean target column, remove id, normalization data
target <- as.numeric(substring(train$target, 7))
train_norm <- cbind(train[2:94], target)
train_norm <- as.data.frame(lapply(train_norm, normalize))

test_norm <- as.data.frame(lapply(test[2:94], normalize))
  
# see how the features interact with each other
train_norm %>% ggvis(~feat_1, ~feat_2, fill=~target) %>% layer_points()

# create sample train and test sets to test and eval the model
sample_train <- train_norm[sample(nrow(train_norm), 2000),]
sample_train_input <- subset(sample_train, select = -c(target))
sample_train_classification <- as.vector(as.matrix(data.frame(train_target <- sample_train$target)))

new_train <- anti_join(train_norm, sample_train)

sample_test <- new_train[sample(nrow(new_train), 2000),]
sample_test_input <- subset(sample_test, select = -c(target))
sample_test_classification <- as.vector(as.matrix(data.frame(test_target <- sample_test$target)))

# create the knn model using caret and view probabilities; k = 10 is picked at random
knn_caret <- knn3Train(sample_train_input, sample_test_input, sample_train_classification, k=10)

# cv to pick optimal k value; use sample data set for cv because real data set is too large
n_folds <- 5
train_cv <- sample_train %>%
  mutate(fold = rep(1:n_folds, length=n()))

k_vector <- seq(from=10, to=450, by=10)

# keep track of multi-class log loss 
prop_correct <- rep(0, length(k_vector))

for(i in 1:length(k_vector)){
  prop_correct_CV <- rep(0, n_folds)
  
  for(j in 1:n_folds){
    # pseudo-train
    train_input <- train_cv %>%
      filter(fold != j)
    train_classifications <- train_input$target
    train_input <- subset(train_input, select=-c(target))
    
    # pseudo-test
    test_input <- train_cv %>%
      filter(fold == j)
    test_classifications <- test_input$target
    test_input <- subset(test_input, select = -c(target))
    
    # train model and get predictions
    model_knn <- knn3Train(train_input, test_input, train_classifications, k=k_vector[i], use.all=TRUE)
    
    # proportion correct
    prop_correct_CV[j] <- mean(test_classifications == model_knn)
  }
  prop_correct[i] <- mean(prop_correct_CV)
  print(paste("k =", k_vector[i]))
}

# visualize the results
ggplot(results, aes(x=k_vector, y=prop_correct)) +
  geom_point()

# determine which k value maximizes the prop_correct
results <- data.frame(k_vector, prop_correct)
max(results$prop_correct)
opt_k <- results[which(results$prop_correct == max(results$prop_correct)), ]

# fit the knn_class model on the test data and submit to kaggle
model_knn <- knn(train_norm[1:93], test_norm, train_norm$target, k=opt_k$k_vector, use.all=TRUE)
copy_model_knn <- model_knn

# undo normalization
levels(copy_model_knn) <- 1:9

# put data from copy_model_knn into a dataframe 
targets <- data.frame(copy_model_knn)
output <- data.frame(id = integer(),
                     Class_1 = integer(),
                     Class_2 = integer(),
                     Class_3 = integer(),
                     Class_4 = integer(),
                     Class_5 = integer(),
                     Class_6 = integer(),
                     Class_7 = integer(),
                     Class_8 = integer(),
                     Class_9 = integer())
# write to excel
for(i in 1:nrow(targets)){
  if(targets$copy_model_knn[i] == 1) {
    row = c(i, 1, 0, 0, 0, 0, 0, 0, 0, 0)
    output <- rbind(output, row)
  }  
  if(targets$copy_model_knn[i] == 2) {
    row = c(i, 0, 1, 0, 0, 0, 0, 0, 0, 0)
    output <- rbind(output, row)
  }
  if(targets$copy_model_knn[i] == 3) {
    row = c(i, 0, 0, 1, 0, 0, 0, 0, 0, 0)
    output <- rbind(output, row)
  }
  if(targets$copy_model_knn[i] == 4) {
    row = c(i, 0, 0, 0, 1, 0, 0, 0, 0, 0)
    output <- rbind(output, row)
  }
  if(targets$copy_model_knn[i] == 5) {
    row = c(i, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    output <- rbind(output, row)
  }
  if(targets$copy_model_knn[i] == 6) {
    row = c(i, 0, 0, 0, 0, 0, 1, 0, 0, 0)
    output <- rbind(output, row)
  }
  if(targets$copy_model_knn[i] == 7) {
    row = c(i, 0, 0, 0, 0, 0, 0, 1, 0, 0)
    output <- rbind(output, row)
  }
  if(targets$copy_model_knn[i] == 8) {
    row = c(i, 0, 0, 0, 0, 0, 0, 0, 1, 0)
    output <- rbind(output, row)
  }
  if(targets$copy_model_knn[i] == 9) {
    row = c(i, 0, 0, 0, 0, 0, 0, 0, 0, 1)
    output <- rbind(output, row)
  }
}

# write to excel and then submit; my computer runs of out Java heap space, despite my manually upping the heap space when running the program
write.xlsx2(output, file = submission, sheetName = "Sheet 1", col.names = TRUE)
