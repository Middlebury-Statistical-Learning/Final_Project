#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Bianca Gonzalez and Emily Miller
# Kaggle Competition Name: Ghouls, Goblins and Ghosts...Boo!
# Kaggle Competition URL: https://www.kaggle.com/c/ghouls-goblins-and-ghosts-boo
#-------------------------------------------------------------------------------


# 1. Load All Necessary Packages ------------------------------------------
library(tidyverse)
library(stringr)
library(gmodels)
library(ggplot2)
library(caret)
library(gdata)
library(rpart)
library(broom)
library(repmis)
library(flexclust)



# Function for multiplots 
##http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}












# 2. Load Data Files & Data Cleaning ---------------------------------------------------------------
# Data downloaded from https://www.kaggle.com/c/ghouls-goblins-and-ghosts-boo/data

# Set Working Drive as Required
# For Bianca
#setwd("/Users/BiancaGonzalez/Desktop/RudeBoyMachineLearning/finalproject")
# For Emilia
#setwd("C:/Users/Emily Miller/Documents/Team_A")

train <- read_csv("Files/train.csv") %>%
  rename(monster_type = type) %>%
  mutate(
    monster_type = as.factor(monster_type),
    color = as.factor(color)) 

test <- read_csv("Files/test.csv") %>%
  mutate(color = as.factor(color)) 

submission <- read_csv("Files/sample_submission.csv")




# 3. Top 4-5 Visualizations/Tables of EDA ---------------------------------

# Data seems evenly distributed among all three monsters
train %>%
  group_by(monster_type) %>%
  count()

# Data is very clustered
q1 <- ggplot(train, aes(x=bone_length, y=hair_length, col = monster_type)) + geom_point()
q2 <- ggplot(train, aes(x=rotting_flesh, y=hair_length, col = monster_type)) +geom_point()
q3 <- ggplot(train, aes(x=rotting_flesh, y=has_soul, col = monster_type)) +geom_point()
q4 <- ggplot(train, aes(x=hair_length, y=has_soul, col = monster_type)) + geom_point()

multiplot(q1, q2, q3, q4, cols = 2)
#Has soul seems to be clustered more than others

# Color does not seem important to monster type
CrossTable(train$color, train$monster_type) 

# color and hair broken up by monster_type
moncolor <- ggplot(train, aes(x=color, y=hair_length)) + 
  geom_point() 
  
moncolor + facet_grid(. ~ monster_type)

# observations: 
# Hair length and Ghost type are correlated. Ghosts seem to have shorter hair (median .3in)
# Ghouls have the longest hair (even up to 1 inch! median .65), and goblins are stuck somewhere in between (.54 in)
# color seems evenly distributed across monster types

var_median<-train %>% 
  group_by(monster_type) %>% 
  summarise(hair_length_median = median(hair_length), rottingflesh_median = median(rotting_flesh), 
            bone_length_median = median(bone_length), has_soul_median = median(has_soul)) 

p1 <- ggplot(var_median, aes(x=monster_type, y = hair_length_median)) + geom_point()
p2 <- ggplot(var_median, aes(x=monster_type, y = rottingflesh_median)) + geom_point()
p3 <- ggplot(var_median, aes(x=monster_type, y = bone_length_median)) + geom_point()
p4 <- ggplot(var_median, aes(x=monster_type, y = has_soul_median)) + geom_point()

multiplot(p1, p2, p3, p4, cols=2)


# may be interesting to do a k means using this info: 
# EDA of hair length and has soul revisited: 
ggplot(train, aes(x=hair_length, y=has_soul, col = monster_type)) +
  geom_point()









# 4. Cross-Validation of Final Model --------------------------------------

# K nearest neighbors model
# Fit knn for Specific k 
# Let's fit knn using the following predictors: bone_length, rotting_flesh, hair_length, 
# and has_soul and for k = 175 which is arbitrarily chosen

# Clear out global environment, keeping test, train, and sample submission
keep(test, train, submission, sure = TRUE)

# Training Data:
# Define X predictors
train <- data.frame(train)
train_input <- train %>%
  select(bone_length, rotting_flesh, hair_length, has_soul)
# Define y outcome variable:
train_classifications <- train$monster_type

# Test Data: use the data to predict itself. Try to avoid overfitting

# Define X predictors
test_input <- train_input
test_classifications <- train_classifications

# Fit knn model
model_knn <- caret::knn3Train(train_input, test_input, train_classifications, k = 175, prob=TRUE)
probs <- attr(model_knn, "prob") %>%
  tbl_df()

# Score: The model is scored on just proportion correct:
# Note, to avoid overfitting need to cross validate
mean(model_knn == test_classifications)






# Cross Validation for Optimal k 
# Note the training data and test data inputs to knn() have to be the same size
# and just be the predictor variables.
keep(test, train, submission, sure = TRUE)

# Add folds
n_folds <- 5
train <- train %>%
  mutate(fold = rep(1:n_folds, length=n()))
# rep function allows us to replicate the values for folds of length vector
# n() number of obs in train dataset

k_vector <- seq(from = 1, to = 300, by = 1)
# now that our k vector is ready for KNN and our training dataset has folds, 
# lets create vector for scores.

# We'll keep track of proportion correct
prop_correct <- rep(0, length(k_vector))

for(i in 1:length(k_vector)){
  prop_correct_CV <- rep(0, n_folds)
  
  for(j in 1:n_folds){
    # Pseudo-Train
    train_input <- train %>%
      select(id, bone_length, rotting_flesh, hair_length, has_soul, fold, monster_type) %>%
      filter(fold != j)
    train_classifications <- train_input$monster_type
    train_input <- train_input %>%
      select(id, bone_length, rotting_flesh, hair_length, has_soul)
    
    # Pseudo-Test
    test_input <- train %>%
      select(id, bone_length, rotting_flesh, hair_length, has_soul, fold, monster_type) %>%
      filter(fold == j)
    test_classifications <- test_input$monster_type
    test_input <- test_input %>%
      select(id, bone_length, rotting_flesh, hair_length, has_soul)
    
    # Train model and get predictions
    model_knn <- knn3Train(train_input, test_input, train_classifications, k = k_vector[i], prob=TRUE)
    # Proportion correct - store in vectors created for scores
    prop_correct_CV[j] <- mean(test_classifications == model_knn)
    
  }
  
  prop_correct[i] <- mean(prop_correct_CV)
  
  par(mfrow=c(1,2))
  plot(x=k_vector[1:i], y=prop_correct[1:i])
}

# Save results:
results <- data_frame(
  k = k_vector[1:i],
  prop_correct = prop_correct[1:i]
  )

ggplot(results, aes(x=k, y=prop_correct)) +
  geom_point()

results <- results[order(results$prop_correct, decreasing = TRUE),]
View(results)

max(results$prop_correct) # Best k value is k = 196 or 173
# we ran this loop multiple times with different random folds. The data set is small enough
# that rerunning the loop results in different best k values. Another best k-value we returned
# fairly often is 173. We submitted csvs with predictions from both kvalues to Kaggle,
# and both retuned the same score 0.73157. Ultimately, we chose the smaller k value, 173. 












# 5. Create Submission ---------------------------------------------------------------
keep(test, train, submission, sure = TRUE)

# Training data:
classifications <- train$monster_type
train_input <- train %>%
  select(bone_length, rotting_flesh, hair_length, has_soul)

# Test data:
test_input <- test %>%
  select(bone_length, rotting_flesh, hair_length, has_soul)

# Fit model and get probabilities
model_knn <- knn3Train(train_input, test_input, classifications, k = 173, prob=TRUE, use.all = TRUE)
probs <- attr(model_knn, "prob") %>%
  tbl_df()
predictions <- data.frame(model_knn)

# Write to CSV for submission to kaggle
test %>%
  bind_cols(predictions) %>%
  select(id, type = model_knn) %>%
  write_csv("Files/submission.csv")


# submitted to Kaggle: score is 0.73346

# How does this compare to original approximation of correct from train data set?
train_input <- train %>%
  select(bone_length, rotting_flesh, hair_length, has_soul)
train_classifications <- train$monster_type
test_input <- train_input
test_classifications <- train_classifications
model_knn <- caret::knn3Train(train_input, test_input, train_classifications, k = 198, prob=TRUE)
mean(model_knn == test_classifications) 
# mean correct is 0.7385, this compares very closely to our kaggle score








# 6. Extras ---------------------------------------------------------------
keep(test, train, sure = TRUE)



# Section A
# Trial of Cart Model

# Convert all our categorical variables to numerical ones. We use
# the match() function to have the specified coding of the levels of the
# categorical variables.Rename type as monster_type.
train <- train %>%
  mutate(
    # Type: 0 = Ghost, 1 = Ghoul, 2 = Goblin
    monster_type = match(monster_type, c("Ghost", "Ghoul", "Goblin")) - 1,
    # Color: 0 = black, 1 = blood, 2 = blue, 3 = clear, 4 = green, 5 = white
    color = match(color, c("black", "blood", "blue", "clear", "green", "white")) - 1)

test <- test %>%
  mutate(
    # Color: 0 = black, 1 = blood, 2 = blue, 3 = clear, 4 = green, 5 = white
    color = match(color, c("black", "blood", "blue", "clear", "green", "white")) - 1)


# Fit and cross validate CART model 

# Create a 5 fold vector to cross validate
n_folds <- 5
train <- train %>%
  mutate(fold = rep(1:n_folds, length=n()))

# Create a knob vector from 1 to 5. Knobs cannot be greater than 5 due to the number of
# variables in the data set
knob_vector <- 1:5

# Create a blank vector to keep the proportion correct
prop_correct <- rep(0, length(knob_vector))

# Cross Validate across knobs
for(i in 1:length(knob_vector)){
  prop_correct_CV <- rep(0, n_folds)
  knobs <- rpart.control(maxdepth = i)
  
  for(j in 1:n_folds){
    # Pseudo-Train
    train_input <- train %>%
      filter(fold != j)
    
    # Pseudo-Test
    test_input <- train %>%
      filter(fold == j)
 
    # We fit the CART model:
    model_formula <- as.formula("monster_type ~ bone_length + rotting_flesh + hair_length + has_soul + color")
    model_CART <- rpart(model_formula, data = train_input, control=knobs)
    
    # Get Predictions and Proportion correct
    test_input$prediction <- round(predict(model_CART, test_input))
    prop_correct_CV[j] <- mean(test_input$prediction== test_input$monster_type)
    
  }
  
  prop_correct[i] <- mean(prop_correct_CV)
  
  par(mfrow=c(1,2))
  plot(x=knob_vector[1:i], y=prop_correct[1:i])
}


# Save results:
results <- data_frame(
  k = knob_vector[1:i],
  prop_correct = prop_correct[1:i]
)

ggplot(results, aes(x=k, y=prop_correct)) +
  geom_point()

results <- results[order(results$prop_correct, decreasing = TRUE),]
View(results)
# 4 is the best knob and has the highest average proportion correct of 0.66



# Write to CSV for submission to kaggle
knob <- rpart.control(maxdepth = 4)

# Fit CART model with best knob
model_formula <- as.formula("monster_type ~ bone_length + rotting_flesh + hair_length + has_soul + color")
model_CART <- rpart(model_formula, data = train, control=knob)

# Get Predictions and Proportion correct
test$prediction <- round(predict(model_CART, test))

# Type: 0 = Ghost, 1 = Ghoul, 2 = Goblin
test_csv<- test %>%
  mutate(type_predict = ifelse(prediction == 0, "Ghost", ifelse(prediction == 1, "Ghoul", "Goblin")))

# Write to CSV for submission to kaggle
# test_csv %>% 
  #select(id, type = type_predict) %>%
  #write.csv("CSVs/monsters_submit_CART.csv")

# 0.64272 Kaggle Score 
# Our cross validated score was .66 so we were in a ballpark range of what we thought our 
# score was going to be









# Section B
# Trial of K means model

rm(list=ls(all=TRUE))

train <- readr::read_csv("Files/train.csv") %>%
  rename(monster_type = type) %>%
  mutate(
    # Type: 0 = Ghost, 1 = Ghoul, 2 = Goblin
    monster_num = match(monster_type, c("Ghost", "Ghoul", "Goblin")) - 1,
    # Color: 0 = black, 1 = blood, 2 = blue, 3 = clear, 4 = green, 5 = white
    color = match(color, c("black", "blood", "blue", "clear", "green", "white")) - 1)

test <- readr::read_csv("Files/test.csv")

test_input <- test %>%
  select(hair_length, has_soul, rotting_flesh, bone_length)
  


# Prepare input data
input_data <- train %>%
  select(hair_length, has_soul, rotting_flesh, bone_length)

# Fit kMeans Clustering
results <- kcca(input_data, k=3)

# Add cluster results to main data:
train <- train %>%
  mutate(cluster = as.factor(clusters(results)))

# Clusters are not yet affiliated with a monster group. Need to figure out which cluster
# is the majority for each monster group.

# get table of clusters and monster type
cluster_table <- as.data.frame(CrossTable(train$cluster, train$monster_type))

# select cluster, monster_type, and frequency
cluster_table <- cluster_table %>%
  select(cluster = t.x, monster_type = t.y, Freq = t.Freq)
View(cluster_table)

# sort cluster table by frequency
cluster_table <- cluster_table[order(cluster_table$Freq, decreasing = TRUE),]
View(cluster_table)

# See clusters according to monster_type
cluster_table <- cluster_table[1:3,]
View(cluster_table)


# substitute monster_type name into train list
train <- train %>%
  mutate(cluster_predict = ifelse(cluster == as.numeric(cluster_table[1,1]), as.character(cluster_table[1,2]), 
                          ifelse(cluster == as.numeric(cluster_table[2,1]), as.character(cluster_table[2,2]), 
                                 as.character(cluster_table[3,2])))) %>%
  select(id, bone_length, rotting_flesh, hair_length, has_soul, color, monster_type, cluster_predict)

 

# proportion correct
mean(train$cluster_predict== train$monster_type)
# 73 percent correct, very close to prediciton accuracy of KNN

# Predict on Test to submit to Kaggle
# Function for predicting from kmeans 
predictions_cluster <- predict(results, test_input)

#make dataframe to bind 
predictions_cluster<-as.data.frame(predictions_cluster)

# Write to CSV for submission to kaggle
#test_csv<-test %>% 
  #bind_cols(predictions_cluster) %>% 
  #mutate(type_predict = ifelse(predictions_cluster == 1, "Ghoul", ifelse(predictions_cluster == 3, "Ghost", "Goblin"))) %>% 
  #select(id, type = type_predict) %>% 
  #write_csv("Files/submission_cluster.csv")
  
#Score on Kaggle is : 0.73157
# Same score as KNN model, but this model is hard to implement because of the ease in identifying
# which cluster is which monster type. Also we ran the model multiple times and found that the
# the proportion correct changes each time varying between 63 percent and 74 percent. The difficulty
# in cross validating this model and the fact that its highest score was equal to KNN is why we discarded
# this model. Therefore decided to implement the KNN model.
