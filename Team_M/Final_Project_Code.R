#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Shannia Fu
# Kaggle Competition Name: Kobe Bryant Shot Selection
# Kaggle Competition URL: https://www.kaggle.com/c/kobe-bryant-shot-selection
#-------------------------------------------------------------------------------

# 1. Load All Necessary Packages ------------------------------------------
library(tidyverse)
library(broom)
library(ggplot2)
library(ROCR)


# 2. Load Data Files & Data Cleaning --------------------------------------
data <- read.csv("Files/data.csv")
# data with all predictors
train <- data %>%
  filter(!is.na(shot_made_flag))
test <- data %>% 
  filter(is.na(shot_made_flag))

# I only want numerical data - lat, lon, loc_x, loc_y, minutes_remaining,
# period, playoffs, seconds_remaining, shot_distance, shot_type
# and the thing I'm trying to predict is shot_made_flag (1 for did make, 
# 0 for didn't)

train <- train %>% 
  select(shot_made_flag, lat, lon, loc_x, loc_y, minutes_remaining,
         period, playoffs, seconds_remaining, shot_distance, shot_type)

test <- test %>% 
  select(shot_id, shot_made_flag, lat, lon, loc_x, loc_y, minutes_remaining,
         period, playoffs, seconds_remaining, shot_distance, shot_type)

# I want to convert the shot_type to an actual number (2 or 3, not 
# 2PT Field Goal, etc.),

train <- train %>% 
  mutate(shot_type = (ifelse(shot_type == "2PT Field Goal", 2, 
                             ifelse(shot_type == "3PT Field Goal", 3, 0))))
test <- test %>% 
  mutate(shot_type = (ifelse(shot_type == "2PT Field Goal", 2, 
                             ifelse(shot_type == "3PT Field Goal", 3, 0))))

# and I want time remaining to be a combination of minutes & seconds, 
# without them being separate.

train <- train %>% 
  mutate(time_remaining = round(minutes_remaining + seconds_remaining/60, 3)) %>% 
  select(-minutes_remaining, -seconds_remaining)

test <- test %>% 
  mutate(time_remaining = round(minutes_remaining + seconds_remaining/60, 3)) %>% 
  select(-minutes_remaining, -seconds_remaining)

# submission <- read_csv("Files/gender_submission.csv")





# 3. Top 4-5 Visualizations/Tables of EDA ---------------------------------

# Below are some visualizations of proportion shots made as correlated with
# time remaining, distance, shot type, and period. Obviously some are more
# correlated (e.g. distance closer -> higher proportion shots made), while
# others seem to be less correlated


# by time
graph_time <- train %>%
  group_by(time_remaining, shot_made_flag) %>%
  summarise(Count = n()) %>%
  group_by(time_remaining) %>%
  mutate(prop_made = round(Count/sum(Count), 3)) %>% 
  filter(shot_made_flag == 1)
  
ggplot(graph_time, aes(x = time_remaining, y = prop_made)) +
  geom_bar(stat = "identity") +
  labs(x = "Time remaining", y = "Proportion of shots made", title = "Shots made by time")

# by shot type (2 vs. 3 pt field goals)
graph_shot_type <- train %>%
  group_by(shot_type, shot_made_flag) %>%
  summarise(Count = n()) %>%
  group_by(shot_type) %>%
  mutate(prop_made = round(Count/sum(Count), 3)) %>% 
  filter(shot_made_flag == 1)

ggplot(graph_shot_type, aes(x = as.factor(shot_type), y = prop_made)) +
  geom_bar(stat = "identity") +
  labs(x = "Shot Type", y = "Proportion of shots made", title = "Shots made by type")

# by distance
graph_distance <- train %>%
  group_by(shot_distance, shot_made_flag) %>%
  summarise(Count = n()) %>%
  group_by(shot_distance) %>%
  mutate(prop_made = round(Count/sum(Count), 3)) %>% 
  filter(shot_made_flag == 1)

ggplot(graph_distance, aes(x = as.factor(shot_distance), y = prop_made)) +
  geom_bar(stat = "identity") +
  labs(x = "Shot Distance", y = "Proportion of shots made", title = "Shots made by distance")

# distance attempts, to see that he attempted shots from further away that
# weren't shown above, by proportion
graph_distance_attempts <- train %>%
  group_by(shot_distance, shot_made_flag) %>%
  summarise(Count = n()) %>%
  group_by(shot_distance)

ggplot(graph_distance_attempts, aes(x = as.factor(Count), y = shot_distance, fill = as.factor(shot_made_flag))) +
  geom_bar(stat = "identity") +
  labs(x = "Shot Distance", y = "Number of shots attempted", title = "Shots made by distance")

# by period
graph_period <- train %>%
  group_by(period, shot_made_flag) %>%
  summarise(Count = n()) %>%
  group_by(period) %>%
  mutate(prop_made = round(Count/sum(Count), 3)) %>% 
  filter(shot_made_flag == 1)

ggplot(graph_period, aes(x = as.factor(period), y = prop_made)) +
  geom_bar(stat = "identity") +
  labs(x = "Shot Distance", y = "Proportion of shots made", title = "Shots made by distance")



# 4. Cross-Validation of Final Model --------------------------------------


model_formula <- as.formula("shot_made_flag ~ lat + lon + loc_x + loc_y + time_remaining +
         period + playoffs + shot_distance + shot_type")

model_logistic <- glm(model_formula, data=train, family="binomial")
train <- train %>% 
  mutate(p_shot_made_flag = round(predict(model_logistic, type="response"), 3))

pred <- prediction(predictions = train$p_shot_made_flag, labels = train$shot_made_flag)
perf <- performance(pred, "tpr","fpr")

auc <- as.numeric(performance(pred,"auc")@y.values)
auc

# Plot
plot(perf, main=paste("Area Under the Curve =", round(auc, 3)))
abline(c(0, 1), lty=2)


# Add folds and randomize
n_folds <- 5
train <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n()))


# Save scores here
score_vector <- rep(0, n_folds)

for(k in 1:n_folds){
  # create pseudo train/test based on folding scheme
  pseudo_train <- train %>%
    filter(fold != k)
  pseudo_test <- train %>%
    filter(fold == k)
  
  # new model based on pseudo_train
  model_logistic <- glm(model_formula, data=pseudo_train, family="binomial")
  pseudo_train <- pseudo_train %>% 
    mutate(p_shot_made_flag = round(predict(model_logistic, type="response"), 3))
  
  # set min error as highest possible so comparatively the actual min error will be
  # lower
  max_prop <- 0
  
  # list of p* values to test
  p_star_vals <-  seq(min(pseudo_train$p_shot_made_flag) + 0.001, max(pseudo_train$p_shot_made_flag) - 0.001, 0.001)
  
  # set optimal p_star as 0 (a value that will isn't even hit)
  p_star <- 0
  
  # test all possible p* values
  for (i in p_star_vals){
    # mutate using probabilities > p*
    pseudo_train <- pseudo_train %>% 
      mutate(shot_guess=ifelse(pseudo_train$p_shot_made_flag > i, 1, 0))
    
    # predict on pseudo_test
    pseudo_test <- pseudo_test %>% 
      mutate(p_shot_made_flag = round(predict(model_logistic, newdata=pseudo_test, type="response"), 3)) %>% 
      mutate(shot_guess = ifelse(p_shot_made_flag > i, 1, 0))
    
    # create contingency table
    mytable <- as.data.frame(table(pseudo_test$shot_made_flag,pseudo_test$shot_guess))
    
    # find proportion of correct guesses
    prop_correct <- (mytable$Freq[1] +mytable$Freq[4])/sum(mytable$Freq)
    if(prop_correct > max_prop) {
      max_prop = prop_correct
      p_star = i
    }
  }

  # add p_star to vector
  score_vector[k] <- p_star
}

# Estimated score
mean(score_vector)

# the average p_star was 0.5554, but it depends on how the data is split up for
# pseudo_train

# 5. Create Submission ----------------------------------------------------

model_logistic <- glm(model_formula, data=train, family="binomial")
test <- test %>% 
  mutate(p_shot_made_flag = round(predict(model_logistic, newdata=test, type="response"), 3)) %>% 
  mutate(shot_made_flag = ifelse(p_shot_made_flag > mean(score_vector), 1, 0))

submission <- test %>% 
  select(shot_id, shot_made_flag)


submission %>%
  write_csv("Files/submission.csv")

# Submitting the above submission.csv file to Kaggle we get a Log Loss score of 
# 13.51164, which is pretty bad I guess. More about this in the write-up.


# 6. Extras ---------------------------------------------------------------

# I thought about using regular regression, but it seemed appropriate to use
# logit instead, like in PS07


