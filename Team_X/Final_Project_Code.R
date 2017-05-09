#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Albert Y. Kim
# Kaggle Competition Name: Titanic
# Kaggle Competition URL: https://www.kaggle.com/c/titanic
#-------------------------------------------------------------------------------

# 1. Load All Necessary Packages ------------------------------------------
library(tidyverse)
library(broom)




# 2. Load Data Files & Data Cleaning --------------------------------------
train <- read_csv("Files/train.csv") %>%
  mutate(is_female = ifelse(Sex=="female", 1, 0))
test <- read_csv("Files/test.csv") %>%
  mutate(is_female = ifelse(Sex=="female", 1, 0))
submission <- read_csv("Files/gender_submission.csv")





# 3. Top 4-5 Visualizations/Tables of EDA ---------------------------------
train %>%
  group_by(Sex, Survived) %>%
  summarise(Count = n()) %>%
  group_by(Sex) %>%
  mutate(
    prop_survived = Count/sum(Count),
    prop_survived = round(prop_survived, 3)
    )

survival_by_class <- train %>%
  mutate(Survived = ifelse(Survived==1, TRUE, FALSE))
ggplot(survival_by_class, aes(x=Pclass, fill=Survived)) +
  geom_bar(position="dodge") +
  labs(x="Class", y="Count", title="Survival by Class")





# 4. Cross-Validation of Final Model --------------------------------------
model_formula <- as.formula("Survived ~ Sex")

# Add folds and randomize
n_folds <- 5
train <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n()))


# Save scores here
score_vector <- rep(0, n_folds)

for(i in 1:n_folds){
  # 1. Create pseudo train/test based on folding scheme
  pseudo_train <- train %>%
    filter(fold != i)
  pseudo_test <- train %>%
    filter(fold == i)

  # 2. Fit model on training data
  model_logistic <- glm(model_formula, data=pseudo_train)

  # 3. Make predictions on test data & evaluate score
  score_vector[i] <- pseudo_test %>%
    mutate(
      p_hat = predict(model_logistic, newdata=pseudo_test, type="response"),
      predicted_survived = p_hat > 0.5
    ) %>%
    select(Survived, predicted_survived) %>%
    summarise(score=mean(Survived == predicted_survived)) %>%
    .[["score"]]
}

# Estimated score
mean(score_vector)






# 5. Create Submission ----------------------------------------------------
model_logistic <- glm(model_formula, data=train)
submission <- submission %>%
  mutate(
    p_hat = predict(model_logistic, newdata=test, type="response"),
    Survived = as.integer(p_hat > 0.5)
  ) %>%
  select(PassengerId, Survived)

submission %>%
  write_csv("Files/submission.csv")

# Submitting the above submission.csv file to Kaggle we get a Categorization
# Accuracy Score of 0.7655, which is close to the estimate score above






# 6. Extras ---------------------------------------------------------------

# Section A



# Section B



# Section C

