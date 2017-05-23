# setwd("/Users/Ben/Documents/Math218/Team_X/Files")

#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Ben Czekanski
# Kaggle Competition Name: Liberty Mutual Fire Peril
# Kaggle Competition URL: https://www.kaggle.com/c/liberty-mutual-fire-peril
#-------------------------------------------------------------------------------

# 1. Load All Necessary Packages ------------------------------------------
library(tidyverse)
library(rpart)

# The following are functions that I got from the kaggle discussions about this competition

WeightedGini <- function(solution, weights, submission){
  df = data.frame(solution = solution, weights = weights, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df$random = cumsum((df$weights/sum(df$weights)))
  totalPositive <- sum(df$solution * df$weights)
  df$cumPosFound <- cumsum(df$solution * df$weights)
  df$Lorentz <- df$cumPosFound / totalPositive
  n <- nrow(df)
  gini <- sum(df$Lorentz[-1]*df$random[-n]) - sum(df$Lorentz[-n]*df$random[-1])
  return(gini)
}

NormalizedWeightedGini <- function(solution, weights, submission) {
  WeightedGini(solution, weights, submission) / WeightedGini(solution, weights, solution)
}

# 2. Load Data Files & Data Cleaning --------------------------------------

# Data is from https://www.kaggle.com/c/liberty-mutual-fire-peril/data 
# train <- read.csv("train.csv", na.strings = c("NA", "Z"))
# test <- read.csv("test.csv", na.strings = c("NA", "Z"))
set.seed(39)

# 3. Top 4-5 Visualizations/Tables of EDA ---------------------------------

train %>%
  ggplot(aes(target)) +
  geom_histogram()

train %>%
  filter(target != 0) %>%
  ggplot(aes(target)) +
    geom_histogram()

ggplot(train, aes(var4, target)) + 
  geom_bar(stat = "identity")

ggplot(train, aes(weatherVar82, target)) + 
  geom_point()

ggplot(train, aes(weatherVar202, target)) +
  geom_point()


# 4. Cross-Validation of Final Model --------------------------------------

train2 <- train %>%
  sample_frac(1)

model_formula <- train2 %>%
  select(-dummy, -c(crimeVar1:weatherVar236)) %>%
  names() %>% 
  setdiff(c("target", "id")) %>% 
  stringr::str_c(collapse=" + ") %>%
  stringr::str_c("target ~ ", .)
model_formula <- as.formula(model_formula)


# CART

nfolds <- 10

train2 <- train2 %>%
  mutate(fold = rep(1:nfolds, length=n())) %>%
  arrange(fold)

scores <- rep(0, 10)


for(i in 1:10){

fold_scores <- rep(0, nfolds)

for (j in 1:nfolds){

  p_train <- train2 %>%
    filter(fold != j)
  p_test <- train2 %>%
    filter(fold == j)

knobs <- rpart.control(cp = (i/10000))
model_CART <- rpart(model_formula, data = p_train, control=knobs)

p_test1 <- p_test %>%
  mutate(pred = predict(model_CART, p_test)) %>%
  select(id, target, var11, pred)

if(j %% 1 == 0){
  print(j)
}
fold_scores[j] <- NormalizedWeightedGini(p_test1$target, p_test1$var11, p_test1$pred)
}
if(i %% 1 == 0){
  print(i)
}
scores[i] <- mean(fold_scores)
}

a <- tbl_df(scores)  %>%
  mutate(num = c(1:10))

ggplot(a, aes(x = num, y = value)) +
  geom_point()

# 5. Create Submission ----------------------------------------------------

knobs2 <- rpart.control(cp = (3/10000))
model_CART2 <- rpart(model_formula, data = train, control=knobs2)

test2 <- test %>%
  mutate(target = predict(model_CART2, test)) %>%
  select(id, target)

write_csv(test2, path = "Submission14.csv")


# Extra Stuff

# data_pca <- train %>%
#   select(-c(id, target, var2, var4, var5, var6, var9, dummy, var1,
#             var3, var12, var14, var16, crimeVar1, crimeVar2, crimeVar3,
#             crimeVar4, crimeVar5, crimeVar6, crimeVar7, crimeVar8, crimeVar9))
#   
# z <- princomp(formula = ~., data = data_pca, na.action=na.exclude, scores = TRUE)
# y <- as.table(z$loadings)
# x <- as.data.frame.matrix(y)
# 
# w <- as.data.frame.matrix(z$scores) %>%
#   select(1:10)

# train2 <- train %>%
#   select(id, target, var2, var4, var5, var6, var9, dummy, var11)

# train3 <- cbind(train2, w)
