library(tidyverse)
library(stringr)
library(broom)
library(glmnet)
library(plotly)

#Allstate claims data


#--load data--
train<- read_csv("~/Google Drive/Math/final_train.csv")
test<- read_csv("~/Google Drive/Math/final_test.csv")
sample_sub <- read_csv("~/Google Drive/Math/final_sample_submission.csv")

train1 <- train %>%
  sample_frac(0.5)

#create dummy var (useful for lasso)
train1 <- train1 %>% 
  mutate(dummy_var = 1)
test <- test %>% 
  mutate(dummy_var = 1)


#exploratory data analysis
glimpse(train)
#there are 132 variables - 116 categorical and 14 continuous, pluss loss and ID
cont_train<- train %>%
  select(loss, id, contains("cont"))
cat_train <- train %>%
  select(loss, id, contains ("cat"))

#literally just graphs of continuous variables and loss because I dont know which variables to use
#nothing is named 
#select 50% to reduce time
cont_train1<- cont_train %>%
  sample_frac(0.5)
ggplot(cont_train1, aes(x=cont1, y=loss)) +
  geom_point()
#cont 1 has no obvious correlation
ggplot(cont_train1, aes(x=cont2, y=loss)) +
  geom_point()
#cont2 is actually discrete and loss seems pos correlated
ggplot(cont_train1, aes(x=cont3, y=loss)) +
  geom_point()
#slightly pos correlated
ggplot(cont_train1, aes(x=cont4, y=loss)) +
  geom_point()
#slightly neg correlated
ggplot(cont_train1, aes(x=cont5, y=loss)) +
  geom_point()
#slightly neg corr as well
ggplot(cont_train1, aes(x=cont6, y=loss)) +
  geom_point()
#no real correlation
ggplot(cont_train1, aes(x=cont7, y=loss)) +
  geom_point()
#weird jump between 0 and 0.15 ish ... slightly pos
ggplot(cont_train1, aes(x=cont8, y=loss)) +
  geom_point()
#no real correlation, maybe slightly negative
ggplot(cont_train1, aes(x=cont9, y=loss)) +
  geom_point()
#higher values in the middle
ggplot(cont_train1, aes(x=cont10, y=loss)) +
  geom_point()
#slightly pos except it decreases at the end
ggplot(cont_train1, aes(x=cont11, y=loss)) +
  geom_point()
#pos correlation
ggplot(cont_train1, aes(x=cont12, y=loss)) +
  geom_point()
#also slightly pos
ggplot(cont_train1, aes(x=cont13, y=loss)) +
  geom_point()
#bimodal .. what to do with this one
ggplot(cont_train1, aes(x=cont14, y=loss)) +
  geom_point()
#oddly jumpy

#--make model--
#first model -> basic linear using only some continous variables. 
#i picked 2, 3, 4, 9, 11 for this run
model_linear <- lm(loss~ cont2 + cont3 + cont4 + cont9 + cont11 ,data = cont_train)
model_augmented <- model_linear %>%
  augment() %>%
  tbl_df()
#predict
model_test <- augment(model_linear, newdata= test) %>%
  rename(loss = .fitted) %>%
  select(id, loss)

#submission 1 
sample_sub %>% 
  mutate(loss = as.vector(model_test$loss)) %>% 
  write_csv("~/Desktop/PS06_FIRSTNAME_LASTNAME_submission.csv")
#score 1932.86963

#ways to go forward: include categorical variables in model, try something like loess or splines

#--- exploratory data analysis part 2 : Categorical ---
# i cant make a bar graph for all 116 categorical variables (or I could but it'd take forever!)
# so im going to randomly 
cat_train1<- cat_train %>%
  sample_frac(0.5)
logloss<- log(cat_train1$loss)
j <- floor(runif(1, min = 1, max = 116))
j
#I don't want to have to reenter the value every time but this is the best i've got
boxplot(logloss~ cat83, data=cat_train1, 
        xlab="j", ylab="loss")
#21 A has more spread than B, so does 58 but B is larger
#93 has 5 relatively even levels
#107 has A-U, all relatively even as well
#25 has two very spread A and B
#35 has A more spread than B
#78 has 4 . B is most spread. D is the highest
#108 A-U even ish
#83 A-D same medians

#- (random) loess model - 
#select random variables to include in the model
j <- floor(runif(8, min = 1, max = 116))
k <- floor(runif(4, min = 1, max = 14))
j
k

#code comes from shrinkage.r 
#there should be a better way to enter variables but again here we are
#i found a way to include ALL the variables - that will be another try
model_formula <- 
  "dummy_var ~ cat11 + cat25 + cat37 + cat109 + cat15 + cat56 + cat27 + cat90 + cont8 + cont4 + cont7 + cont11" 
model_formula <- as.formula(model_formula)

X <- model.matrix(model_formula, data = train1)[, -1]
y <- train1$loss

#for round 1 i'm not cross validating lambda yet. this is just to try differenc combinations of variables
model_lasso <- glmnet(X, y, alpha = 1)
model_lasso %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line()
ggplotly()
#oh my this is really something
#log(lambda) of 3 looks interesting

# --- predictions---
test_X <- model.matrix(model_formula, data = test)[, -1]

predictions <- model_lasso %>%
  predict(newx= test_X, s = exp(2))

# Write submissions to CSV
sample_sub %>% 
  mutate(loss = as.vector(predictions)) %>% 
  write_csv("~/Desktop/final_submission.csv")
#did 2 random combinations in one day
# first was cat69 + cat67 + cat32 + cat82 + cat78 + cat100 + cat40 + cont11 + cont6 + cont4 + cont2
#this one got a score of 1749.05471. getting better! 
#second was cat25 + cat91 + cat82 + cat33 + cat11 + cat64 + cat31 + cat65 + cont8 + cont4 + cont7 + cont5
#score = 1814.62783 not as good. still valuable to learn 


