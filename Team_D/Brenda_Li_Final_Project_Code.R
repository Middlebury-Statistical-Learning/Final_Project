#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Brenda Li
# Kaggle Competition Name: Africa Soil Property Prediction
# Kaggle Competition URL: https://www.kaggle.com/c/afsis-soil-properties
#-------------------------------------------------------------------------------

# 1. Load All Necessary Packages ------------------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(glmnet)
library(tidyverse)
library(broom)
library(grid)
library(gridExtra)
library(knitr)



# 2. Load Data Files & Data Cleaning --------------------------------------
training<-read.csv("Files/soil_training.csv") %>%
  mutate(int_depth=ifelse(Depth=="Subsoil",1,0)) %>%  #Changing the Depth variable to numerical instead of categorical
  select(-Depth)

test<-read.csv("Files/soil_test.csv")





# 3. Top 4-5 Visualizations/Tables of EDA ---------------------------------

# EDA of Outcome Variables

# Histograms of the distributions of each outcome variable
p1<-ggplot(training,aes(x=Ca))+geom_histogram(bins=50)+ggtitle("Distribution of Ca Values")
p2<-ggplot(training,aes(x=P))+geom_histogram(bins=50)+ggtitle("Distribution of P Values")
p3<-ggplot(training,aes(x=pH))+geom_histogram(bins=50)+ggtitle("Distribution of pH Values")
p4<-ggplot(training,aes(x=Sand))+geom_histogram(bins=50)+ggtitle("Distribution of Sand Values")
p5<-ggplot(training,aes(x=SOC))+geom_histogram(bins=50)+ggtitle("Distribution of SOC Values")
grid.arrange(p1,p2,p3,p4,p5,ncol=3,
             top="Distribution of Each Outcome Variable in the Training Set")

# We can see that the distribution of Ca, P, and Soc are unimodal and peak around 0 but are all skewed
# to the right.  The distributions of pH and Sand are centered around 0 and are bimodal and multimodal
# respectively.



# EDA with respect to the spatial autocorrelation issues (See report PDF for context)

# Histograms of Elevation
p6<-ggplot(training,aes(x=ELEV))+geom_histogram(bins=50)
p7<-ggplot(test,aes(x=ELEV))+geom_histogram(bins=50)

grid.arrange(p6,p7,ncol=2,
             top="Distribution of Elevation Values in the Training Set Versus the Test Set")

# It seems that most of the samples in the training data were collected from one of two elevation intervals,
# between -1.33 and 0,and between 0 and 2 (note that the elevation values have been sampled and scaled).
# However, we see in the case of the test data that while the distribution peak at approximately the same
# two intervals, the spread of each interval is different and there are hardly any outlier valaues that lie
# outside of those intervals in the test set as opposed to in the training data.  This suggests that the
# training data and the test data do seem to be representative of different "soil populations", which
# implies that my model will be overfit to the training data.

# Histograms of Precipitation
p8<-ggplot(training,aes(x=TMAP))+geom_histogram(bins=30)
p9<-ggplot(test,aes(x=TMAP))+geom_histogram(bins=30)

grid.arrange(p8,p9,ncol=2,
             top="Distribution of Mean Annual Precipitation Values in the Training Set Versus the Test Set")

# We can see that the distributions of the Mean Annual Precipitation differ significantly between the test
# dataset and the training dataset.  Whereas the histogram for the training data is almost a bell curve, the
# distribution for the test set looks almost uniform aside from a couple peaks.  Again, this confirms that
# there is a notable difference between the regions from which the training and test datasets were sampled.




# 4. Cross-Validation of Final Model --------------------------------------

# I commented out all the CV portions as they are incredibly time consuming to run and instead
# I added code that stores the final optimal lambda values for each variables as well as the
# final error values.

# Final CV for Ca
model_formula_Ca <- training %>%
  names() %>%
  setdiff(c("PIDN","Ca","P","pH","SOC","Sand")) %>%
  stringr::str_c(collapse=" + ") %>%
  stringr::str_c("Ca ~ ", .)
model_formula_Ca <- as.formula(model_formula_Ca)

# X <- model.matrix(model_formula_Ca, data = training)[, -5]
# y <- training$Ca
#
# lambda_values_Ca <- 10^seq(from = -6, to = -3, length=1000)
#
# cvfit_Ca <- cv.glmnet(X, y, alpha = 1,type.measure = "mse", lambda=lambda_values_Ca,nfolds=5)
#
# lambda_star_LASSO_Ca <- cvfit_Ca %>%
#   glance() %>%
#   .[["lambda.min"]]

lambda_star_LASSO_Ca <- 0.000807062

# Ca_index<-match(lambda_star_LASSO_Ca,lambda_values_Ca)
# MSE_Ca<-cvfit_Ca$cvm[Ca_index]

MSE_Ca <-  0.1489273

# Final CV for P
model_formula_P <- training %>%
  names() %>%
  setdiff(c("PIDN","Ca","P","pH","SOC","Sand")) %>%
  stringr::str_c(collapse=" + ") %>%
  stringr::str_c("P ~ ", .)
model_formula_P <- as.formula(model_formula_P)

# X <- model.matrix(model_formula_P, data = training)[, -5]
# y <- training$P
#
# lambda_values_P <- 10^seq(from = -6, to = -3, length=1000)
#
# cvfit_P <- cv.glmnet(X, y, alpha = 1,type.measure = "mse", lambda=lambda_values_P,nfolds=5)
#
# lambda_star_LASSO_P <- cvfit_P %>%
#   glance() %>%
#   .[["lambda.min"]]

lambda_star_LASSO_P <- 1.013925e-06

# P_index<-match(lambda_star_LASSO_P,lambda_values_P)
# MSE_P<-cvfit_P$cvm[P_index]
MSE_P <- 0.1405889

# Final CV for SOC
model_formula_SOC <- training %>%
  names() %>%
  setdiff(c("PIDN","Ca","P","pH","SOC","Sand")) %>%
  stringr::str_c(collapse=" + ") %>%
  stringr::str_c("SOC ~ ", .)
model_formula_SOC <- as.formula(model_formula_SOC)

# X <- model.matrix(model_formula_SOC, data = training)[, -5]
# y <- training$SOC
#
# lambda_values_SOC <- 10^seq(from = -7, to = -5, length=1000)
#
# cvfit_SOC <- cv.glmnet(X, y, alpha = 1,type.measure = "mse", lambda=lambda_values_SOC,nfolds=5)
#
# lambda_star_LASSO_SOC <- cvfit_SOC %>%
#   glance() %>%
#   .[["lambda.min"]]

lambda_star_LASSO_SOC <- 9.954008e-06

# SOC_index<-match(lambda_star_LASSO_SOC,lambda_values_SOC)
# MSE_SOC<-cvfit_SOC$cvm[SOC_index]

MSE_SOC <- 0.1542125

# Final CV for pH
model_formula_pH <- training %>%
  names() %>%
  setdiff(c("PIDN","Ca","P","pH","SOC","Sand")) %>%
  stringr::str_c(collapse=" + ") %>%
  stringr::str_c("pH ~ ", .)
model_formula_pH <- as.formula(model_formula_pH)

# X <- model.matrix(model_formula_pH, data = training)[, -5]
# y <- training$pH
#
# lambda_values_pH <- 10^seq(from = 0, to = -10, length=1000)
#
# cvfit_pH <- cv.glmnet(X, y, alpha = 1,type.measure = "mse", lambda=lambda_values_pH,nfolds=5)
#
# lambda_star_LASSO_pH <- cvfit_pH %>%
#   glance() %>%
#   .[["lambda.min"]]

lambda_star_LASSO_pH <- 5.293266e-07

# pH_index<-match(lambda_star_LASSO_pH,lambda_values_pH)
# MSE_pH<-cvfit_pH$cvm[pH_index]

MSE_pH <- 0.1105821


# Final CV for Sand
model_formula_Sand <- training %>%
  names() %>%
  setdiff(c("PIDN","Ca","P","pH","SOC","Sand")) %>%
  stringr::str_c(collapse=" + ") %>%
  stringr::str_c("Sand ~ ", .)
model_formula_Sand <- as.formula(model_formula_Sand)

# X <- model.matrix(model_formula_Sand, data = training)[, -5]
# y <- training$Sand
#
# lambda_values_Sand <- 10^seq(from = -4, to = -6, length=1000)
#
# cvfit_Sand <- cv.glmnet(X, y, alpha = 1,type.measure = "mse", lambda=lambda_values_Sand,nfolds=5)
#
# lambda_star_LASSO_Sand <- cvfit_Sand %>%
#   glance() %>%
#   .[["lambda.min"]]

lambda_star_LASSO_Sand <- 4.452959e-06

# Sand_index<-match(lambda_star_LASSO_Sand,lambda_values_Sand)
# MSE_Sand<-cvfit_Sand$cvm[Sand_index]

MSE_Sand <-  0.1235523


MSE_vector<-c(sqrt(MSE_Ca),sqrt(MSE_P),sqrt(MSE_pH),sqrt(MSE_Sand),sqrt(MSE_SOC))
MCRMSE<-mean(MSE_vector)

MCRMSE
# The CV estimated MCRMSE is 0.3675202, which is quite far off from the error generated by
# Kaggle probably due to the fact that the test dataset was sampled from different regions than
# the training dataset.  As discussed in the report, the training and test datasets are not
# representative of the same "soil populations", which means that my model will probably
# be overfit to the training data.



# 5. Create Submission ----------------------------------------------------
# This section of code also takes a while to run but is more feasible than the commented sections above

fake_test<-test %>% mutate(Ca=1,pH=1,P=1,Sand=1,SOC=1) %>%
  mutate(int_depth=ifelse(Depth=="SubSoil",1,0)) %>%
  select(-Depth)

# Final Ca Model & Predictions
X <- model.matrix(model_formula_Ca, data = training)[, -5]
y <- training$Ca

X_test <- model.matrix(model_formula_Ca, data=fake_test)[, -5]
optimal_model_LASSO_Ca<-glmnet(X, y, alpha = 1, lambda = lambda_star_LASSO_Ca)
LASSO_predictions_Ca <- optimal_model_LASSO_Ca %>% predict(newx=X_test)


# Final pH Model & Predictions
X <- model.matrix(model_formula_pH, data = training)[, -5]
y <- training$pH

X_test <- model.matrix(model_formula_pH, data=fake_test)[, -5]
optimal_model_LASSO_pH<-glmnet(X, y, alpha = 1, lambda = lambda_star_LASSO_pH)
LASSO_predictions_pH <- optimal_model_LASSO_pH %>% predict(newx=X_test)


# Final P Model & Predictions
X <- model.matrix(model_formula_P, data = training)[, -5]
y <- training$P

X_test <- model.matrix(model_formula_P, data=fake_test)[, -5]
optimal_model_LASSO_P<-glmnet(X, y, alpha = 1, lambda = lambda_star_LASSO_P)
LASSO_predictions_P <- optimal_model_LASSO_P %>% predict(newx=X_test)


# Final Sand Model & Predictions
X <- model.matrix(model_formula_Sand, data = training)[, -5]
y <- training$Sand

X_test <- model.matrix(model_formula_Sand, data=fake_test)[, -5]
optimal_model_LASSO_Sand<-glmnet(X, y, alpha = 1, lambda = lambda_star_LASSO_Sand)
LASSO_predictions_Sand <- optimal_model_LASSO_Sand %>% predict(newx=X_test)


# Final SOC Model & Predictions
X <- model.matrix(model_formula_SOC, data = training)[, -5]
y <- training$SOC

X_test <- model.matrix(model_formula_SOC, data=fake_test)[, -5]
optimal_model_LASSO_SOC<-glmnet(X, y, alpha = 1, lambda = lambda_star_LASSO_SOC)
LASSO_predictions_SOC <- optimal_model_LASSO_SOC %>% predict(newx=X_test)


# Binding all the predictions together (You still have to go in and add the right
# variable names in the CSV file before Kaggle Submission)
submission_LASSO2<-cbind(as.character(test$PIDN),LASSO_predictions_Ca,LASSO_predictions_P,
                         LASSO_predictions_pH,LASSO_predictions_Sand,LASSO_predictions_SOC)

submission_LASSO2 %>%
  write.csv("Files/Soil_Submission_LASSO2.csv")

# Submitting the above submission.csv file to Kaggle, we get a score of 0.62444, which is
# not very close to the estimated score above but again, that's due to the issues described
# in the report.




# 6. Extras ---------------------------------------------------------------

# ANALYSIS OF OPTIMAL MODELS

# Tidy tables of the optimal models
Ca_model_table<-optimal_model_LASSO_Ca %>%
  tidy() %>%
  as.data.frame() %>%
  mutate(abs_estimate=abs(estimate))

P_model_table<-optimal_model_LASSO_P %>%
  tidy() %>%
  as.data.frame() %>%
  mutate(abs_estimate=abs(estimate))

pH_model_table<-optimal_model_LASSO_pH %>%
  tidy() %>%
  as.data.frame() %>%
  mutate(abs_estimate=abs(estimate))

Sand_model_table<-optimal_model_LASSO_Sand %>%
  tidy() %>%
  as.data.frame() %>%
  mutate(abs_estimate=abs(estimate))

SOC_model_table<-optimal_model_LASSO_SOC %>%
  tidy() %>%
  as.data.frame() %>%
  mutate(abs_estimate=abs(estimate)) %>%
  rename(SOC_Coeff=term)


# Examining the lambdas used in the optimal models:
optimal_lambdas<-c(lambda_star_LASSO_Ca,lambda_star_LASSO_P,lambda_star_LASSO_pH,
                   lambda_star_LASSO_Sand,lambda_star_LASSO_SOC)
optimal_lambdas
# All the lambda values are quite small, which means not a lot of shrinkage occured


# Examining how many coefficients were reduced to zero:
n_Shrunk_Ca<-nrow(Ca_model_table %>% filter(abs_estimate==0))
n_Shrunk_P<-nrow(P_model_table %>% filter(abs_estimate==0))
n_Shrunk_pH<-nrow(pH_model_table %>% filter(abs_estimate==0))
n_Shrunk_Sand<-nrow(Sand_model_table %>% filter(abs_estimate==0))
n_Shrunk_SOC<-nrow(SOC_model_table %>% filter(abs_estimate==0))

n_Shrunk_all<-c(n_Shrunk_Ca,n_Shrunk_P,n_Shrunk_pH,n_Shrunk_Sand,n_Shrunk_SOC)


# Table of lambda versus number of variables shrunk to zero for each optimal model
grid.newpage()
grid.arrange(tableGrob(cbind(optimal_lambdas,n_Shrunk_all)),
             top="Lambda Values and Number of Predictor Variables Shrunk to 0 for
             the Optimal Ca, P, pH, Sand, and SOC Models")

# We can see that the larger the lambda, the more predictor variables are shrunk to 0,
# which obviously makes sense.  I did find it interesting, however, that setting lambda
# to a mere 0.0008 shrinks 3304 variables to 0, leaving only 276 variables left in the model.


# Tables of the variables with the most predictive power of each optimal model

mini_Ca_table<-Ca_model_table %>%
  arrange(desc(abs_estimate)) %>%
  select(term, abs_estimate) %>%
  rename(Ca_Coeff=term) %>%
  head(10)

mini_P_table<-P_model_table %>%
  arrange(desc(abs_estimate)) %>%
  select(term, abs_estimate) %>%
  rename(P_Coeff=term) %>%
  head(10)

mini_pH_table<-pH_model_table %>%
  arrange(desc(abs_estimate)) %>%
  select(term, abs_estimate) %>%
  rename(pH_Coeff=term) %>%
  head(10)

mini_Sand_table<-Sand_model_table %>%
  arrange(desc(abs_estimate)) %>%
  select(term, abs_estimate) %>%
  rename(Sand_Coeff=term) %>%
  head(10)

mini_SOC_table<-SOC_model_table %>%
  arrange(desc(abs_estimate)) %>%
  select(SOC_Coeff, abs_estimate) %>%
  head(10)

grid.newpage()
grid.arrange(tableGrob(mini_Ca_table),tableGrob(mini_P_table),tableGrob(mini_pH_table),
             tableGrob(mini_Sand_table),tableGrob(mini_SOC_table),ncol=3,
             top="Most Predictive Variables of Each Outcome Variable")


# We can see that there are only a few variables that are among the top ten most predictive
# variables for more than one model.  For example, m1787.71 is a highly predictive variable for both
# the Ca model as well as the Sand and SOC models, but most variables are only a strong predictor for one
# of the outcome variables.
