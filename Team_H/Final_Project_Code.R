library(readr)
library(tidyverse)
library(lubridate)
library(broom)
library(plotly)
library(glmnet)
library(stringr)
library(pls)

#DATA CLEANING for test 
test <- read_csv("Files/test.csv") %>% 
  # Variable names in R shouldn't have spaces, so rename:
  dplyr::rename(
    open_date = `Open Date`,
    city_group = `City Group`
  ) %>% 
  ## makes the date recognized by R using lubridate package 
  mutate(open_date=mdy(open_date)) %>% 
  ##recode the City names and Types because what is present in the train dataset doesnt encapsulate all that
  ##is in the test data set
  mutate(City=
           if_else(City=="Istanbul","Istanbul", 
                   if_else(City=="Ankara","Ankara", 
                           if_else(City=="Izmir","Izmir", "Other")))) %>% 
  mutate(City=as.factor(City)) %>% 
  mutate(Type=
           if_else(Type=="FC", "FC", "Other")) %>% 
  mutate(Type=as.factor(Type)) %>% 
  mutate(city_group=as.factor(city_group))

# first round of DATA CLEANING for train 
train <- read_csv("Files/train.csv") %>% 
  # Variable names in R shouldn't have spaces, so rename:
  dplyr::rename(
    open_date = `Open Date`,
    city_group = `City Group`
  ) %>% 
  ## makes the date recognized by R using lubridate package 
  mutate(open_date=mdy(open_date))

#***EDA*** 

##Plot of opening date vs revenue 
ggplot(data=train, aes(x=open_date, y=revenue))+
  geom_point() +
  stat_smooth(method=lm) +
  labs(x="Restaurant Opening Date", y="Revenue")

##exploring revenue by city
train_city <- train %>% 
  group_by(City,city_group) %>% 
  dplyr::summarize(avg_revenue=mean(revenue))  

##Plot of revenue by city 
ggplot(data=train_city, aes(x=reorder(City, avg_revenue), y=avg_revenue, fill=city_group)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(y="Average Revenue", x="City Name", title="Average Revenue by City") +
  scale_fill_discrete(name="City Group")

##exploring revenue by city group 
train_group <- train %>% 
  group_by(city_group) %>% 
  summarize(avg_revenue=mean(revenue))

##Plot of revenue by city 
ggplot(data=train_group, aes(x=reorder(city_group, avg_revenue), y=avg_revenue)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="City Group", y="Average Revenue", title="Average Revenue by City Group")

##exploring revenue by type 
train_type<- train %>% 
  group_by(Type) %>% 
  summarize(avg_revenue=mean(revenue))
##Plot of revenue by city 
ggplot(data=train_type, aes(x=reorder(Type, avg_revenue), y=avg_revenue, fill=Type)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Restaurant Type", y="Average Revenue", title="Average Revenue by Restaurant Type") +
  scale_fill_discrete(labels=c("Drive Thru", "Food Court", "In Line"))


#second round of DATA CLEANING for train 
train <- train %>%  
  ##recode the City names and Types because what is present in the train dataset doesnt encapsulate all that
  ##is in the test data set 
  mutate(City=
           if_else(City=="Istanbul","Istanbul", 
           if_else(City=="Ankara","Ankara", 
           if_else(City=="Izmir","Izmir", "Other")))) %>% 
  mutate(City=as.factor(City)) %>% 
  mutate(Type=
         if_else(Type=="FC", "FC", "Other")) %>% 
  mutate(Type=as.factor(Type)) %>% 
  mutate(city_group=as.factor(city_group))

## Cross Validation of FINAL MODEL
train <- train %>% 
  # This is where we select what dimensions will be included in our model 
  select(
    Id, open_date, City, city_group, Type, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, 
    P19, P20, P21, P22, P23, P24, P25, P26, P27, P28, P29, P30, P31, P32, P33, P34, 
    P35, P36, P37, revenue
  )

# Create the model formula for lm(), ridge regression, LASSO, and elastic net:
model_formula <- train %>% 
  # Take all predictor variable names and separate them with + signs:
  names() %>% 
  setdiff(c("Id", "fold", "revenue")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("revenue ~ ", .)
model_formula
model_formula <- as.formula(model_formula)

# Create the model formula for PCR
model_formula_pcr <- train %>% 
  # Take all predictor variable names and separate them with + signs:
  names() %>% 
  setdiff(c("Id", "fold", "revenue", "city_group", "City", "Type")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("revenue ~ ", .)
model_formula_pcr
model_formula_pcr <- as.formula(model_formula_pcr)


# Used in glmnet() function for ridge regression, LASSO, and elastic net
X <- model.matrix(model_formula, data = train)[, -1]
y <- train$revenue

# Number of folds to use for all CV
n_folds <- 10

# 1. Obtain optimal lambda for ridge regression --------------------------------

# Feel free to toy with the from, to, and length values:
lambda_values <- 10^seq(from = 6, to = 10, length = 2500)
# Using built-in function, we don't need to use our own for-loops. Note
# alpha=0 for ridge regression
cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = n_folds)
cv_ridge_tidy <- cv_ridge %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda)) 


# Plot cross-validated MSE value for each lambda:
ggplot(cv_ridge_tidy, aes(x=log_lambda, y=estimate)) +
  geom_point() +
  labs(x="log(lambda)", y="Cross-Validated MSE", title="Ridge Regression Cross-Validation")

# Extract optimal lambda using broom::glance()
lambda_star_ridge <- cv_ridge %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_ridge
# cv_ridge_tidy <- cv_ridge %>%
#   tidy() %>%
#   tbl_df() %>%
#   mutate(log_lambda=log(lambda)) 
# 
# ggplot(cv_ridge_tidy, aes(x=log_lambda, y=estimate)) +
#   geom_point() +
#   labs(x="log(lambda)", y="Cross-Validated MSE", title="Ridge Regression Cross-Validation") + 
#   geom_vline(xintercept = log(lambda_star_ridge), col="red")


# Compute coefficients for each value of lambda
lambda_values <- 10^seq(from = 0.1, to = 9, length = 2500)
model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_values)
coefficients <- model_ridge %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)")

# Plot coefficients for each value of lambda, with optimal lambda marked
ridge_coefficients <- ggplot(coefficients, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_ridge)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="Ridge Regression Coefficients")

# to converge to 0 at the same time:
# ggplotly(ridge_coefficients)


# 2. Obtain optimal lambda for LASSO -------------------------------------------

lambda_values <- 10^seq(from = 3.5, to = 7, length = 2500)
cv_LASSO <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = n_folds)
# cv_LASSO_tidy <- cv_LASSO %>%
#   tidy() %>%
#   tbl_df() %>%
#   mutate(log_lambda=log(lambda)) 
# 
# 
# # Plot cross-validated MSE value for each lambda:
# ggplot(cv_LASSO_tidy, aes(x=log_lambda, y=estimate)) +
#   geom_point() +
#   labs(x="log(lambda)", y="Cross-Validated MSE", title="LASSO Cross-Validation")

# Extract optimal lambda using broom::glance()
lambda_star_LASSO <- cv_LASSO %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_LASSO

# # Compute coefficients for each value of lambda
lambda_values <- 10^seq(from = 1, to = 9, length = 5000)
model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_values)
coefficients <- model_LASSO %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)")

# Plot coefficients for each value of lambda, with optimal lambda marked
LASSO_coefficients <- ggplot(coefficients, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_LASSO)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="LASSO Coefficients")

# Looking at the interactive graph
# ggplotly(LASSO_coefficients)


# 3. Obtain optimal lambda for Elastic Net Reg  -------------------------------------------

lambda_values <- 10^seq(from = 3.5, to = 7, length = 2500)
cv_elastic <- cv.glmnet(X, y, alpha = 0.5, lambda=lambda_values, nfolds = n_folds)
# cv_elastic_tidy <- cv_elastic %>%
#   tidy() %>%
#   tbl_df() %>%
#   mutate(log_lambda=log(lambda)) 
# 
# 
# # Plot cross-validated MSE value for each lambda:
# ggplot(cv_elastic_tidy, aes(x=log_lambda, y=estimate)) +
#   geom_point() +
#   labs(x="log(lambda)", y="Cross-Validated MSE", title="Elastic Net Cross-Validation")


# Extract optimal lambda using broom::glance()
lambda_star_elastic <- cv_elastic %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_elastic

# # Compute coefficients for each value of lambda
lambda_values <- 10^seq(from = 1, to = 9, length = 5000)
model_elastic <- glmnet(X, y, alpha = 0.5, lambda = lambda_values)
coefficients <- model_elastic %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)")

# Plot coefficients for each value of lambda, with optimal lambda marked
elastic_coefficients <- ggplot(coefficients, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_elastic)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="Elastic Coefficients")

# Looking at the interactive graph
# ggplotly(elastic_coefficients)


#4. Obtain optimal components for PCA ---------------------------------
set.seed(2)
#run pca regressions for all components
pcr.fit=pcr(model_formula_pcr, data=train, scale=TRUE, validation="CV")

#get their MSEs
pcr_mseps <- data.frame(matrix(MSEP(pcr.fit, c("CV"))$val))
colnames(pcr_mseps) <- c("CV")
pcr_mseps <- pcr_mseps %>%
  mutate(
    id = as.integer(rownames(pcr_mseps))
  )

#Extract min MSE
min_pcr_mse <- pcr_mseps %>%
  arrange(CV) %>%
  slice(1)

#plot components and MSE
# validationplot(pcr.fit, val.type = "MSEP") +
#   abline(v=min_pcr_mse$id)


# 5. Cross-validation comparison of lm(), ridge, LASSO, elastic net, and PCR ---------------------

# Run simulations to determine which is best:
nSims <- 200
simulation_results_rank <- data_frame(
  lm=rep(0, nSims),
  ridge=rep(0, nSims),
  LASSO=rep(0, nSims),
  elastic=rep(0, nSims),
  PCR=rep(0, nSims)
)
simulation_results_score <- data_frame(
  lm=rep(0, nSims),
  ridge=rep(0, nSims),
  LASSO=rep(0, nSims),
  elastic=rep(0, nSims),
  PCR=rep(0, nSims)
)

for(j in 1:nSims){
  # Create folds
  train <- train %>%
    sample_frac(1) %>%
    mutate(fold = rep(1:n_folds, length=n())) %>%
    arrange(fold)
  
  # For all n_folds folds, save 5 MSE's from the 5 methods here. For each fold
  # we will append/bind the results for each fold.
  results_all_folds <- NULL
  
  for(i in 1:n_folds){
    # 1. Create pseudo train/test based on folding scheme
    pseudo_train <- train %>%
      filter(fold != i)
    pseudo_test <- train %>%
      filter(fold == i)
    # Needed for glmnet() methods:
    pseudo_train_X <- model.matrix(model_formula, data = pseudo_train)[, -1]
    pseudo_train_y <- pseudo_train$revenue
    pseudo_test_X <- model.matrix(model_formula, data = pseudo_test)[, -1]
    
    # 2. Fit model to pseudo-train
    model_lm <- lm(model_formula, data=pseudo_train)
    model_ridge <- glmnet(pseudo_train_X, pseudo_train_y, alpha = 0, lambda = lambda_star_ridge)
    model_LASSO <- glmnet(pseudo_train_X, pseudo_train_y, alpha = 1, lambda = lambda_star_LASSO)
    model_elastic <- glmnet(pseudo_train_X, pseudo_train_y, alpha = 0.5, lambda = lambda_star_elastic)
    model_PCR <- pcr(model_formula_pcr, data=pseudo_train, scale=TRUE, validation="CV")
    
    # 3. Get predictions for all three methods.
    lm_predictions <- model_lm %>%
      predict(newdata=pseudo_test)
    ridge_predictions <- model_ridge %>% 
      predict(newx=pseudo_test_X, s=lambda_star_ridge)
    LASSO_predictions <- model_LASSO %>% 
      predict(newx=pseudo_test_X, s=lambda_star_LASSO)
    elastic_predictions <- model_elastic %>%
      predict(newx=pseudo_test_X, s=lambda_star_elastic)
    PCR_predictions <- model_PCR %>%
      predict(pseudo_test, ncomp=min_pcr_mse$id)
    
    
    # 4. Compute MSE for each method and save them (by appending/binding to
    # results_all_folds)
    results_all_folds <- 
      # Create data frame of y=revenue and predictions from all 5 methods
      data_frame(
        revenue = pseudo_test$revenue,
        lm = as.vector(lm_predictions),
        ridge = as.vector(ridge_predictions),
        LASSO = as.vector(LASSO_predictions),
        elastic = as.vector(elastic_predictions),
        PCR = as.vector(PCR_predictions)
      ) %>% 
      # Switch to tidy format so we can use group_by() to compute MSE's for all
      # 5 methods easily
      tidyr::gather(method, yhat, -revenue) %>% 
      group_by(method) %>% 
      summarise(RMSE = sqrt(mean((revenue-yhat)^2))) %>% 
      # Include fold i and append/bind to results_all_folds
      mutate(fold=i) %>% 
      bind_rows(results_all_folds)
  }
  # Show results
  result_summary <- results_all_folds %>%
    group_by(method) %>%
    summarise(RMSE_CV = mean(RMSE)) %>%
    arrange(RMSE_CV)
  
  #compute the results of the simulation
  #rankings
  for(reg_method in result_summary$method){
    simulation_results_rank[j,reg_method] <- as.integer(which(result_summary$method == reg_method))
  }
  #MSE scores
  for(reg_method in result_summary$method){
    simulation_results_score[j,reg_method] <- as.vector(result_summary %>%
                                                          filter(method==reg_method))$RMSE_CV
  }
}

#clean data for plotting avg simulation scores
simulation_results_score_tidy <- gather(simulation_results_score, "Method", "Score", 1:5 ) %>% 
  group_by(Method) %>% 
  summarize(avg_score=mean(Score))

#Avg score plots:
#with lm
ggplot(data=simulation_results_score_tidy, aes(x=Method, y=avg_score)) + geom_col()
#w/o lm
ggplot(data=simulation_results_score_tidy %>% filter(Method != 'lm'), aes(x=Method, y=avg_score)) + 
  geom_col() +
  labs(y="Average Score",title="Average RMSE Scores Across Simulations without Lm")


#clean data for plotting scores in each simulation
simulation_results_score_tidy2 <- simulation_results_score %>%
  mutate(simNum = c(1:nSims)) %>% 
  tidyr::gather(method, RMSE, -simNum) %>%
  group_by(method)


#line graph across simulations:
#with lm
ggplot(simulation_results_score_tidy2, aes(x=simNum, y=RMSE, col=method)) + geom_point() + geom_line()

#without lm
ggplot(simulation_results_score_tidy2 %>% filter(method != 'lm'), aes(x=simNum, y=RMSE, col=method)) +
  geom_point() + 
  geom_line() +
  labs(x="Number of Simulations", y="Average RMSE across the 10 CV folds", title="Simulation RMSE Results without Lm") +
  theme(text = element_text(size=20))

# Data cleaning to prep for plot below
ranking_lm <- as.data.frame(table(factor(simulation_results_rank$lm, levels = c(1:5))))
ranking_ridge <- as.data.frame(table(factor(simulation_results_rank$ridge, levels = c(1:5))))
ranking_LASSO <- as.data.frame(table(factor(simulation_results_rank$LASSO, levels = c(1:5))))
ranking_elastic <- as.data.frame(table(factor(simulation_results_rank$elastic, levels = c(1:5))))
ranking_PCR <- as.data.frame(table(factor(simulation_results_rank$PCR, levels = c(1:5))))

#Count of simulation rankings of each method 
lm_bar <- ggplot(data=ranking_lm, aes(x=Var1, y=Freq)) + geom_col() + labs(x="Rank", y="Frequency",title="Linear Model") + coord_cartesian(ylim=c(0,nSims))
ridge_bar <-ggplot(data=ranking_ridge, aes(x=Var1, y=Freq)) + geom_col() + labs(x="Rank", y="Frequency",title="Ridge Model") + coord_cartesian(ylim=c(0,nSims))
LASSO_bar <- ggplot(data=ranking_LASSO, aes(x=Var1, y=Freq)) + geom_col() + labs(x="Rank", y="Frequency",title="LASSO Model") + coord_cartesian(ylim=c(0,nSims))
elastic_bar <- ggplot(data=ranking_elastic, aes(x=Var1, y=Freq)) + geom_col() + labs(x="Rank", y="Frequency",title="Elastic Model") + coord_cartesian(ylim=c(0,nSims))
PCR_bar <- ggplot(data=ranking_PCR, aes(x=Var1, y=Freq)) + geom_col() + labs(x="Rank", y="Frequency",title="PCR Model") + coord_cartesian(ylim=c(0,nSims))

#Displayed together
grid.arrange(lm_bar, ridge_bar, LASSO_bar, elastic_bar, PCR_bar, ncol=2, top="Method Ranking Comparison")

# 6. Submit Predictions to Kaggle ----------------------------------------------
sample_submission <- read_csv("Files/samplesubmission.csv")
test <- test %>%
  # Add a "dummy" SalePrice outcome so that the model.matrix() commmand below
  # will still work
  mutate(revenue=1)

# In case our case the optimal method was ridge regression so we will need this 
# for glmnet()
test_X <- model.matrix(model_formula, data = test)[, -1]

# Get predictions using what you think was the best method:
# Ridge regression, Elastic Net, LASSO, and PCR both all have similar average scores 

#RIDGE
predictions <- model_ridge %>% 
  predict(newx=test_X, s=lambda_star_ridge) %>% 
  as.vector()

# Write ridge submissions to CSV
ridge_submission <- sample_submission %>% 
  mutate(Prediction = as.vector(predictions)) %>% 
  write_csv("Ridge_Submission.csv")

#ELASTIC
predictions <- model_elastic%>%
  predict(newx=test_X, s=lambda_star_elastic) %>% 
  as.vector()

# Write elastic submissions to CSV
elastic_submission <- sample_submission %>% 
  mutate(Prediction = as.vector(predictions)) %>% 
  write_csv("Elastic_Submission.csv")

#LASSO
predictions <- model_LASSO %>%
  predict(newx=test_X, s=lambda_star_LASSO) %>% 
  as.vector()

# Write elastic submissions to CSV
LASSO_submission <- sample_submission %>% 
  mutate(Prediction = as.vector(predictions)) %>% 
  write_csv("LASSO_Submission.csv")


#PCR-FINAL MODEL
predictions <- model_PCR %>%
  predict(test, ncomp=min_pcr_mse$id) %>%
  as.vector()

# Write submissions to CSV
PCR_submission <- sample_submission %>% 
  mutate(Prediction = as.vector(predictions)) %>% 
  write_csv("PCR_Submission.csv")

## The PCR submission file generates the lowest RMSE score on kaggle 
## despite the ranking graphic as seen above. so treat PCR_submission.csv 
## as our final submission. 



##Everything below is EXTRA
## We tried to manually do PCA and then used those new principal components as new 
## predictor variables that then replaced P1-P37 and then cross validated to determine 
## whether LASSO, Ridge, or LM was best given this new dataset comprised of the PCs. 

##trying PCA from Prof. Kim's code from class
X <- train %>% 
  select(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, 
         P19, P20, P21, P22, P23, P24, P25, P26, P27, P28, P29, P30, P31, P32, P33, P34, 
         P35, P36, P37)
X_recenter <- X %>% 
  mutate(
    P1 = P1 - mean(P1),
    P2 = P2 - mean(P2),
    P3 = P3 - mean(P3),
    P4 = P4 - mean(P4),
    P5 = P5 - mean(P5),
    P6 = P6 - mean(P6),
    P7 = P7 - mean(P7),
    P8 = P8 - mean(P8),
    P9 = P9 - mean(P9),
    P10 = P10 - mean(P10),
    P11 = P11 - mean(P11),
    P12 = P12 - mean(P12),
    P13 = P13 - mean(P13),
    P14 = P14 - mean(P14),
    P15 = P15 - mean(P15),
    P16 = P16 - mean(P16),
    P17 = P17 - mean(P17),
    P18 = P18 - mean(P18),
    P19 = P19 - mean(P19),
    P20 = P20 - mean(P20),
    P21 = P21 - mean(P21),
    P22 = P22 - mean(P22),
    P23 = P23 - mean(P23),
    P24 = P24 - mean(P24),
    P25 = P25 - mean(P25),
    P26 = P26 - mean(P26),
    P27 = P27 - mean(P27),
    P28 = P28 - mean(P28),
    P29 = P29 - mean(P29),
    P30 = P30 - mean(P30),
    P31 = P31 - mean(P31),
    P32 = P32 - mean(P32),
    P33 = P33 - mean(P33),
    P34 = P34 - mean(P34),
    P35 = P35 - mean(P35),
    P36 = P36 - mean(P36),
    P37 = P37 - mean(P37)
  )

eigen <- cov(X_recenter) %>% eigen()
eigen_vals <- eigen$values
Gamma <- eigen$vectors

Y <- as.matrix(X_recenter) %*% Gamma %>% 
  as_data_frame() %>% 
  rename(Y1 = V1, Y2 = V2, Y3 = V3, Y4 = V4, Y5 = V5, Y6 = V6, Y7 = V7, Y8 = V8, Y9 = V9, Y10 = V10, Y11 = V11, Y12 = V12, 
         Y13 = V13, Y14 = V14, Y15 = V15, Y16 = V16, Y17 = V17, Y18 = V18, Y19 = V19, Y20 = V20, Y21 = V21, Y22 = V22, Y23 = V23, 
         Y24 = V24, Y25 = V25, Y26 = V26, Y27 = V27, Y28 = V28, Y29 = V29, Y30 = V30, Y31 = V31, Y32 = V32, Y33 = V33, Y34 = V34,
         Y35 = V35, Y36 = V36, Y37 = V37)

cov(Y) %>% round(3)

var_Y <- cov(Y) %>% diag()
var_Y

cumsum(var_Y)/sum(var_Y)

train_PCA <- bind_cols(train, Y) %>% 
  select(Id, open_date, City, city_group, Type, Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, revenue)

# Create the model formula for lm(), ridge regression, LASSO, and elastic net:
model_formula <- train_PCA %>% 
  # Take all predictor variable names and separate them with + signs:
  names() %>% 
  setdiff(c("Id", "fold", "revenue")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("revenue ~ ", .)
model_formula
model_formula <- as.formula(model_formula)

# Used in glmnet() function for ridge regression, LASSO, and elastic net
X <- model.matrix(model_formula, data = train_PCA)[, -1]
y <- train_PCA$revenue

# Number of folds to use for all CV
n_folds <- 10

# 1. Obtain optimal lambda for Ridge regression --------------------------------

# Feel free to toy with the from, to, and length values:
lambda_values <- 10^seq(from = 6, to = 10, length = 2500)
# Using built-in function, we don't need to use our own for-loops. Note
# alpha=0 for ridge regression
cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = n_folds)
cv_ridge_tidy <- cv_ridge %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda)) 


# Plot cross-validated MSE value for each lambda:
ggplot(cv_ridge_tidy, aes(x=log_lambda, y=estimate)) +
  geom_point() +
  labs(x="log(lambda)", y="Cross-Validated MSE", title="Ridge Regression Cross-Validation")

# Extract optimal lambda using broom::glance()
lambda_star_ridge <- cv_ridge %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_ridge
cv_ridge_tidy <- cv_ridge %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda)) 

ggplot(cv_ridge_tidy, aes(x=log_lambda, y=estimate)) +
  geom_point() +
  labs(x="log(lambda)", y="Cross-Validated MSE", title="Ridge Regression Cross-Validation") + 
  geom_vline(xintercept = log(lambda_star_ridge), col="red")

# Compute coefficients for each value of lambda
lambda_values <- 10^seq(from = 0.1, to = 9, length = 2500)
model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_values)
coefficients <- model_ridge %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)")

# Plot coefficients for each value of lambda, with optimal lambda marked
ridge_coefficients <- ggplot(coefficients, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_ridge)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="Ridge Regression Coefficients")

# to converge to 0 at the same time:
ggplotly(ridge_coefficients)


# 2. Obtain optimal lambda for LASSO -------------------------------------------

lambda_values <- 10^seq(from = 3.5, to = 7, length = 2500)
cv_LASSO <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = n_folds)
cv_LASSO_tidy <- cv_LASSO %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda)) 


# Plot cross-validated MSE value for each lambda:
ggplot(cv_LASSO_tidy, aes(x=log_lambda, y=estimate)) +
  geom_point() +
  labs(x="log(lambda)", y="Cross-Validated MSE", title="LASSO Cross-Validation")

# Extract optimal lambda using broom::glance()
lambda_star_LASSO <- cv_LASSO %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_LASSO

# # Compute coefficients for each value of lambda
lambda_values <- 10^seq(from = 1, to = 9, length = 5000)
model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_values)
coefficients <- model_LASSO %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)")

# Plot coefficients for each value of lambda, with optimal lambda marked
LASSO_coefficients <- ggplot(coefficients, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_LASSO)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="LASSO Coefficients")

# Looking at the interactive graph
ggplotly(LASSO_coefficients)

# 3. Cross-validation comparison of lm(), ridge, and LASSO ---------------------
# Create folds
train_PCA <- train_PCA %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

# For all n_folds folds, save 3 MSE's from the 3 methods here. For each fold
# we will append/bind the results for each fold.
results_all_folds <- NULL

for(i in 1:n_folds){
  # 1. Create pseudo train/test based on folding scheme
  pseudo_train <- train_PCA %>%
    filter(fold != i)
  pseudo_test <- train_PCA %>%
    filter(fold == i)
  # Needed for glmnet() methods:
  pseudo_train_X <- model.matrix(model_formula, data = pseudo_train)[, -1]
  pseudo_train_y <- pseudo_train$revenue
  pseudo_test_X <- model.matrix(model_formula, data = pseudo_test)[, -1]
  
  # 2. Fit model to pseudo-train
  model_lm <- lm(model_formula, data=pseudo_train)
  model_ridge <- glmnet(pseudo_train_X, pseudo_train_y, alpha = 0, lambda = lambda_star_ridge)
  model_LASSO <- glmnet(pseudo_train_X, pseudo_train_y, alpha = 1, lambda = lambda_star_LASSO)
  
  # 3. Get predictions for all three methods.
  lm_predictions <- model_lm %>% 
    predict(newdata=pseudo_test)
  ridge_predictions <- model_ridge %>% 
    predict(newx=pseudo_test_X, s=lambda_star_ridge)
  LASSO_predictions <- model_LASSO %>% 
    predict(newx=pseudo_test_X, s=lambda_star_LASSO)
  
  # 4. Compute MSE for each method and save them (by appending/binding to
  # results_all_folds)
  results_all_folds <- 
    # Create data frame of y=revenue and predictions from all three methods
    data_frame(
      revenue = pseudo_test$revenue,
      lm = as.vector(lm_predictions),
      ridge = as.vector(ridge_predictions),
      LASSO = as.vector(LASSO_predictions)
    ) %>% 
    # Switch to tidy format so we can use group_by() to compute MSE's for all
    # three methods easily
    tidyr::gather(method, yhat, -revenue) %>% 
    group_by(method) %>% 
    summarise(MSE = mean((revenue-yhat)^2)) %>% 
    # Include fold i and append/bind to results_all_folds
    mutate(fold=i) %>% 
    bind_rows(results_all_folds)
}

# Show results
results_all_folds %>% 
  group_by(method) %>% 
  summarise(MSE_CV = mean(MSE)) %>% 
  arrange(desc(MSE_CV))

# Run the CV comparision a few times manually (in order to account for sampling
# variability) and get a rough ranking of which method does best. You will use
# this method below

##we see that LASSO is always ranked first when we repeat this a bunch of times 

# And now Submit Predictions to Kaggle ----------------------------------------------
sample_submission <- read_csv("Files/samplesubmission.csv")
test_PCA <- test %>%
  # Add a "dummy" revenue outcome so that the model.matrix() commmand below
  # will still work
  mutate(revenue=1) 

##need to restructure test data bc we did PCA
X <- test_PCA %>% 
  select(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, 
         P19, P20, P21, P22, P23, P24, P25, P26, P27, P28, P29, P30, P31, P32, P33, P34, 
         P35, P36, P37)
X_recenter <- X %>% 
  mutate(
    P1 = P1 - mean(P1),
    P2 = P2 - mean(P2),
    P3 = P3 - mean(P3),
    P4 = P4 - mean(P4),
    P5 = P5 - mean(P5),
    P6 = P6 - mean(P6),
    P7 = P7 - mean(P7),
    P8 = P8 - mean(P8),
    P9 = P9 - mean(P9),
    P10 = P10 - mean(P10),
    P11 = P11 - mean(P11),
    P12 = P12 - mean(P12),
    P13 = P13 - mean(P13),
    P14 = P14 - mean(P14),
    P15 = P15 - mean(P15),
    P16 = P16 - mean(P16),
    P17 = P17 - mean(P17),
    P18 = P18 - mean(P18),
    P19 = P19 - mean(P19),
    P20 = P20 - mean(P20),
    P21 = P21 - mean(P21),
    P22 = P22 - mean(P22),
    P23 = P23 - mean(P23),
    P24 = P24 - mean(P24),
    P25 = P25 - mean(P25),
    P26 = P26 - mean(P26),
    P27 = P27 - mean(P27),
    P28 = P28 - mean(P28),
    P29 = P29 - mean(P29),
    P30 = P30 - mean(P30),
    P31 = P31 - mean(P31),
    P32 = P32 - mean(P32),
    P33 = P33 - mean(P33),
    P34 = P34 - mean(P34),
    P35 = P35 - mean(P35),
    P36 = P36 - mean(P36),
    P37 = P37 - mean(P37)
  )

eigen <- cov(X_recenter) %>% eigen()
eigen_vals <- eigen$values
Gamma <- eigen$vectors

Y <- as.matrix(X_recenter) %*% Gamma %>% 
  as_data_frame() %>% 
  rename(Y1 = V1, Y2 = V2, Y3 = V3, Y4 = V4, Y5 = V5, Y6 = V6, Y7 = V7, Y8 = V8, Y9 = V9, Y10 = V10, Y11 = V11, Y12 = V12, 
         Y13 = V13, Y14 = V14, Y15 = V15, Y16 = V16, Y17 = V17, Y18 = V18, Y19 = V19, Y20 = V20, Y21 = V21, Y22 = V22, Y23 = V23, 
         Y24 = V24, Y25 = V25, Y26 = V26, Y27 = V27, Y28 = V28, Y29 = V29, Y30 = V30, Y31 = V31, Y32 = V32, Y33 = V33, Y34 = V34,
         Y35 = V35, Y36 = V36, Y37 = V37)

cov(Y) %>% round(3)

var_Y <- cov(Y) %>% diag()
var_Y

cumsum(var_Y)/sum(var_Y)

test_PCA <- bind_cols(test_PCA, Y) %>% 
  select(Id, open_date, City, city_group, Type, Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, revenue)


# In case our case the optimal method was ridge regression so we will need this 
# for glmnet()
test_X <- model.matrix(model_formula, data = test_PCA)[, -1]

# LASSO was most often ranked #1, so let's use it with the
# optimal ridge lambda value:
predictions <- model_LASSO%>% 
  predict(newx=test_X, s=lambda_star_LASSO) %>% 
  as.vector()

# Write submissions to CSV
PCA_submission <- sample_submission %>% 
  mutate(Prediction = as.vector(predictions)) %>% 
  write_csv("PCA_submission.csv")

