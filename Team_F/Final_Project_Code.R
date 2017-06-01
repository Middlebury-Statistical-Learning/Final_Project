#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Aayam Poudel and David Valentin
# Kaggle Competition Name: Rossman Store Sales
# Kaggle Competition URL: https://www.kaggle.com/c/rossmann-store-sales
#-------------------------------------------------------------------------------

# 1. Load All Necessary Packages ------------------------------------------
library(tidyverse)
library(broom)
library(lubridate)
#cool themes
library(ggthemes)
library(glmnet)
library(bisoreg)



# 2. Load Data Files & Data Cleaning --------------------------------------

train <- read_csv("./Files/train.csv")
test <- read_csv("./Files/test.csv")
sample_submissions <- read_csv("Files/sample_submission.csv")
#supplemetal information about the stores:
store <- read_csv("./Files/store.csv")

#since the store info is relevant to both train and test datasets let's join them to the datasets
train <- left_join(train,store, by="Store")
test <- left_join(test,store,by="Store")

#Since the dataset is too big, we couldn't do proper conputations so we decided to make a subset of it
#comment for all the sections except the extras (although computations might take some time)
#for extras, we recommend that you uncomment the following 2 lines code or computations will be very slow

#train <- train %>%
#sample_frac(0.1)

#modify train to get variables that make sense
modified_train <- train %>%
  mutate(Year=year(Date), Month=month(Date), DayOfMonth=day(Date)) %>%
  select(-Date, -Promo2SinceWeek, -PromoInterval, -Promo2SinceYear, -CompetitionOpenSinceMonth, -CompetitionOpenSinceYear) %>%
  mutate(StateHoliday = ifelse(is.na(StateHoliday), 0, StateHoliday)) %>%
  mutate(CompetitionDistance = ifelse(is.na(CompetitionDistance), 0, CompetitionDistance)) %>%
  group_by(Store) %>%
  mutate(avg_sales_by_storetype = mean(Sales)) %>%
  mutate(avg_customers_by_storetype = mean(Customers)) %>%
  mutate(avg_distance_by_storetype = mean(CompetitionDistance)) %>%
  mutate(total_num_promotions = sum(Promo)) %>%
  mutate(Sales_Per_Customer = signif(Sales/Customers, 5)) %>%
  mutate(Sales_Per_Customer = ifelse(is.na(Sales_Per_Customer), 0, Sales_Per_Customer))

#modify test to get variables that make sense
modified_test <- test %>%
  mutate(Year=year(Date), Month=month(Date), DayOfMonth=day(Date)) %>%
  select(-Date, -Promo2SinceWeek, -PromoInterval, -Promo2SinceYear, -CompetitionOpenSinceMonth, -CompetitionOpenSinceYear) %>%
  mutate(CompetitionDistance = ifelse(is.na(CompetitionDistance), 0, CompetitionDistance)) %>%
  mutate(Open = ifelse(is.na(Open), 0, Open)) %>%
  mutate(StateHoliday = ifelse(is.na(StateHoliday), 0, StateHoliday)) %>%
  group_by(Store) %>%
  mutate(avg_distance_by_storetype = mean(CompetitionDistance)) %>%
  mutate(total_num_promotions = sum(Promo))


#group data by day
group_by_day <- modified_train %>%
  group_by(DayOfWeek, Year) %>%
  summarise(TotalSales = mean(Sales)) %>% mutate(String_Day_Of_Week =
                                                   ifelse(DayOfWeek == 1, "Monday",
                                                          ifelse(DayOfWeek == 2, "Tuesday",
                                                                 ifelse(DayOfWeek == 3, "Wednesday",
                                                                        ifelse(DayOfWeek == 4, "Thursday",
                                                                               ifelse(DayOfWeek == 5, "Friday",
                                                                                      ifelse(DayOfWeek == 6, "Saturday",
                                                                                             ifelse(DayOfWeek == 7, "Sunday", 0))))))))




# Lets group data by month and create some new variables for plotting
group_by_month <- modified_train %>%
  group_by(Month, Year) %>%
  summarise(TotalSales = mean(Sales), MedianSales = median(Sales), PromotionsByMonth = sum(Promo2)) %>%
  mutate(String_Month = ifelse(Month == 1, "January",
                               ifelse(Month == 2, "February",
                                      ifelse(Month == 3, "March",
                                             ifelse(Month == 4, "April",
                                                    ifelse(Month == 5, "May",
                                                           ifelse(Month == 6, "June",
                                                                  ifelse(Month == 7, "July",
                                                                         ifelse(Month == 8, "August",
                                                                                ifelse(Month == 9, "September",
                                                                                       ifelse(Month == 10, "October",
                                                                                              ifelse(Month == 11, "November",
                                                                                                     ifelse(Month == 12, "December", 0)))))))))))))



# 3. Top 4-5 Visualizations/Tables of EDA ---------------------------------


#3a EDA on day of week and month
#_______________________________


# Lets take a look at the mean of the sales by day of week!
dayofweek_sales <- ggplot(group_by_day, aes(y=TotalSales, x=DayOfWeek, fill=String_Day_Of_Week)) +
  geom_bar(stat="identity") +
  facet_wrap(~Year) +
  scale_y_log10() +
  ggtitle("Mean Number of Sales by Day of Week") + theme_fivethirtyeight() +
  theme(axis.title = element_text()) + ylab('Mean Number of Sales') + xlab("Day of Week") +
  scale_color_discrete(name="Day of the Week")


# We can see that Sunday definitely has the lowest number of Sales reported, which makes sense.
dayofweek_sales

#3b EDA on Sales by Month
#________________________



# Lets take a look at the mean of the sales by day of week!
month_sales <- ggplot(group_by_month, aes(y=TotalSales, x=Month, fill=String_Month)) +
  geom_bar(stat="identity") +
  facet_wrap(~Year) +
  ggtitle("Mean Number of Sales by Month") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + ylab('Mean Sales') + xlab("Month")


# We can see that December has the highest Mean Sales, which makes sense because of the all the Holidays then!
month_sales

#3c. EDA on Promo
#________________


# Let's look at the correlation between Sales and whether there was a promo event or not.
promo_vs_sales <- ggplot(train, aes(x=Promo, y=Sales, group=Promo)) +
  geom_boxplot(alpha=0.2) +
  scale_y_log10() +
  ggtitle("Non-Promo vs Promo Sales") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + ylab('Sales') + xlab("Non-Promotion VS Promotion")

# Interesting, promotions definitely have an effect the number of sales by looking at it through a log scale.
promo_vs_sales

#3d. EDA on Store Type
#________________

#Lets group data by Store Type and create some new variables for plotting

group_by_promos <- modified_train %>% group_by(StoreType, Year) %>%
  summarise(Mean_Sales_By_Storetype = mean(Sales), Median_Sales_By_Storetype = median(Sales), Promotions_By_StoreType = sum(Promo))

#Lollipop Test Chart
lollipop_sales_by_storetype <- ggplot(group_by_promos, aes(x=Mean_Sales_By_Storetype, y=StoreType, color=StoreType, label = paste0(round(Mean_Sales_By_Storetype, 0)))) +
  geom_segment(aes(x = 0, y = StoreType, xend = Mean_Sales_By_Storetype, yend = StoreType), color = "black") +
  facet_wrap(~Year) +
  xlim(0, 12000) +
  ggtitle("Sales By Store Type") +
  geom_point(size=11) +
  geom_text(color = "white", size = 3) +
  theme_fivethirtyeight() +
  theme(panel.spacing = unit(2, "lines"), axis.title = element_text()) + xlab('Sales') + ylab("Store Type")

# We can see that year on year, StoreType B had increased sales over other StoreTypes. Maybe, we should investigate this further.
lollipop_sales_by_storetype

#3e. EDA on Assortment
#_____________________

#Lets group by assortment and year, which allows us to see a breakdown in possible trends from year to year!
group_by_assortment <- modified_train %>% group_by(Assortment, Year) %>%
  summarise(Mean_Sales_By_Assortment = mean(Sales), Median_Sales_By_Assortment = median(Sales), Promotions_By_Assortment = sum(Promo2))

#Lolipop chart by assortment
#Interesting, the store with assortment type b reported larger number of sales than the other stores!
lollipop_sales_by_assortment <- ggplot(group_by_assortment, aes(x=Mean_Sales_By_Assortment, y=Assortment, color=Assortment, label = paste0(round(Mean_Sales_By_Assortment, 0)))) +
  geom_segment(aes(x = 0, y = Assortment, xend = Mean_Sales_By_Assortment, yend = Assortment), color = "black") +
  facet_wrap(~Year) +
  xlim(0, 10000) +
  ggtitle("Sales By Assortment Type") +
  geom_point(size=12) +
  geom_text(color = "white", size = 3) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + xlab('Sales') + ylab("Assortment Type")

lollipop_sales_by_assortment

#3e. EDA on Competition Distance
#_____________________

train_violin <- modified_train %>%
  group_by(Store, StoreType) %>%
  filter(Open == 1) %>%
  summarise(mean_sales = mean(Sales), mean_customers = median(Customers), total_promos = sum(Promo), avg_sales_per_customer = round(mean_sales/mean_customers, 3), avg_num_promotions = mean(Promo), avg_sales_per_total_promotions = round(mean_sales/avg_num_promotions, 3), avg_competition_distance = round(mean(CompetitionDistance),5), avg_sales_by_competition_distance = round(mean_sales/avg_competition_distance, 3))

#Interesting, the total number of promotions doesn't seem to indicate an increase in average sales.
avg_sales_by_avg_distance <- ggplot(train_violin, aes(x=mean_sales, y=avg_competition_distance, fill=StoreType)) +
  geom_violin(alpha=0.7, color="gray")+
  coord_flip() +
  facet_wrap(~StoreType) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face="bold")) + xlab('Total Number of Sales') + ylab("Average Distance") +
  ggtitle("Average Distance by Number of Sales") +
  scale_color_discrete(name="Store Type")

# Note that for Store Types of Type B is somewhat incomplete; however, there is a variability with average sales with stores c and d.
avg_sales_by_avg_distance



# 4. Cross-Validation of Final Model --------------------------------------
# We are using LOESS with span=0.02 for our final model because it gave us the best score!
# If you're interested in looking at the full CV of how we got span = 0.02, it's in the extras section.

# Add folds and randomize
n_folds <- 10
modified_train <- modified_train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n()))


# Save scores here
score_vector <- rep(0, n_folds)

for(i in 1:n_folds){
  # 1. Create pseudo train/test based on folding scheme
  pseudo_train <- modified_train %>%
    filter(fold != i)
  pseudo_test <- modified_train %>%
    filter(fold == i) %>% as.data.frame()

  #giving id's to pseudo test
  Id <- rownames(pseudo_test)
  pseudo_test <- cbind(Id=Id, pseudo_test)

  # 2. Fit model on training data
  model_loess <- loess(Sales ~ CompetitionDistance, pseudo_train, span=0.02)

  # 3. Make predictions on test data & evaluate score
  # Making predictions and getting the desired scores wasn't easy so we had to make a bunch of
  # new variables and get to the point where we wanted

  predictions_loess <- predict(model_loess, newdata=pseudo_test$CompetitionDistance) %>%
    tbl_df()
  final_loess <- data.frame(pseudo_test$Id,predictions_loess$value) %>%
    transmute(Id=pseudo_test.Id,PredictedSales=predictions_loess.value)
  pseudo_test_2 <- pseudo_test %>%
    mutate(ActualSales = Sales) %>%
    select(Id, ActualSales)
  final_loess <- left_join(final_loess,pseudo_test_2, by="Id" )
  final_loess_0_ignored <- final_loess %>%
    filter(ActualSales != 0)
  score_vector[i] <- final_loess_0_ignored %>%
    summarize(score = sqrt(mean(((ActualSales-PredictedSales)/ActualSales)^2)))

  print(i)

}

# Estimated score
mean(as.numeric(score_vector))
#gave us a score of 0.41, which is very close to the submission score of 0.37



# 5. Create Submission ----------------------------------------------------
span <- 0.02
model_loess <- loess(Sales ~ CompetitionDistance, modified_train, span=span)

predictions_loess <- predict(model_loess, newdata=modified_test$CompetitionDistance) %>%
  tbl_df()

sample_submission_loess <- data.frame(modified_test$Id,predictions_loess$value) %>%
  transmute(Id=modified_test.Id,Sales=predictions_loess.value)

sample_submission_loess %>%
  readr::write_csv("Files/FINAL_SUBMISSION.csv")


# 6. Extras ---------------------------------------------------------------

# Section A: Splines

#First lets perform CV on a spline model with different df values to find the ideal df value for the model
#Potential df values
df_vals <- seq(from=1 , to = 20)

#To store scores for each df_values
df_scores <- rep(0,100)


for(i in df_vals){

  # Assign folds at random
  n_folds <- 10

  modified_train_2 <- modified_train %>%
    # Equivalent to shuffling the rows:
    sample_frac(1) %>%
    mutate(fold = rep(1:n_folds, length=n()))

  #score for each iteration of the inner for loop
  instance_score <- rep(0, n_folds)

  #for each fold
  for(j in 1:n_folds){
    #disjoint train and test sets
    pseudo_train <- modified_train_2 %>%
      filter(fold != j)


    pseudo_test <- modified_train_2 %>%
      filter(fold == j)  %>% as.data.frame()
    #id the test dataset
    Id <- rownames(pseudo_test)
    pseudo_test <- cbind(Id=Id, pseudo_test)

    #fit the model
    model_spline <- smooth.spline(x=pseudo_train$CompetitionDistance, y=pseudo_train$Sales, df=i)

    #Predict on the model
    spline_predictions <- model_spline %>%
      predict(x=pseudo_test$CompetitionDistance) %>%
      tbl_df() %>%
      transmute(CompetitionDistance = x, PredictedSales = y)

    pseudo_test_2 <- pseudo_test %>%
      select(Id, CompetitionDistance,Sales) %>%
      mutate(ActualSales = Sales) %>%      select(-Sales)

    final_spline <- left_join(pseudo_test_2, spline_predictions, by="CompetitionDistance") %>%
      distinct(Id, PredictedSales) %>%
      left_join(pseudo_test_2, by="Id")


    #get score for each iteration of the for loop
    final_spline_0_ignored <- final_spline %>%
      filter(ActualSales != 0)

    #store the scores
    instance_score[j] <- final_spline_0_ignored %>%
      summarize(score = sqrt(mean(((ActualSales-PredictedSales)/ActualSales)^2)))

  }
  print(i)

  #Store the scores for each df values in a vector
  df_scores[i] <- as.numeric(instance_score) %>% mean()

}

#find the optimal df
df_star <- 0
prev_score = 1000000000
for (i in 1:length(df_scores)){
  if(df_scores[i] < prev_score){
    df_star = i
    prev_score = df_scores[i]
  }
}


# Let's train a spline model based on the ideal df value and make submissions
# ideal df value
df <- df_star

#train a model
model_spline <- smooth.spline(x=modified_train$CompetitionDistance, y=modified_train$Sales, df=df)

#make predictions
spline_predictions <- model_spline %>%
  predict(x=modified_test$CompetitionDistance) %>%
  tbl_df() %>%
  transmute(CompetitionDistance = x, Sales = y)

modified_test_2 <- modified_test %>%
  select(Id, CompetitionDistance)

sample_submission_spline <- left_join(modified_test_2, spline_predictions, by="CompetitionDistance") %>%
  distinct(Id, Sales) %>%
  ungroup(Store) %>%
  select(Id,Sales)

#generate submissions for splines
sample_submission_spline %>%
  readr::write_csv("Files/sample_submission_spline.csv")

# Section B: Loess

#Let's perform Cross validation on Loess model with different span values to get the best model
#Potential span values
#tried a bunch of span values, and kept reducing them till we got to this level
span_vals <- c(0.02,0.04,0.06,0.08,0.1)

#To store scores for each df_values
span_scores <- rep(0,5)

#we will need index to store the scores
index <- 1
for(i in span_vals){

  # Assign folds at random
  n_folds <- 10

  modified_train_2 <- modified_train %>%
    # Equivalent to shuffling the rows:
    sample_frac(1) %>%
    mutate(fold = rep(1:n_folds, length=n()))

  #score for each iteration of the inner for loop
  instance_score <- rep(0, n_folds)

  #for each fold
  for(j in 1:n_folds){
    #disjoint train and test sets
    pseudo_train <- modified_train_2 %>%
      filter(fold != j)

    pseudo_test <- modified_train_2 %>%
      filter(fold == j)  %>% as.data.frame()
    #id the test dataset
    Id <- rownames(pseudo_test)
    pseudo_test <- cbind(Id=Id, pseudo_test)

    #fit a model on pseudo_train
    model_loess <- loess(Sales ~ CompetitionDistance, pseudo_train, span=i)

    #predict on pseudo_test
    predictions_loess <- predict(model_loess, newdata=pseudo_test$CompetitionDistance) %>%
      tbl_df()

    final_loess <- data.frame(pseudo_test$Id,predictions_loess$value) %>%
      transmute(Id=pseudo_test.Id,PredictedSales=predictions_loess.value)

    pseudo_test_2 <- pseudo_test %>%
      mutate(ActualSales = Sales) %>%
      select(Id, ActualSales)

    final_loess <- left_join(final_loess,pseudo_test_2, by="Id" )

    final_loess_0_ignored <- final_loess %>%
      filter(ActualSales != 0)

    instance_score[j] <- final_loess_0_ignored %>%
      summarize(score = sqrt(mean(((ActualSales-PredictedSales)/ActualSales)^2)))

  }

  print(as.numeric(instance_score) %>% mean())
  #Store the scores for each span values in a vector
  span_scores[index] <- as.numeric(instance_score) %>% mean()

  index <- index + 1

}

#find the optimal span using this chunk of code
span_star <- 0
prev_score = 1000000000
for (i in 1:length(span_scores)){
  if(span_scores[i] < prev_score){
    span_star = span_vals[i]
    prev_score = span_scores[i]
  }
}

#using the ideal span value, let's train a model
span <- span_star
model_loess <- loess(Sales ~ CompetitionDistance, modified_train, span=span)

#let's make predictions on the model
predictions_loess <- predict(model_loess, newdata=modified_test$CompetitionDistance) %>%
  tbl_df()

sample_submission_loess <- data.frame(modified_test$Id,predictions_loess$value) %>%
  transmute(Id=modified_test.Id,Sales=predictions_loess.value)

#submit submissions to kaggle
sample_submission_loess %>%
  readr::write_csv("Files/sample_submission_loess.csv")


# Section C: Ridge Regression

#the formula for the train and test models for both Ridge regression and lasso
model_formula <- Sales ~ Store + DayOfWeek + Open + StateHoliday + SchoolHoliday + CompetitionDistance + Promo2 + Year + Month + DayOfMonth + avg_distance_by_storetype + total_num_promotions
model_formula_test <- ~ Store + DayOfWeek + Open + StateHoliday + SchoolHoliday + CompetitionDistance + Promo2 + Year + Month + DayOfMonth + avg_distance_by_storetype + total_num_promotions
X <- model.matrix(model_formula, data = modified_train)[, -1]
y <- modified_train$Sales

n_folds <- 5
#Cross validation for ridge regression

lambda_values <- 10^seq(from = -2, to = 4, length = 2500)
# Using built-in function, we don't need to use our own for-loops. Note
# alpha=0 for ridge regression
cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = n_folds)

#find the optimal lambda value
lambda_star_ridge <- cv_ridge %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_ridge

#Train a ridge regression model based on optimal lambda value
model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_star_ridge)

test_X <- model.matrix(model_formula_test, data = modified_test)[, -1]

#make predictions
predictions_ridge <- model_ridge %>%
  predict(newx=test_X, s=lambda_star_ridge) %>%
  as.vector()

sample_submission_ridge <- sample_submissions %>%
  mutate(Sales = as.vector(predictions_ridge))

#submit to kaggle
sample_submission_ridge %>%
  readr::write_csv("Files/sample_submission_ridge.csv")

# Section D: LASSO
n_folds <- 5
#Cross validation for lasso

lambda_values <- 10^seq(from = -2, to = 4, length = 2500)
# Using built-in function, we don't need to use our own for-loops. Note
# alpha=1 for lasso
cv_lasso <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = n_folds)

#find the optimal lambda value
lambda_star_lasso <- cv_lasso %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_lasso

#Train a lasso model based on optimal lambda value
model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_star_lasso)

test_X <- model.matrix(model_formula_test, data = modified_test)[, -1]

#make predictions
predictions_lasso <- model_lasso %>%
  predict(newx=test_X, s=lambda_star_lasso) %>%
  as.vector()

sample_submission_lasso <- sample_submissions %>%
  mutate(Sales = as.vector(predictions_lasso))

#submit to kaggle
sample_submission_lasso %>%
  readr::write_csv("Files/sample_submission_lasso.csv")
