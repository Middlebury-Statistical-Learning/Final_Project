#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Nina Sonneborn, Rebecca Conover
# Kaggle Competition Name: Airbnb New User Bookings
# Kaggle Competition URL:https://www.kaggle.com/c/airbnb-recruiting-new-user-bookings
#-------------------------------------------------------------------------------

# 1. Load All Necessary Packages ------------------------------------------
library(tidyverse)
library(broom)
library(stringr)
library(lubridate)
library(glmnet)
library(plotly)
library(caret)
library(rpart)



# 2. Load Data Files & Data Cleaning --------------------------------------
train <- read_csv('File/train_users_2.csv')
sessions <- read_csv('Files/sessions.csv')
test <- read_csv('Files/test_users.csv')

# We did some stuff with LASSO that took a long time to run, so we saved our
# models as Rdata objects to save the trouble. The code used to generate can be
# found in the extras section

load('Files/Team_J.Rdata')

# 3. Top 4-5 Visualizations/Tables of EDA ---------------------------------

# What do the abbreviations stand for?

country_abbrevs<- cbind(unique(train$country_destination), 
                        c("No destination found","US", "other", "France", "Canada", "United Kingdom",
                          "Spain", "Italy", "Portugal","Netherlands", "Germany","Australia"))
country_abbrevs


distribution_country_destination <-
  train %>% 
  group_by(country_destination) %>%
  summarise(count = n(), prop = n()/nrow(train)) %>% 
  arrange(desc(prop)) 

distribution_country_destination %>% 
  ggplot(aes(x = reorder(country_destination, desc(prop)) , y=prop)) +
  geom_bar(stat = "identity") +
  labs(title = "Outcome variable: Country Destination in train
       data",
       x = "Country", y = "Proportion of users")

# This is the 2Norm beta coefficient plot from running LASSO on a training set
# where categorical predictors were pruned to limit the number levels (keeping
# the most popular).

#the code that generated this model is in the extras section, it take forever
#(20 mins) to run so we saved you the trouble

plot(model_lasso_pruned, xvar = "lambda", label = TRUE, type.coef = "2norm")



# Note: a consequence of the distribution of users among country_destination being
# skewed toward NDF and US is that pruning based on proportion can remove specificity
# of the model on levels other than those 2 outcomes.
# That is to say: Most recorded users might have affiliate_provider google and craiglist,
# simply because thats the tendency of US users who travel in the US
# (and they make up most of the sample too). Baidu is Chinese and could give more detail.
# So the model would get good at discriminating between NDF/US/EverythingElse
# but not great at discriminating within the EverythingElse category


# Pruning categories on train data ----------------------


n_distinct(train$gender)
n_distinct(train$signup_method)
n_distinct(train$signup_app)
n_distinct(train$language)
n_distinct(train$affiliate_channel)
n_distinct(train$affiliate_provider)
n_distinct(train$first_affiliate_tracked)
n_distinct(train$first_device_type)
n_distinct(train$first_browser)

# based on these results, I think I should prune (or remove) the following categories:
# -language
# -affiliate_provider
# -first_device_type
# -first_browser
# -first_affiliate_tracked
# -affiliate_channel

# -language
distribution_language <-
  train %>% 
  group_by(language) %>%
  summarise(count = n(), prop = n()/nrow(train)) %>% 
  arrange(desc(prop))

# -affiliate_provider
distribution_affiliate_provider <-
  train %>% 
  group_by(affiliate_provider) %>%
  summarise(count = n(), prop = n()/nrow(train)) %>% 
  arrange(desc(prop))

# -affiliate channel
distribution_affiliate_channel <-
  train %>% 
  group_by(affiliate_channel) %>%
  summarise(count = n(), prop = n()/nrow(train)) %>% 
  arrange(desc(prop))

# -first_affiliate_tracked
distribution_first_affiliate_tracked <-
  train %>% 
  group_by(first_affiliate_tracked) %>%
  summarise(count = n(), prop = n()/nrow(train)) %>% 
  arrange(desc(prop))

# -first_device_type
distribution_first_device_type <-
  train %>% 
  group_by(first_device_type) %>%
  summarise(count = n(), prop = n()/nrow(train)) %>% 
  arrange(desc(prop))

# -first_browser
distribution_first_browser <-
  train %>% 
  group_by(first_browser) %>%
  summarise(count = n(), prop = n()/nrow(train)) %>% 
  arrange(desc(prop))

distribution_language
distribution_affiliate_provider
distribution_first_affiliate_tracked
distribution_first_browser
distribution_first_browser

# GENDER - combine other and unknown into one factor

# LANGUAGE - Conflicted. Either keep all.. Could be important. 
# Or: keep only native tongues of country_destination category?
# https://www.loc.gov/standards/iso639-2/php/code_list.php
# For now, top 9? 
top_n_language <-
  distribution_language %>%
  slice(1:9)

# AFFILIATE PROVIDER -Affiliate provider could be a proxy for language 
# (which is proxy for user country).
# Take top 6 on this one (direct, google, craiglist, bing, facebook, "other")
top_n_affiliate_provider <- 
  distribution_affiliate_provider %>%
  # exclude row 3 (other)
  slice(c(1:2, 4:6))

# FIRST AFFILIATE TRACKED
# Take top 3 on this on this one (untracked, linked, omg, "other")
top_n_affiliate_tracked <-
  distribution_first_affiliate_tracked %>%
  slice(1:3)

# FIRST DEVICE TYPE
# Categories (desktop, phone, tablet, other/unknown)
# Users may be more likely to book from a computer and
# browse on phones

desktops <- c("Mac Desktop", "Windows Desktop", "Desktop (Other)")
phones <- c("iPhone", "Android Phone", "SmartPhone (Other)")
tablets <- c("iPad", "Android Tablet")

# FIRST BROWSER
# Top 4 (Chrome, Safari, Firefox, -unknown-, "other")
# - Be sure to group Mobile Safari into safari, Chrome Mobile into Chrome, etc.
top_n_first_browser <- 
  distribution_first_browser %>%
  slice(1:4)

# Edit data
train_pruned <- train
train_pruned <- train_pruned %>%
  mutate(gender = ifelse(gender %in% c("FEMALE","MALE"), gender, "other/unknown"),
         language = ifelse(language %in% top_n_language$language, language, "other"),
         affiliate_provider = ifelse(affiliate_provider %in% top_n_affiliate_provider$affiliate_provider,
                                     affiliate_provider, "other"),
         first_affiliate_tracked = 
           ifelse(first_affiliate_tracked %in% top_n_affiliate_tracked$first_affiliate_tracked,
                  first_affiliate_tracked, "other"),
         first_device_type = 
           ifelse(first_device_type %in% desktops, "desktop",
                  ifelse(first_device_type %in% phones, "phone",
                         ifelse(first_device_type %in% tablets, "tablet", "Other/Unknown"))),
         first_browser = ifelse(first_browser %in% top_n_first_browser$first_browser,
                                first_browser, ifelse(first_browser == "Chrome Mobile", "Chrome",
                                                      ifelse(first_browser == "Mobile Safari", "Safari", "other")))
         
  )



# 4. Cross-Validation of Final Model --------------------------------------

model_formula_ALL <- train %>%
  names() %>% 
  setdiff(c("id", "timestamp_first_active", "country_destination", 
            "date_first_booking")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  stringr::str_c("country_destination ~ ", .)

#we took out date_first_booking because half of the values were missing, even 
#though it probably would be very useful if we had the tools/time to make sense
#of it.

model_formula_ALL <- as.formula(model_formula_ALL)


cv_CART <- function(complexity_parameters, n_folds) {
  
  cp_scores <- rep(0, length(complexity_parameters))
  
  # Assign folds at random
  n_folds <- n_folds
  train_pruned <- train_pruned %>%
    sample_frac(1) %>%
    mutate(fold = rep(1:n_folds, length = n())) %>%
    arrange(fold)
  
  for (j in 1:length(complexity_parameters)) {
    # Store your scores here:
    scores <- rep(0, n_folds)
    
    for (i in 1:n_folds) {
      # 1. Create disjoint pseudo-train and pseudo-test sets based on folding
      # scheme.
      pseudo_train <- train_pruned %>%
        filter(fold != i)
      pseudo_test <- train_pruned %>%
        filter(fold == i)
      
      # 2. Train the model using (i.e. fit the model to) the pseudo_train data.
      # set up the input for the model
      knobs_loop <-
        rpart.control(maxdepth = 30, cp = complexity_parameters[j])
      model_CART_loop <-
        rpart(
          model_formula_ALL,
          data = pseudo_train,
          control = knobs_loop,
          method = "class"
        )
      
      # 3. Get prediction for the pseudo_test data using the trained model
      predictions_loop <-
        predict(model_CART_loop, newdata = pseudo_test, type = "class")
      predictions_loop <-
        predictions_loop %>% tbl_df() %>% rename(country = value)
      submission_loop <-
        bind_cols(tbl_df(pseudo_test$id), predictions_loop) %>% rename(id = value)
      # 4. Compute your score on the pseudo_test data Using classification error
      # The Kaggle scoring mechanism for this competition is, (normalized 
      # discounted cumulative
      # gain)[https://en.wikipedia.org/wiki/Discounted_cumulative_gain], which 
      # is pretty complicated if you choose to make multiple, ranked predictions
      # for each user. There is no function in mlmetrics that does this.  But, 
      # when you only submit the top prediction for a user, the scoring 
      # mechanism boils down to proportion correct/classification error, so we 
      # used this for our scoring in CV.
      
      score <-
        sum(pseudo_test$country_destination == submission_loop$country) / nrow(pseudo_test)
      
      # 5. Save your score for this fold
      scores[i] <- score
    }
    # Store mean cross-validated score for this value of cp
    cp_scores[j] <- mean(scores)
    
    # Print statement to view progress of loop
    if (j %% 1 == 0) {
      print(j)
    }
  }
  # Create data frame with all tested cp values and corresponding sores
  scores_per_cp <- complexity_parameters %>% 
    tbl_df() %>% 
    mutate(score = cp_scores) %>%
    rename(cp = value)
  return(scores_per_cp)
}

wide_net_cv <- cv_CART(seq(0.00001, 0.10001, by = .01), 8)

wide_net_cv %>%
  ggplot(aes(x = cp, y = score)) + geom_point()


more_precise_cv <- cv_CART(seq(.00001, .011, by = .001), 8)

more_precise_cv %>%
  ggplot(aes(x=cp, y=score)) + geom_point()

optimal_cp <- 0.00101



# 5. Create Submission ----------------------------------------------------

knobs <- rpart.control(maxdepth = 30, cp=optimal_cp)
model_CART <- rpart(model_formula_ALL, data = train_pruned, control=knobs, method = "class")

plot(model_CART, margin=0.25)
text(model_CART, use.n = TRUE)
box()

predictions <- predict(model_CART, newdata = test_pruned_na_replace, type="class")
predictions <- predictions %>% tbl_df() %>% rename(country=value)
submission <- bind_cols(tbl_df(test$id),predictions) %>% rename(id=value)

write_csv(submission,"Files/submission.csv")


# Kaggle score: 0.69294

# Why is Kaggle score so different from cross-validated score?
# Test and train are not random samples of the population - they are
# segmented by date, and airbnb is an evolving company.
# For example, by submitting predictions all "NDF" (no destination found)
# and predictions all "US" we determine the composition of the test set
# with respect to these 2 outcomes:

submission %>% mutate(country = "NDF") %>% write_csv("submission_NDF.csv")
# Score: 0.67909
submission %>% mutate(country = "US") %>% write_csv("submission_US.csv")
# Score: 0.23470

# This tells us that test consists of 68% NDF and 23% US.
# Meanwhile, training data is 58% NDF and 29% US.
distribution_country_destination

# Using this information, to optimize Kaggle score we would have
# to do some sort of time trend analysis or assign different weights to US
# and NDF in training data.
# We didn't do this.

# 6. Extras ---------------------------------------------------------------

# Shrinkage on all variables without clean-up ---------------------------
model_formula_ALL <- train %>%
  # Take all predictor variable names and separate them with + signs:
  names() %>% 
  setdiff(c("id", "timestamp_first_active", "country_destination", 
            "date_first_booking")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("country_destination ~ ", .)
model_formula_ALL <- as.formula(model_formula_ALL)
model_formula_ALL

# Replace missing values
train_na_replace <- train %>% 
  mutate(
    date_first_booking = ifelse(is.na(date_first_booking),
                                mean(date_first_booking, na.rm=TRUE), 
                                date_first_booking),
    age = ifelse(is.na(age), mean(age, na.rm=TRUE), age))
# Once age and date_first_booking are taken care of, only 3% of rows have missing values.
# Seems okay to drop

train_na_replace <- na.omit(train_na_replace)

X <- model.matrix(model_formula_ALL, data = train_na_replace)[, -1]
y <- train_na_replace$country_destination

# This shit took 28 minutes to run
mbm = microbenchmark(
  model_lasso <- glmnet(X, y, alpha = 1, family = "multinomial", type.multinomial = "grouped",
                        maxit = 50000),
  times = 1
)

# And broom don't even work. Like, for serious?
my_plot <- plot(model_lasso, xvar = "lambda", label = TRUE, type.coef = "2norm")

# model_lasso$beta has a different matrix for each outcome (US, CA, etc.)
# model_lasso$beta$US can be tidy()'d
# - gotta interpret this stuff though

# Tidying multnet output --------

get_lambda <- function(sn){
  index <- as.numeric(str_replace(sn, "s", ""))
  return(lambdas[index])
}

# Note: this function does not work when "response" is passed as a parameter
# (even when as.name(response) is called)
# MUST GO IN AND CHANGE EACH RESPONSE IN THE FUNCTION TO SAVE TABLES (ie. cant
# pass parameter as variable in loop)


tidy_mulnet <- function(){
  lambdas <- model_lasso$lambda
  # response <- as.name(response)
  Beta_response <- model_lasso$beta$response %>%
    tidy() %>%
    rename(term=row, lambda=column, beta_coef = value) %>%
    mutate(lambda = get_lambda(lambda))
  return(Beta_response)
}

unique(train$country_destination)
Beta_AU <- tidy_mulnet() # where response is "AU"
Beta_US <- tidy_mulnet() # where response is "US"
Beta_CA <- tidy_mulnet() # where response is "CA"
Beta_NDF <- tidy_mulnet() # where response is "NDF"
Beta_other <- tidy_mulnet() # where response is "other"
Beta_FR <- tidy_mulnet() # where response is "FR"
Beta_GB <- tidy_mulnet() # where response is "GB"
Beta_ES <- tidy_mulnet() # where response is "ES"
Beta_IT <- tidy_mulnet() # where response is "IT"
Beta_PT <- tidy_mulnet() # where response is "PT"
Beta_NL <- tidy_mulnet() # where response is "NL"
Beta_DE <- tidy_mulnet() # where response is "DE"

AU_plot <- Beta_AU %>%
  mutate(log_lambda = log(lambda)) %>%
  ggplot(aes(x=log_lambda, y=beta_coef, col=term)) +
  geom_line()
ggplotly(AU_plot)

US_plot <- Beta_US %>%
  mutate(log_lambda = log(lambda)) %>%
  ggplot(aes(x=log_lambda, y=beta_coef, col=term)) +
  geom_line() + ggtitle("Response: US")
ggplotly(US_plot)

# multnet on pruned data train --------------------------------

# Replace missing values
train_pruned_na_replace <- train_pruned %>% 
  mutate(
    date_first_booking = ifelse(is.na(date_first_booking),
                                mean(date_first_booking, na.rm=TRUE), 
                                date_first_booking),
    age = ifelse(is.na(age), mean(age, na.rm=TRUE), age))

# Once age and date_first_booking are taken care of, only 3% of rows have missing values.
# Seems okay to drop

train_pruned_na_replace <- na.omit(train_pruned_na_replace)

# Still using the same model formula as the LASSO on un-pruned data
X_pruned <- model.matrix(model_formula_ALL, data = train_pruned_na_replace)[, -1]
y_pruned <- train_pruned_na_replace$country_destination



lambdas <- model_lasso$lambda

mbm2 = microbenchmark(
  model_lasso_pruned <- 
    glmnet(X_pruned, y_pruned, alpha = 1, family = "multinomial", 
           type.multinomial = "ungrouped", maxit = 50000, lambda = lambdas),
  times = 1
)

# the top of the graph's labels indicate how many nonzero coefs
plot(model_lasso_pruned, xvar = "lambda", label = TRUE, type.coef = "2norm")


# Also do multnet output tidying here
tidy_mulnet <- function(){
  lambdas <- model_lasso$lambda
  # response <- as.name(response)
  Beta_response <- model_lasso$beta$DE %>%
    tidy() %>%
    rename(term=row, lambda=column, beta_coef = value) %>%
    mutate(lambda = get_lambda(lambda))
  return(Beta_response)
}

unique(train$country_destination)
Beta_AU <- tidy_mulnet() # where response is "AU"
Beta_US <- tidy_mulnet() # where response is "US"
Beta_CA <- tidy_mulnet() # where response is "CA"
Beta_NDF <- tidy_mulnet() # where response is "NDF"
Beta_other <- tidy_mulnet() # where response is "other"
Beta_FR <- tidy_mulnet() # where response is "FR"
Beta_GB <- tidy_mulnet() # where response is "GB"
Beta_ES <- tidy_mulnet() # where response is "ES"
Beta_IT <- tidy_mulnet() # where response is "IT"
Beta_PT <- tidy_mulnet() # where response is "PT"
Beta_NL <- tidy_mulnet() # where response is "NL"
Beta_DE <- tidy_mulnet() # where response is "DE"

AU_plot <- Beta_AU %>%
  mutate(log_lambda = log(lambda)) %>%
  ggplot(aes(x=log_lambda, y=beta_coef, col=term)) +
  geom_line()
ggplotly(AU_plot)

US_plot <- Beta_US %>%
  mutate(log_lambda = log(lambda)) %>%
  ggplot(aes(x=log_lambda, y=beta_coef, col=term)) +
  geom_line() + ggtitle("Response: US")

ggplotly(US_plot)

NDF_plot <- Beta_NDF %>%
  mutate(log_lambda = log(lambda)) %>%
  ggplot(aes(x=log_lambda, y=beta_coef, col=term)) +
  geom_line() + ggtitle("Response: US")
ggplotly(NDF_plot)

# Cross Validation ------------------------------------
mbm4 = microbenchmark(
  cv_grouped <- 
    cv.glmnet(X_pruned, y_pruned, alpha=1, family="multinomial", nfolds=5,
              type.measure = "class", type.multinomial="ungrouped", lambda = lambdas)
)

#prune test
test_pruned <- test
test_pruned <- test_pruned %>%
  mutate(gender = ifelse(gender %in% c("FEMALE","MALE"), gender, "other/unknown"),
         language = ifelse(language %in% top_n_language$language, language, "other"),
         affiliate_provider = ifelse(affiliate_provider %in% top_n_affiliate_provider$affiliate_provider,
                                     affiliate_provider, "other"),
         first_affiliate_tracked = 
           ifelse(first_affiliate_tracked %in% top_n_affiliate_tracked$first_affiliate_tracked,
                  first_affiliate_tracked, "other"),
         first_device_type = 
           ifelse(first_device_type %in% desktops, "desktop",
                  ifelse(first_device_type %in% phones, "phone",
                         ifelse(first_device_type %in% tablets, "tablet", "Other/Unknown"))),
         first_browser = ifelse(first_browser %in% top_n_first_browser$first_browser,
                                first_browser, ifelse(first_browser == "Chrome Mobile", "Chrome",
                                                      ifelse(first_browser == "Mobile Safari", "Safari", "other")))
         
  )

#this does not work and it should?

test_x<-model.matrix(model_formula_ALL,data = test_na_replace)[,-1]

#or use optimal lamda from CV
lambda_test<-lambdas[40]

predictions<-predict(model_lasso_pruned,newx=test_x,type="class",s=lambda_test)


#Maybe need a sparsed matrix?
sparsed_test_data <- Matrix(data=0,
                            nrow=nrow(test_na_replace),
                            ncol=ncol(train_pruned),
                            dimnames=list(rownames(test_na_replace),
                                          colnames(train_pruned)),
                            sparse = T)

for(i in colnames(test_na_replace)){
  sparsed_test_data[, i] <- test_na_replace[, i]
}


test_x<-model.matrix(model_formula_ALL,data = sparsed_test_data)[,-1]

lambda_test<-lambdas[40]

predictions<-predict(model_lasso_pruned,newx=test_x,type="class",s=lambda_test)

#still doesnt work!





