#Connor McCormick
#Kaggle: Shelter Animal Outcomes
#https://www.kaggle.com/c/shelter-animal-outcomes/

# 1. Load Packages ----------------------------------------------------------------
library(tidyverse)
library(broom)
library(plotly)
library(glmnet)
library(stringr)
library(lubridate)
library(caret)
library(MLmetrics)
library(randomForest)

# 2. Load and Clean Data ----------------------------------------------------------------
train <- read_csv("Files/train.csv")
test <- read_csv("Files/test.csv")
sample <- read_csv("Files/sample_submission.csv")


clean <- train %>%
  tbl_df() %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  mutate(Name = ifelse(is.na(Name),"Noname", Name)) %>%
  mutate(day = wday(DateTime)) %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime))

cleaner <- clean %>%
  mutate(female = ifelse(grepl("Female", SexuponOutcome), 1, ifelse(grepl("Unknown", SexuponOutcome), .5, 0))) %>%
  mutate(intactanimal = ifelse(grepl("Intact", SexuponOutcome), 1, ifelse(grepl("Unknown", SexuponOutcome), .5, 0))) %>%
  mutate(mix = ifelse(grepl("Mix", Breed), 1, 0)) 

#doing ages
cleanest <- cleaner %>%
  mutate(agenumber = ifelse(AgeuponOutcome == "Unknown", NA, as.integer(substr(AgeuponOutcome, 0, 1)))) %>%
  mutate(unit = ifelse(grepl("year", AgeuponOutcome), 365, 
                       ifelse(grepl("month",AgeuponOutcome), 30, 
                              ifelse(grepl("weeks", AgeuponOutcome), 7, 1)))) %>%
  mutate(agedays = agenumber*unit)  %>%
  mutate(ageindays = ifelse(is.na(agedays), mean(agedays, na.rm = TRUE), agedays))

#final cleaning 
superclean <- cleanest %>%
  mutate(Dog = ifelse(grepl("Dog", AnimalType), 1, 0)) %>%
  mutate(named = ifelse(grepl("Noname", Name), 0, 1)) %>% 
  mutate(Color = ifelse(Color %in% c("Calico", "Torbie", "Tricolor", "Tortie", "Buff", "Seal Point"), 
                        "/", Color)) %>%
  mutate(solid = ifelse(grepl("/", Color), 0, 1)) %>%
  mutate(babyanimal = ifelse(ageindays<365, 1, 0))%>%
  select(AnimalID, OutcomeType, Dog, month, female, intactanimal, mix, named, ageindays, solid, babyanimal)

# 3. EDA ----------------------------------------------------------------

#looking at the effects of being dog/cat
animal <- cleaner %>%
  group_by(AnimalType, OutcomeType) %>%
  summarise(num_animals = n())

ggplot(animal, aes(x=OutcomeType, y=num_animals, fill = AnimalType))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Shelter Animal Outcomes", x = "Outcome Type", y = "Number of Animals")


#looking at the effects of being spayed/neutered
intact <- cleaner %>%
  group_by(intactanimal, OutcomeType) %>%
  summarise(num_animals = n())

ggplot(intact, aes(x=OutcomeType, y=num_animals, fill = intactanimal))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Fixed vs. Intact", x = "Outcome Type", y = "Percent of Animals")
#This visualization shows the major component of my CART model, which using whether an
#animal is fixed to determine it's outcome.  The graph clearly shows the majority of 
#animals who are adopted are fixed, while most of the intact animals (not fixed) are 
#transfered.  Likely, the Austin Animal Shelter does not have the capacity to spay and
#neuter all of the animals they recieve, so they transfer many of them to be fixed


#looking at ages
ageatoutcome <- cleanest %>%
  group_by(OutcomeType) %>%
  summarise(avg_age = mean(ageindays))
kable(ageatoutcome)

avgage <- cleanest %>%
  group_by(ageindays) %>%
  summarise(num_age = n())

ggplot(avgage, aes(x=ageindays, y=num_age))+
  geom_point()+
  labs(title = "Age at Outcome", x = "Approximate Age in Days", y = "Number of Animals")
#Since the animals coming into the shelter are strays or people's pets they've lost,
#their ages are approximated.  

#one last table compiling the means for each outcome by variable.
edas <- superclean %>%
  group_by(OutcomeType) %>%
  summarise(mean(ageindays), mean(intactanimal), mean(Dog), mean(female), 
            mean(mix), mean(month), mean(named), mean(solid), mean(babyanimal))
kable(edas)


#test data cleaning
testclean <- test %>%
  tbl_df() %>%
  mutate(Name = ifelse(is.na(Name),"Noname", Name)) %>%
  mutate(day = wday(DateTime)) %>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime))%>%
  mutate(female = ifelse(grepl("Female", SexuponOutcome), 1, ifelse(grepl("Unknown", SexuponOutcome), .5, 0))) %>%
  mutate(intactanimal = ifelse(grepl("Intact", SexuponOutcome), 1, ifelse(grepl("Unknown", SexuponOutcome), .5, 0))) %>%
  mutate(mix = ifelse(grepl("Mix", Breed), 1, 0)) %>%
  mutate(agenumber = ifelse(AgeuponOutcome == "Unknown", NA, as.integer(substr(AgeuponOutcome, 0, 1)))) %>%
  mutate(unit = ifelse(grepl("year", AgeuponOutcome), 365, 
                       ifelse(grepl("month",AgeuponOutcome), 30, 
                              ifelse(grepl("weeks", AgeuponOutcome), 7, 1)))) %>%
  mutate(agedays = agenumber*unit)  %>%
  mutate(ageindays = ifelse(is.na(agedays), mean(agedays, na.rm = TRUE), agedays)) %>%
  mutate(Dog = ifelse(grepl("Dog", AnimalType), 1, 0)) %>%
  mutate(named = ifelse(grepl("Noname", Name), 0, 1)) %>%
  mutate(Color = ifelse(Color %in% c("Calico", "Torbie", "Tricolor", "Tortie", "Buff", "Seal Point"), 
                        "/", Color)) %>%
  mutate(solid = ifelse(grepl("/", Color), 0, 1)) %>%
  mutate(babyanimal = ifelse(ageindays<365, 1, 0))%>%
  select(ID, Dog, month, female, intactanimal, mix, ageindays, named, solid, babyanimal)


# 4. CV ----------------------------------------------------------------
#trying some KNN
#need some more variation, too many ties
superclean <- superclean %>%
  mutate(ageindays = jitter(ageindays))

#simple model
neighs <- 5
knnmod <-
  knn3(OutcomeType ~ Dog+intactanimal+mix+ageindays+month+named+female+solid+babyanimal, data=superclean, k = neighs)

#CVing
kneighs <- seq(150, 200, by = 5)
kscore <- rep(0, length(kneighs))

#folding
folds <- 8
train_data <- superclean %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:folds, length=n())) %>%
  arrange(fold)

#looping
for(j in 1:length(kneighs)){
  score <- rep(0, folds)
  
  for(i in 1:folds){
    # pseudo train/test data sets
    pseudotrain <- train_data %>%
      filter(fold != i)
    pseudotest <- train_data %>%
      filter(fold == i)
    
    #fit model
    loopmod <-knn3(OutcomeType ~ Dog+intactanimal+mix+ageindays+month+female+solid+babyanimal, data=pseudotrain, k =kneighs[j])
    
    #predict using model from loop
    probs <- predict(loopmod, newdata=pseudotest, type=c("prob", "class"))
    
    #scoring (same as score on kaggle)
    scorekag <- MultiLogLoss(y_true = as.factor(pseudotest$OutcomeType), y_pred = probs)
    
    #score for each fold
    score[i] <- scorekag
  }
  #cv score for k neighs
  kscore[j] <- mean(score)
  
  #loop progress
  if (j%%1 == 0){print(j)}
}
#originally did 5 to 500 for neighbors, but 150-200 was the minimum, so I wanted to look
#more closely here

scoresbyk <- kneighs %>% 
  tbl_df() %>% 
  mutate(score = kscore) %>%
  rename(k = value)

scoresbyk %>%
  ggplot(aes(x = k, y = score)) + 
  geom_point() +
  coord_cartesian(ylim = c(1.03, 1.06))

# 5. Submission ----------------------------------------------------------------

#min at 165, but this is about as good as the CART model
bestk <- 165

#test set
knnfinal <- knn3(OutcomeType ~ Dog+intactanimal+mix+ageindays+month+named, train_data, k = bestk)

probabilities <- testclean %>%
  predict(knnfinal, newdata=., type=c("prob", "class")) %>% 
  tbl_df()  %>%
  mutate(ID = 1:n()) 

submission3 <- probabilities%>% 
  write_csv("Connor_McCormick_submission.csv")
#score: 1.01763, better than either of the CART models.  






# 6. Extras ----------------------------------------------------------------

#Looking at outcomes by animal as a percentage 
ggplot(animal, aes(x=OutcomeType, y=num_animals, fill = AnimalType))+
  geom_bar(stat = "identity", position = "fill")+
  labs(title = "Shelter Animal Outcomes", x = "Outcome Type", y = "Percent of Animals")

#Using CART
#formula
model_formula <- as.formula("OutcomeType ~ Dog+mix+ageindays+month+named+solid+female+babyanimal")
#I removed intact animal, the variable for whether the animal is fixed, because it was a very
#strong determining predictor.  Also, it appears that the shelter puts fixed animals up for 
#adoption, while transfering animals who are not fixed to partners to be fixed.  


#CART Model without intactanimal variable

knobs <- rpart.control(maxdepth = 30)
model <- rpart(model_formula, data = superclean, control=knobs, method = "class", xval = 10)

plotcp(model)
printcp(model)

print(model)
summary(model)

par(mfrow=c(1,1))
plot(model, margin=0.25)
text(model, use.n = TRUE)
box()
sub <- testclean %>%
  predict(model, newdata=.)%>% 
  tbl_df() %>%
  mutate(ID = 1:n()) 

submission <- sub%>% 
  write_csv("Connor_McCormick_submission6.csv")
#score of 1.06466, before named included
#now 1.07458, named made it worse

#bringing back intact animals
#should give a more accurate prediction
model_formula2 <- as.formula("OutcomeType ~ Dog+intactanimal+mix+ageindays+month+named+female+solid+babyanimal")

#CART Model with intactanimal variable

knobs <- rpart.control(maxdepth = 30)
model2 <- rpart(model_formula2, data = superclean, control=knobs, method = "class", xval = 10)

plotcp(model2)
printcp(model2)

par(mfrow=c(1,1))
plot(model2, margin=0.25)
text(model2, use.n = TRUE)
box()

sub2 <- testclean %>%
  predict(model2, newdata=.)%>% 
  tbl_df() %>%
  mutate(ID = 1:n()) 

submission3 <- sub2%>% 
  write_csv("Connor_McCormick_submission_intact.csv")

#slightly better score of 1.03623
#named did not change anything, still only using intact to determine it

