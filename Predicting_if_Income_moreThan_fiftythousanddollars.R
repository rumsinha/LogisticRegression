# Loading all the libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(e1071)

# The prediction task is to determine whether a person makes over 50K a year
# Target variable has value 1 when the income is greater > 50,000 else value is 0

# Reading the data
# header is false since the dataset is having no header row
AdultData_df <-
  read.csv("adult.data", header = FALSE, stringsAsFactors = FALSE)

# Analyzing the dataset
head(AdultData_df)
str(AdultData_df)

#32561 observations and 15 variables of which 14 are the predictors and one is the target or response

# Since no headers exist, we will rename all the columns
names(AdultData_df) <-
  c(
    'age',
    # the age of an individual
    'workClass',
    # the employment status of an individual
    'fnlwgt',
    #the number of people census believes the entry represents
    'education',
    # the highest level of an education acheived
    'education_num',
    # the highest level of an education acheived in numeric form
    'marital_status',
    # the marital status
    'occupation',
    # the generic type of an occupation
    'relationshipStatus',
    # how the individual is related
    'race',
    # the race
    'gender',
    # the gender
    'capital_gain',
    # the capital gain of an individual
    'capital_loss',
    # the capital loss of an individual
    'hours_per_week',
    # the number of hours per week, an individual has reported for work
    'native_country',
    # country of origin
    'salaryRange' # the response or target variable
  )

head(AdultData_df)
glimpse(AdultData_df)

#####  Univariate and BiVariate Data Analysis #####
# Step 1) Exploring the continuous variables, the function select_if() from the dplyr library
# will select  the numerical columns
continuousVariables <- select_if(AdultData_df, is.numeric)
continuousVariables$fnlwgt <- NULL
continuousVariables$education_num <- NULL
#five point summary
summary(continuousVariables)
# we can see different scales and outliers

#From the summary, we can observe the minimum value of age is 17 and maximum value is 90
# for the Capital gain, mostly the values are 0 with mean at 1078
# for the Capital loss, mostly values are 0 with mean at 87.3
# hours_per_week, the minimum value is 1 and maximum is 99


# Histogram with kernel density curve
ggplot(continuousVariables, aes(x = age)) + geom_density(alpha = .2, fill = "#FF6666")
#ggplot(continuousVariables, aes(x = fnlwgt)) + geom_density(alpha = .2, fill = "#FF6666")
#ggplot(continuousVariables, aes(x = education_num)) + geom_density(alpha = .2, fill = "#FF6666")
ggplot(continuousVariables, aes(x = capital_gain)) + geom_density(alpha = .2, fill = "#FF6666")
ggplot(continuousVariables, aes(x = capital_loss)) + geom_density(alpha = .2, fill = "#FF6666")
ggplot(continuousVariables, aes(x = hours_per_week)) + geom_density(alpha = .2, fill = "#FF6666")
#Outliers
#Distributions

## Binning the age variable in equal representation of 20
AdultData_df$ageCat <-
  cut(AdultData_df$age, breaks = seq(0, 100, 20))
head(AdultData_df[, c('age', 'ageCat')])

# ageCat vs income
ggplot(AdultData_df, aes(x = ageCat, fill = salaryRange)) +
  geom_bar(position = "fill") +
  theme_classic()

ggplot(AdultData_df, aes(ageCat, ..count..)) + geom_bar(aes(fill = salaryRange), position = "dodge")

table(AdultData_df$ageCat)
AdultData_df$ageCat <- as.factor(AdultData_df$ageCat)

# Both capital gains and capital loss are right skewed hence we perform the log transformation
AdultData_df$logCapitalGain_v <- log(AdultData_df$capital_gain + 1)
#ggplot(AdultData_df, aes(x = logCapitalGain)) + geom_density(alpha = .2, fill = "#FF6666")

skewness(AdultData_df$capital_gain)
skewness(AdultData_df$capital_loss)
# skewed towards the right

#scaling the capital loss and capital gain
AdultData_df$scaledCapitalGain_v <- scale(AdultData_df$capital_gain)
AdultData_df$scaledCapitalLoss_v <- scale(AdultData_df$capital_loss)
#ggplot(AdultData_df, aes(x = scaledCapitalLoss)) + geom_density(alpha = .2, fill = "#FF6666")


AdultData_df$logCapitalLoss_v <- log(AdultData_df$capital_loss + 1)
#ggplot(AdultData_df, aes(x = logCapitalLoss)) + geom_density(alpha = .2, fill = "#FF6666")

AdultData_df$capitaldiff <- AdultData_df$capital_gain - AdultData_df$capital_loss
ggplot(AdultData_df, aes(x = capitaldiff)) + geom_density(alpha = .2, fill = "#FF6666")
skewness(AdultData_df$capitaldiff)

## hours per week minimum is 1 and maximum is 100
## binning in 20 hours category
AdultData_df$hoursWeekCat <-
  cut(AdultData_df$hours_per_week, breaks = seq(0, 100, 20))
head(AdultData_df[, c('hours_per_week', 'hoursWeekCat')])

# hoursWeekCatCat vs income
ggplot(AdultData_df, aes(x = hoursWeekCat, fill = salaryRange)) +
  geom_bar(position = "fill") +
  theme_classic()

ggplot(AdultData_df, aes(hoursWeekCat, ..count..)) + geom_bar(aes(fill = salaryRange), position = "dodge")

table(AdultData_df$hoursWeekCat)
AdultData_df$hoursWeekCat <- as.factor(AdultData_df$hoursWeekCat)

# will select  the numerical columns
continuousVariables <- select_if(AdultData_df, is.numeric)

# We will exclude fnlwgt from the dataframe
continuousVariables$fnlwgt <- NULL
# We will exclude the numerical form of the education and explore the categorical form
continuousVariables$education_num <- NULL

#five point summary
summary(continuousVariables)

#Step 2) Analyzing the string or factor variables
# trimming the white space
AdultData_df$workClass <- str_trim(AdultData_df$workClass)
unique(AdultData_df$workClass) #unique values of the column

# combining "Federal-gov","Local-gov","State-gov" into "gov"
# combining "Never-worked", "Without-pay" into "no_work_no_pay"
# combining "Self-emp-inc", "Self-emp-not-inc" into "Self-emp-inc_and_not-inc"
# "?" to "unknown"
AdultData_df$workClass = ifelse(
  AdultData_df$workClass == "Federal-gov" |
    AdultData_df$workClass == "Local-gov" |
    AdultData_df$workClass == "State-gov",
  "gov",
  ifelse(
    AdultData_df$workClass == "Never-worked" |
      AdultData_df$workClass == "Without-pay" ,
    "no_work_no_pay",
    ifelse(
      AdultData_df$workClass == "Self-emp-inc" |
        AdultData_df$workClass == "Self-emp-not-inc" ,
      "Self-emp-inc_and_not-inc",
      ifelse(
        AdultData_df$workClass == "?",
        "unknown",
        AdultData_df$workClass
      )
    )
  )
)
table(AdultData_df$workClass)
AdultData_df$workClass <- as.factor(AdultData_df$workClass)

AdultData_df$education <- str_trim(AdultData_df$education)
unique(AdultData_df$education)
AdultData_df$education = ifelse(
  AdultData_df$education == "11th" |
    AdultData_df$education == "9th" |
    AdultData_df$education == "7th-8th" |
    AdultData_df$education == "5th-6th" |
    AdultData_df$education == "10th" |
    AdultData_df$education == "1st-4th" |
    AdultData_df$education == "12th" |
    AdultData_df$education == "Preschool" |
    AdultData_df$education == "HS-grad",
  "schoolDropouts",
  ifelse(
    AdultData_df$education == "Assoc-acdm" |
      AdultData_df$education == "Assoc-voc" ,
    "assoc",
    AdultData_df$education
  )
)
unique(AdultData_df$education)
AdultData_df$education <- as.factor(AdultData_df$education)

AdultData_df$marital_status <-
  str_trim(AdultData_df$marital_status)
unique(AdultData_df$marital_status)
AdultData_df$marital_status = ifelse(
  AdultData_df$marital_status == "Divorced" |
    AdultData_df$marital_status == "Widowed" |
    AdultData_df$marital_status == "Separated",
  "Separated",
  ifelse(
    AdultData_df$marital_status == "Married-civ-spouse" |
      AdultData_df$marital_status == "Married-AF-spouse" |
      AdultData_df$marital_status == "Married-spouse-absent",
    "Married",
    AdultData_df$marital_status
  )
)
unique(AdultData_df$marital_status)
AdultData_df$marital_status <-
  as.factor(AdultData_df$marital_status)

AdultData_df$occupation <- str_trim(AdultData_df$occupation)
unique(AdultData_df$occupation)
AdultData_df$occupation <-
  ifelse(
    AdultData_df$occupation %in% c("?", "Other-service"),
    "Other Service",
    AdultData_df$occupation
  )
unique(AdultData_df$occupation)
AdultData_df$occupation <- as.factor(AdultData_df$occupation)

AdultData_df$relationshipStatus <-
  str_trim(AdultData_df$relationshipStatus)
unique(AdultData_df$relationshipStatus)
AdultData_df$relationshipStatus <-
  ifelse(
    AdultData_df$relationshipStatus %in%
      c("Not-in-family", "Unmarried", "Other-relative"),
    "Other relative",
    AdultData_df$relationshipStatus
  )
unique(AdultData_df$relationshipStatus)
AdultData_df$relationshipStatus <-
  as.factor(AdultData_df$relationshipStatus)

AdultData_df$race <- str_trim(AdultData_df$race)
unique(AdultData_df$race)
AdultData_df$race <- as.factor(AdultData_df$race)

AdultData_df$gender <- str_trim(AdultData_df$gender)
unique(AdultData_df$gender)
AdultData_df$gender <- as.factor(AdultData_df$gender)

# Select categorical column
factorVariables <- data.frame(select_if(AdultData_df, is.factor))
ncol(factorVariables)
names(factorVariables)

ggplot(factorVariables, aes(workClass)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(factorVariables, aes(education)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(factorVariables, aes(marital_status)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(factorVariables, aes(occupation)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(factorVariables, aes(relationshipStatus)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(factorVariables, aes(race)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(factorVariables, aes(gender)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))


#contingency table for the Marital Status
table(AdultData_df$marital_status)

# Marital Status vs income
ggplot(AdultData_df, aes(x = marital_status, fill = salaryRange)) +
  geom_bar(position = "fill") +
  theme_classic()

# gender vs income
ggplot(AdultData_df, aes(x = gender, fill = salaryRange)) +
  geom_bar(position = "fill") +
  theme_classic()

# race vs income
ggplot(AdultData_df, aes(x = race, fill = salaryRange)) +
  geom_bar(position = "fill") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(AdultData_df, aes(x = salaryRange, y = age)) + geom_boxplot()
ggplot(AdultData_df, aes(x = salaryRange, y = hours_per_week)) + geom_boxplot()

#############################################################################################
##########################TRAINING MODEL AND TESTING#########################################

## Response variable as 1 or 0
AdultData_df$salaryRange <- str_trim(AdultData_df$salaryRange)
AdultData_df$salaryRange = as.factor(ifelse(AdultData_df$salaryRange == ">50K", 1, 0))

AdultData_df1 <- AdultData_df %>%
  select(
    'age',
    'workClass',
    'education',
    'marital_status',
    'occupation',
    'relationshipStatus',
    'race',
    'gender',
    'capital_gain',
    'capital_loss',
    'hours_per_week'
  )

str(AdultData_df1)

unique(AdultData_df$salaryRange)

# dummify the data
dummyVariables <- dummyVars(" ~ .", data = AdultData_df1)
AdultData_df_new <-
  data.frame(predict(dummyVariables, newdata = AdultData_df1))
AdultData_df_new <-
  cbind(AdultData_df_new, AdultData_df[c("salaryRange")])

str(AdultData_df_new)

names(AdultData_df_new)

table(AdultData_df_new$salaryRange)

######################
set.seed(430)
splitIdx = createDataPartition(AdultData_df_new$salaryRange, p = 0.70, list = FALSE)
trainData_df = AdultData_df_new[splitIdx,]
testData_df = AdultData_df_new[-splitIdx,]

table(trainData_df$salaryRange)
table(testData_df$salaryRange)

modellr <- train(
  form = salaryRange ~ .,
  data = trainData_df,
  trControl = trainControl(method = "cv", number = 5),
  #five fold cross validation
  method = "glm",
  family = "binomial"
)

plot(varImp(modellr), main = "Variable Importance")

modellr
names(modellr)
modellr$results
modellr$finalModel
summary(modellr)

## will remove the race and workclass features then train again
AdultData_df1$workClass <- NULL
AdultData_df1$race <- NULL

# dummify the data
dummyVariables <- dummyVars(" ~ .", data = AdultData_df1)
AdultData_df_new <-
  data.frame(predict(dummyVariables, newdata = AdultData_df1))
AdultData_df_new <-
  cbind(AdultData_df_new, AdultData_df[c("salaryRange")])

str(AdultData_df_new)

names(AdultData_df_new)

table(AdultData_df_new$salaryRange)

######################
set.seed(430)
splitIdx = createDataPartition(AdultData_df_new$salaryRange, p = 0.70, list = FALSE)
trainData_df = AdultData_df_new[splitIdx,]
testData_df = AdultData_df_new[-splitIdx,]

table(trainData_df$salaryRange)
table(testData_df$salaryRange)

modellr <- train(
  form = salaryRange ~ .,
  data = trainData_df,
  trControl = trainControl(method = "cv", number = 5),
  #five fold cross validation
  method = "glm",
  family = "binomial"
)

summary(modellr)
plot(varImp(modellr))

outcomeName <- "salaryRange"
predictors <-
  c(
    "age",
    "education.assoc" ,
    "education.Bachelors",
    "education.Doctorate"      ,
    "education.Masters"     ,
    "education.Prof.school"   ,
    "education.schoolDropouts"   ,
    "education.Some.college"  ,
    "marital_status.Married"  ,
    "marital_status.Never.married"  ,
    "marital_status.Separated" ,
    "occupation.Adm.clerical"  ,
    "occupation.Armed.Forces"   ,
    "occupation.Craft.repair" ,
    "occupation.Exec.managerial" ,
    "occupation.Farming.fishing"  ,
    "occupation.Handlers.cleaners"  ,
    "occupation.Machine.op.inspct" ,
    "occupation.Other.Service"   ,
    "occupation.Priv.house.serv",
    "occupation.Prof.specialty" ,
    "occupation.Protective.serv",
    "occupation.Sales" ,
    "occupation.Tech.support" ,
    "occupation.Transport.moving" ,
    "relationshipStatus.Husband",
    "relationshipStatus.Other.relative",
    "relationshipStatus.Own.child" ,
    "relationshipStatus.Wife"     ,
    "gender.Female"        ,
    "gender.Male"     ,
    "capital_gain"    ,
    "capital_loss" ,
    "hours_per_week"
  )

#Predictions
predictions <-
  predict.train(object = modellr, testData_df[, predictors], type = "raw")
table(predictions) #predictions #0   1 #7904 1864
confusionMatrix(predictions, testData_df[, outcomeName])

head(predict(modellr, newdata = testData_df))
head(predict(modellr, newdata = testData_df, type = "prob"))


pred = predict(modellr, newdata = testData_df)
accuracy <- table(pred, testData_df[, "salaryRange"])
sum(diag(accuracy)) / sum(accuracy)
## [1] 0.8519656
confusionMatrix(data = pred, testData_df$salaryRange)

### predict on test dataset
lreg_pred <- predict(modellr, testData_df)
##results
confusionMatrix(lreg_pred, testData_df$salaryRange)


#### with this model we test on the adult.test dataset ####
#data preprocessing steps for the test data
# Reading the data
# header is false since the dataset is having no header row
AdultTestData_df <-
  read.csv("adult.test", header = FALSE, stringsAsFactors = FALSE)

# Analyzing the dataset
head(AdultTestData_df)
str(AdultTestData_df)

#16281 observations and 15 variables of which 14 are the predictors and one is the target or response

# Since no headers exist, we will rename all the columns
names(AdultTestData_df) <-
  c(
    'age',
    # the age of an individual
    'workClass',
    # the employment status of an individual
    'fnlwgt',
    #the number of people census believes the entry represents
    'education',
    # the highest level of an education acheived
    'education_num',
    # the highest level of an education acheived in numeric form
    'marital_status',
    # the marital status
    'occupation',
    # the generic type of an occupation
    'relationshipStatus',
    # how the individual is related
    'race',
    # the race
    'gender',
    # the gender
    'capital_gain',
    # the capital gain of an individual
    'capital_loss',
    # the capital loss of an individual
    'hours_per_week',
    # the number of hours per week, an individual has reported for work
    'native_country',
    # country of origin
    'salaryRange' # the response or target variable
  )

head(AdultTestData_df)
glimpse(AdultTestData_df)

#####  Univariate and BiVariate Data Analysis #####
# Step 1) Exploring the continuous variables, the function select_if() from the dplyr library
# will select  the numerical columns
continuousVariables <- select_if(AdultTestData_df, is.numeric)
continuousVariables$fnlwgt <- NULL
continuousVariables$education_num <- NULL
#five point summary
summary(continuousVariables)
# we can see different scales and outliers

#From the summary, we can observe the minimum value of age is 17 and maximum value is 90
# for the Capital gain, mostly the values are 0 with mean at 1082
# for the Capital loss, mostly values are 0 with mean at 87.9
# hours_per_week, the minimum value is 1 and maximum is 99


# Histogram with kernel density curve
ggplot(continuousVariables, aes(x = age)) + geom_density(alpha = .2, fill = "#FF6666")
#ggplot(continuousVariables, aes(x = fnlwgt)) + geom_density(alpha = .2, fill = "#FF6666")
#ggplot(continuousVariables, aes(x = education_num)) + geom_density(alpha = .2, fill = "#FF6666")
ggplot(continuousVariables, aes(x = capital_gain)) + geom_density(alpha = .2, fill = "#FF6666")
ggplot(continuousVariables, aes(x = capital_loss)) + geom_density(alpha = .2, fill = "#FF6666")
ggplot(continuousVariables, aes(x = hours_per_week)) + geom_density(alpha = .2, fill = "#FF6666")
#Outliers
#Distributions

## Binning the age variable in equal representation of 20
AdultTestData_df$ageCat <-
  cut(AdultTestData_df$age, breaks = seq(0, 100, 20))
head(AdultTestData_df[, c('age', 'ageCat')])

# ageCat vs income
ggplot(AdultTestData_df, aes(x = ageCat, fill = salaryRange)) +
  geom_bar(position = "fill") +
  theme_classic()

table(AdultTestData_df$ageCat)
AdultTestData_df$ageCat <- as.factor(AdultTestData_df$ageCat)

# Both capital gains and capital loss are right skewed hence we perform the log transformation
AdultTestData_df$logCapitalGain_v <-
  log(AdultTestData_df$capital_gain + 1)

#scaling the capital loss and capital gain
AdultTestData_df$scaledCapitalGain_v <-
  scale(AdultTestData_df$capital_gain)
AdultTestData_df$scaledCapitalLoss_v <-
  scale(AdultTestData_df$capital_loss)

AdultTestData_df$logCapitalLoss_v <-
  log(AdultTestData_df$capital_loss + 1)

## hours per week minimum is 1 and maximum is 100
## binning in 20 hours category
AdultTestData_df$hoursWeekCat <-
  cut(AdultTestData_df$hours_per_week, breaks = seq(0, 100, 20))
head(AdultTestData_df[, c('hours_per_week', 'hoursWeekCat')])

# hoursWeekCatCat vs income
ggplot(AdultTestData_df, aes(x = hoursWeekCat, fill = salaryRange)) +
  geom_bar(position = "fill") +
  theme_classic()

table(AdultTestData_df$hoursWeekCat)
AdultTestData_df$hoursWeekCat <-
  as.factor(AdultTestData_df$hoursWeekCat)

# will select  the numerical columns
continuousVariables <- select_if(AdultTestData_df, is.numeric)

# We will exclude fnlwgt from the dataframe
continuousVariables$fnlwgt <- NULL
# We will exclude the numerical form of the education and explore the categorical form
continuousVariables$education_num <- NULL

#five point summary
summary(continuousVariables)

#Step 2) Analyzing the string or factor variables
# trimming the white space
AdultTestData_df$workClass <- str_trim(AdultTestData_df$workClass)
unique(AdultTestData_df$workClass) #unique values of the column

# combining "Federal-gov","Local-gov","State-gov" into "gov"
# combining "Never-worked", "Without-pay" into "no_work_no_pay"
# combining "Self-emp-inc", "Self-emp-not-inc" into "Self-emp-inc_and_not-inc"
# "?" to "unknown"
AdultTestData_df$workClass = ifelse(
  AdultTestData_df$workClass == "Federal-gov" |
    AdultTestData_df$workClass == "Local-gov" |
    AdultTestData_df$workClass == "State-gov",
  "gov",
  ifelse(
    AdultTestData_df$workClass == "Never-worked" |
      AdultTestData_df$workClass == "Without-pay" ,
    "no_work_no_pay",
    ifelse(
      AdultTestData_df$workClass == "Self-emp-inc" |
        AdultTestData_df$workClass == "Self-emp-not-inc" ,
      "Self-emp-inc_and_not-inc",
      ifelse(
        AdultTestData_df$workClass == "?",
        "unknown",
        AdultTestData_df$workClass
      )
    )
  )
)
table(AdultTestData_df$workClass)
AdultTestData_df$workClass <- as.factor(AdultTestData_df$workClass)

AdultTestData_df$education <- str_trim(AdultTestData_df$education)
unique(AdultTestData_df$education)
AdultTestData_df$education = ifelse(
  AdultTestData_df$education == "11th" |
    AdultTestData_df$education == "9th" |
    AdultTestData_df$education == "7th-8th" |
    AdultTestData_df$education == "5th-6th" |
    AdultTestData_df$education == "10th" |
    AdultTestData_df$education == "1st-4th" |
    AdultTestData_df$education == "12th" |
    AdultTestData_df$education == "Preschool" |
    AdultTestData_df$education == "HS-grad",
  "schoolDropouts",
  ifelse(
    AdultTestData_df$education == "Assoc-acdm" |
      AdultTestData_df$education == "Assoc-voc" ,
    "assoc",
    AdultTestData_df$education
  )
)
unique(AdultTestData_df$education)
AdultTestData_df$education <- as.factor(AdultTestData_df$education)

AdultTestData_df$marital_status <-
  str_trim(AdultTestData_df$marital_status)
unique(AdultTestData_df$marital_status)
AdultTestData_df$marital_status = ifelse(
  AdultTestData_df$marital_status == "Divorced" |
    AdultTestData_df$marital_status == "Widowed" |
    AdultTestData_df$marital_status == "Separated",
  "Separated",
  ifelse(
    AdultTestData_df$marital_status == "Married-civ-spouse" |
      AdultTestData_df$marital_status == "Married-AF-spouse" |
      AdultTestData_df$marital_status == "Married-spouse-absent",
    "Married",
    AdultTestData_df$marital_status
  )
)
unique(AdultTestData_df$marital_status)
AdultTestData_df$marital_status <-
  as.factor(AdultTestData_df$marital_status)

AdultTestData_df$occupation <- str_trim(AdultTestData_df$occupation)
unique(AdultTestData_df$occupation)
AdultTestData_df$occupation <-
  ifelse(
    AdultTestData_df$occupation %in% c("?", "Other-service"),
    "Other Service",
    AdultTestData_df$occupation
  )
unique(AdultTestData_df$occupation)
AdultTestData_df$occupation <-
  as.factor(AdultTestData_df$occupation)

AdultTestData_df$relationshipStatus <-
  str_trim(AdultTestData_df$relationshipStatus)
unique(AdultTestData_df$relationshipStatus)
AdultTestData_df$relationshipStatus <-
  ifelse(
    AdultTestData_df$relationshipStatus %in%
      c("Not-in-family", "Unmarried", "Other-relative"),
    "Other relative",
    AdultTestData_df$relationshipStatus
  )
unique(AdultTestData_df$relationshipStatus)
AdultTestData_df$relationshipStatus <-
  as.factor(AdultTestData_df$relationshipStatus)

AdultTestData_df$race <- str_trim(AdultTestData_df$race)
unique(AdultTestData_df$race)
AdultTestData_df$race <- as.factor(AdultTestData_df$race)

AdultTestData_df$gender <- str_trim(AdultTestData_df$gender)
unique(AdultTestData_df$gender)
AdultTestData_df$gender <- as.factor(AdultTestData_df$gender)

# Select categorical column
factorVariables <-
  data.frame(select_if(AdultTestData_df, is.factor))
ncol(factorVariables)
names(factorVariables)

# Create graph for each column
graph <- lapply(names(factorVariables),
                function(x)
                  ggplot(factorVariables, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))

# Print the graph
graph

#contingency table for the Marital Status
table(AdultTestData_df$marital_status)

# Marital Status vs income
ggplot(AdultTestData_df, aes(x = marital_status, fill = salaryRange)) +
  geom_bar(position = "fill") +
  theme_classic()

# gender vs income
ggplot(AdultTestData_df, aes(x = gender, fill = salaryRange)) +
  geom_bar(position = "fill") +
  theme_classic()

# race vs income
ggplot(AdultTestData_df, aes(x = race, fill = salaryRange)) +
  geom_bar(position = "fill") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(AdultTestData_df, aes(x = salaryRange, y = age)) + geom_boxplot()
ggplot(AdultTestData_df, aes(x = salaryRange, y = hours_per_week)) + geom_boxplot()

## Response variable as 1 or 0
AdultTestData_df$salaryRange <-
  str_trim(AdultTestData_df$salaryRange)
AdultTestData_df$salaryRange = as.factor(ifelse(AdultTestData_df$salaryRange == ">50K.", 1, 0))

AdultTestData_df1 <- AdultTestData_df %>%
  select(
    'age',
    'workClass',
    'education',
    'marital_status',
    'occupation',
    'relationshipStatus',
    'race',
    'gender',
    'capital_gain',
    'capital_loss',
    'hours_per_week'
  )

str(AdultTestData_df1)

unique(AdultTestData_df$salaryRange)

## will remove the race and workclass features 
AdultTestData_df1$workClass <- NULL
AdultTestData_df1$race <- NULL

# dummify the data
dummyVariables <- dummyVars(" ~ .", data = AdultTestData_df1)
AdultDataTest_df_new <-
  data.frame(predict(dummyVariables, newdata = AdultTestData_df1))
AdultDataTest_df_new <-
  cbind(AdultDataTest_df_new, AdultTestData_df[c("salaryRange")])

str(AdultDataTest_df_new)

names(AdultDataTest_df_new)

table(AdultDataTest_df_new$salaryRange)

######################
set.seed(430)

outcomeName <- "salaryRange"
predictors <-
  c(
    "age",
    "education.assoc" ,
    "education.Bachelors",
    "education.Doctorate"      ,
    "education.Masters"     ,
    "education.Prof.school"   ,
    "education.schoolDropouts"   ,
    "education.Some.college"  ,
    "marital_status.Married"  ,
    "marital_status.Never.married"  ,
    "marital_status.Separated" ,
    "occupation.Adm.clerical"  ,
    "occupation.Armed.Forces"   ,
    "occupation.Craft.repair" ,
    "occupation.Exec.managerial" ,
    "occupation.Farming.fishing"  ,
    "occupation.Handlers.cleaners"  ,
    "occupation.Machine.op.inspct" ,
    "occupation.Other.Service"   ,
    "occupation.Priv.house.serv",
    "occupation.Prof.specialty" ,
    "occupation.Protective.serv",
    "occupation.Sales" ,
    "occupation.Tech.support" ,
    "occupation.Transport.moving" ,
    "relationshipStatus.Husband",
    "relationshipStatus.Other.relative",
    "relationshipStatus.Own.child" ,
    "relationshipStatus.Wife"     ,
    "gender.Female"        ,
    "gender.Male"     ,
    "capital_gain"    ,
    "capital_loss" ,
    "hours_per_week"
  )

#Predictions
predictions <-
  predict.train(object = modellr, AdultDataTest_df_new[, predictors], type =
                  "raw")
table(predictions) #predictions #0   1 #11581 2257
confusionMatrix(predictions, AdultDataTest_df_new[, outcomeName])

head(predict(modellr, newdata = AdultDataTest_df_new))
head(predict(modellr, newdata = AdultDataTest_df_new, type = "prob"))


pred = predict(modellr, newdata = AdultDataTest_df_new)
accuracy <- table(pred, AdultDataTest_df_new[, "salaryRange"])
sum(diag(accuracy)) / sum(accuracy)
## [1] 0.8499478
confusionMatrix(data = pred, AdultDataTest_df_new$salaryRange)

### predict on test dataset
lreg_pred <- predict(modellr, AdultDataTest_df_new)
##results
confusionMatrix(lreg_pred, AdultDataTest_df_new$salaryRange)
