library(tidyverse)
library(caret) # downSample() function lives here
library(e1071) # co-package for caret
library(naniar) # has miss_var graph
library(GGally)
library(ROSE)
library(glmnet)
library(corrplot)
library(gplots)
library(MASS)
library(ROCR)

## Input variables:

##   # bank client data:
## 1 - age (numeric)
## 2 - job (categorical: 'admin.','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown')
## 3 - marital (categorical: 'divorced','married','single','unknown'; note: 'divorced' means divorced or widowed)
## 4 - education (categorical: 'basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course','university.degree','unknown')
## 5 - default: has credit in default? (categorical: 'no','yes','unknown')
## 6 - housing: has housing loan? (categorical: 'no','yes','unknown')
## 7 - loan: has personal loan? (categorical: 'no','yes','unknown')
## 8 - contact: contact communication type (categorical: 'cellular','telephone')
## 9 - month: last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')
## 10 - day_of_week: last contact day of the week (categorical: 'mon','tue','wed','thu','fri')
## 11 - duration: last contact duration, in seconds (numeric). 

# Important note: this attribute highly affects the output target (e.g., if duration=0 then y='no'). 
# Yet, the duration is not known before a call is performed. 
# Also, after the end of the call y is obviously known. 
# Thus, this input should only be included for benchmark purposes and 
# should be discarded if the intention is to have a realistic predictive model.

## # other attributes:
## 12 - campaign (numeric, includes last contact)
## 13 - pdays: (numeric; 999 means client was not previously contacted)
## 14 - previous: number of contacts performed before this campaign and for this client (numeric)
## 15 - poutcome: (categorical: 'failure','nonexistent','success')


## # social and economic context attributes
## 16 - emp.var.rate: employment variation rate - quarterly indicator (numeric)
## 17 - cons.price.idx: consumer price index - monthly indicator (numeric)
## 18 - cons.conf.idx: consumer confidence index - monthly indicator (numeric)
## 19 - euribor3m: euribor 3 month rate - daily indicator (numeric)
## 20 - nr.employed: number of employees - quarterly indicator (numeric)

## Output variable (desired target):
## 21 - y - has the client subscribed a term deposit? (binary: 'yes','no')


# categorical c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15)

# numeric c(1, 11, 12, 13, 14, 16, 17, 18, 19, 20)

mrkt_raw <- read.csv("data/bank-additional-full.csv", sep=";")

dim(mrkt_raw)

names(mrkt_raw)

head(mrkt_raw)

# Plot missing values, none found in the data set
gg_miss_var(mrkt_raw)

summary(mrkt_raw)


# Converting all of the character columns to the rightful factor type

mrkt_raw[sapply(mrkt_raw, is.character)] <- 
  lapply(mrkt_raw[sapply(mrkt_raw, is.character)], as.factor)

# Looks like maybe the previous col should be converted as well, it is
# a few ints

str(mrkt_raw)


# summary stats of jobs by age

t(aggregate(age~job,data=mrkt_raw,summary))


# Boxplot of age range by jobs

mrkt_raw %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = job,
      y = age,
      color = y
    )
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )


# Summary histogram with job counts, also shows response

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(job,job,function(x) -length(x)),
      color = y
    ),
    position = "dodge",
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )



# Summary histogram with martial counts, also shows response

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(marital,marital,function(x) -length(x)),
      color = y
    ),
    position = "dodge",
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )


# Summary histogram with education counts, also shows response

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(education,education,function(x) -length(x)),
      color = y
    ),
    position = "dodge",
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )


# Summary boxplots with age by response

mrkt_raw %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = y,
      y = age,
      color = y
    )
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )


# Exploring categorical 

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(job,job,function(x) -length(x))
    ),
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(marital,marital,function(x) -length(x))
    ),
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(education,education,function(x) -length(x))
    ),
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(default,default,function(x) -length(x))
    ),
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

# They hadn't contacted anyone that's previously defaulted on a loan

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(housing,housing,function(x) -length(x))
    ),
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(loan,loan,function(x) -length(x))
    ),
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(contact,contact,function(x) -length(x))
    ),
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(month,month,function(x) -length(x))
    ),
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

# May is the busiest month by far, dec is practially none  

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(day_of_week,day_of_week,function(x) -length(x))
    ),
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )



# Exploring the numeric variables

# numeric c(1, 11, 12, 13, 14, 16, 17, 18, 19, 20)

# Seems like on the age and duration are

# These cols aren't really (that) numeric, has few unique values on 40000 obs, output below:

ggpairs(mrkt_raw[,c(1,11,21)], mapping=aes(color=mrkt_raw[,c(21)]))

unique(mrkt_raw[,c(11)]) ## 11 - duration

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(as.factor(duration),as.factor(duration),function(x) -length(x)),
      color = y
    ),
    position = "dodge",
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )


unique(mrkt_raw[,c(12)]) ## 12 - campaign (numeric, includes last contact)

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(as.factor(campaign),as.factor(campaign),function(x) -length(x)),
      color = y
    ),
    position = "dodge",
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 19 18 23 14 22 25 16 17 15 20 
##       56 39 35 42 28 26 27 32 21 24 29 31 30 41 37 40 33 34 43



unique(mrkt_raw[,c(13)]) ## 13 - pdays: (numeric; 999 means client was not previously contacted)

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(as.factor(pdays),as.factor(pdays),function(x) -length(x)),
      color = y
    ),
    position = "dodge",
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )


##  [1] 999   6   4   3   5   1   0  10   7   8   9  11   2  12  13  14  15  16  
##      21  17  18  22  25  26  19  27  20

unique(mrkt_raw[,c(14)]) ## 14 - previous: (numeric)

## [1] 0 1 2 3 4 5 6 7

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = reorder(as.factor(previous),as.factor(previous),function(x) -length(x)),
      color = y
    ),
    position = "dodge",
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

unique(mrkt_raw[,c(16)]) ## 16 - emp.var.rate: employment variation rate - quarterly indicator (numeric)

## [1]  1.1  1.4 -0.1 -0.2 -1.8 -2.9 -3.4 -3.0 -1.7 -1.1

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = as.factor(emp.var.rate),
      color = y
    ),
    position = "dodge",
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

unique(mrkt_raw[,c(17)]) ## cons.price.idx: consumer price index - monthly indicator (numeric)

## A consumer price index measures changes in the price level of a weighted average market basket of consumer goods and services purchased by households (From WIKI)

## [1] 93.994 94.465 93.918 93.444 93.798 93.200 92.756 92.843 93.075 92.893 92.963 92.469 92.201 92.379 
##     92.431 92.649 92.713 93.369 93.749 93.876 94.055 94.215 94.027 94.199 94.601 94.767

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = as.factor(cons.price.idx),
      color = y
    ),
    position = "dodge",
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

unique(mrkt_raw[,c(18)]) ## 18 - cons.conf.idx: consumer confidence index - monthly indicator (numeric)

## the degree of optimism on the state of the U.S. economy that consumers are expressing through their activities of savings and spending (FROM WIKI)

## [1] -36.4 -41.8 -42.7 -36.1 -40.4 -42.0 -45.9 -50.0 -47.1 -46.2 -40.8 -33.6 -31.4 -29.8 -26.9 -30.1 
##     -33.0 -34.8 -34.6 -40.0 -39.8 -40.3 -38.3 -37.5 -49.5 -50.8


mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = as.factor(cons.conf.idx),
      color = y
    ),
    position = "dodge",
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )


unique(mrkt_raw[,c(19)]) ## 19 - euribor3m: euribor 3 month rate - daily indicator (numeric)

# This is about 300 different numbers, I would think that this is continuous

## The 3 month Euribor interest rate is the interest rate at which a panel of banks lend money to one another with a maturity of 3 months.

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = as.factor(euribor3m),
      color = y
    ),
    position = "dodge",
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)
  )


unique(mrkt_raw[,c(20)]) ## 20 - nr.employed: number of employees - quarterly indicator (numeric)

## [1] 5191.0 5228.1 5195.8 5176.3 5099.1 5076.2 5017.5 5023.5 5008.7 4991.6 4963.6

mrkt_raw %>%
  ggplot() +
  geom_histogram(
    aes(
      x = as.factor(nr.employed),
      color = y
    ),
    position = "dodge",
    stat="count"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)
  )


## I'm going to try to convert these columns to factors for modelling c(12,13,14,16,20)
## These are basically what I would think are this company specific, not the indexes

# mrkt_raw$campaign <- as.factor(mrkt_raw$campaign)
# 
# mrkt_raw$pdays <- as.factor(mrkt_raw$pdays)
# 
# mrkt_raw$previous <- as.factor(mrkt_raw$previous)
# 
# mrkt_raw$emp.var.rate <- as.factor(mrkt_raw$emp.var.rate)
# 
# mrkt_raw$nr.employed <- as.factor(mrkt_raw$nr.employed)


## Exploring the response variable to get a sense of what are percentage splits are

percentYeses <- dim(mrkt_raw %>% filter(mrkt_raw[,21] == 'yes'))[1]/dim(mrkt_raw)[1] * 100

percentNos <- dim(mrkt_raw %>% filter(mrkt_raw[,21] == 'no'))[1]/dim(mrkt_raw)[1] * 100

percentCheck <- percentYeses + percentNos

# There are roughly 11.3% yeses total, 88.7% nos

## Try out downsampling of the data
## Found a package called ROSE that might be better than downsampling

set.seed(1234)

#mrkt_idx <- 
#  sample(
#    1:dim(mrkt_raw)[1],
#    round(dim(mrkt_raw)[1] * 0.7),
#    replace=F
#  )

mrkt_idx <- createDataPartition(mrkt_raw$y, p=0.8, list = F) 


mrkt_train <- mrkt_raw[mrkt_idx,]
mrkt_test <- mrkt_raw[-mrkt_idx,]




## Checking the balance of the classes, they remain close to the original data

dim(mrkt_train %>% filter(mrkt_train[,21] == 'yes'))[1]/dim(mrkt_train)[1] * 100

dim(mrkt_train %>% filter(mrkt_train[,21] == 'no'))[1]/dim(mrkt_train)[1] * 100

dim(mrkt_test %>% filter(mrkt_test[,21] == 'yes'))[1]/dim(mrkt_test)[1] * 100

dim(mrkt_test %>% filter(mrkt_test[,21] == 'no'))[1]/dim(mrkt_test)[1] * 100



## Some downsampling, I couldn't get it to work on the prediction, will debug

 mrkt_downSample <- downSample(x = mrkt_train[, -ncol(mrkt_train)],
                          y = mrkt_train$y)
 
 

 
 mrkt_log_ds <- glm(mrkt_downSample$Class ~ ., data=mrkt_downSample, family=binomial)
# 
# mrkt_log_ds.pred <- predict(mrkt_log_ds, newdata=mrkt_test, type="response")
# 
mrkt_rose <- ROSE(y~., mrkt_train, seed=22)$data

# 
# mrkt_log_rose <- glm(y ~ ., data=mrkt_rose, family=binomial)
# 
# mrkt_log_rose.pred <- predict(mrkt_log_rose, newdata=mrkt_test, type="response")


mrkt_logit <- glm(mrkt_raw$y ~ ., data=mrkt_raw, family=binomial)

## ~~~~

mrkt_logit_age <- glm(mrkt_raw$y ~ age, data=mrkt_raw, family=binomial)

summary(mrkt_logit_age)

## ~~~~

mrkt_logit_job <- glm(mrkt_raw$y ~ job, data=mrkt_raw, family=binomial)

summary(mrkt_logit_job)

## ~~~~

mrkt_logit_marital <- glm(mrkt_raw$y ~ marital, data=mrkt_raw, family=binomial)

summary(mrkt_logit_marital)

## ~~~~

mrkt_logit_education <- glm(mrkt_raw$y ~ education, data=mrkt_raw, family=binomial)

summary(mrkt_logit_education)

## ~~~~

mrkt_logit_default <- glm(mrkt_raw$y ~ default, data=mrkt_raw, family=binomial)

summary(mrkt_logit_default)

## ~~~~

mrkt_logit_housing <- glm(mrkt_raw$y ~ housing, data=mrkt_raw, family=binomial)

summary(mrkt_logit_housing)

## ~~~~

mrkt_logit_loan <- glm(mrkt_raw$y ~ loan, data=mrkt_raw, family=binomial)

summary(mrkt_logit_loan)

## ~~~~

mrkt_logit_contact <- glm(mrkt_raw$y ~ contact, data=mrkt_raw, family=binomial)

summary(mrkt_logit_contact)

## ~~~~

mrkt_logit_month <- glm(mrkt_raw$y ~ month, data=mrkt_raw, family=binomial)

summary(mrkt_logit_month)

## ~~~~

mrkt_logit_day_of_week <- glm(mrkt_raw$y ~ day_of_week, data=mrkt_raw, family=binomial)

summary(mrkt_logit_day_of_week)

## ~~~~

mrkt_logit_duration <- glm(mrkt_raw$y ~ duration, data=mrkt_raw, family=binomial)

summary(mrkt_logit_duration)

## ~~~~

mrkt_logit_campaign <- glm(mrkt_raw$y ~ campaign, data=mrkt_raw, family=binomial)

summary(mrkt_logit_campaign)

## ~~~~

mrkt_logit_pdays <- glm(mrkt_raw$y ~ pdays, data=mrkt_raw, family=binomial)

summary(mrkt_logit_pdays)

## ~~~~

mrkt_logit_previous <- glm(mrkt_raw$y ~ previous, data=mrkt_raw, family=binomial)

summary(mrkt_logit_previous)

## ~~~~

mrkt_logit_poutcome <- glm(mrkt_raw$y ~ poutcome, data=mrkt_raw, family=binomial)

summary(mrkt_logit_poutcome)

## ~~~~

mrkt_logit_emp.var.rate <- glm(mrkt_raw$y ~ emp.var.rate, data=mrkt_raw, family=binomial)

summary(mrkt_logit_emp.var.rate)

## ~~~~

mrkt_logit_cons.price.idx <- glm(mrkt_raw$y ~ cons.price.idx, data=mrkt_raw, family=binomial)

summary(mrkt_logit_cons.price.idx)

## ~~~~

mrkt_logit_cons.conf.idx <- glm(mrkt_raw$y ~ cons.conf.idx, data=mrkt_raw, family=binomial)

summary(mrkt_logit_cons.conf.idx)

## ~~~~

mrkt_logit_euribor3m <- glm(mrkt_raw$y ~ euribor3m, data=mrkt_raw, family=binomial)

summary(mrkt_logit_euribor3m)

## ~~~~

mrkt_logit_nr.employed <- glm(mrkt_raw$y ~ nr.employed, data=mrkt_raw, family=binomial)

summary(mrkt_logit_nr.employed)

## ~~~~


mrkt_logit_build <- 
  glm(y ~ 
        age + 
        job + 
        marital + 
        education + 
        default + 
        contact + 
        month + 
        duration + 
        campaign +
        pdays +
        previous +
        poutcome +
        emp.var.rate +
        cons.price.idx +
        cons.conf.idx +
        euribor3m +
        nr.employed, 
      data=mrkt_raw, 
      family=binomial)

summary(mrkt_logit_build)


## ~~~~

mrkt_logit_build2 <- 
  glm(y ~ 
        age + 
        job + 
        marital + 
        default + 
        contact + 
        month + 
        duration + 
        campaign +
        pdays +
        previous+
        poutcome +
        emp.var.rate +
        cons.price.idx +
        cons.conf.idx +
        euribor3m +
        nr.employed, 
      data=mrkt_raw, 
      family=binomial)

summary(mrkt_logit_build2)

# Dropped education

## ~~~~

mrkt_logit_build3 <- 
  glm(y ~ 
        contact + 
        duration + 
        campaign +
        pdays +
        poutcome +
        emp.var.rate +
        cons.price.idx +
        cons.conf.idx, 
      data=mrkt_raw, 
      family=binomial)

summary(mrkt_logit_build3)

## ~~~~

mrkt_logit_build5 <- 
  glm(y ~ 
        contact + 
        duration +
        poutcome +
        cons.price.idx +
        cons.conf.idx, 
      data=mrkt_raw, 
      family=binomial)

summary(mrkt_logit_build5)

## ~~~~


mrkt_logit_build_tr <- 
  glm(y ~ 
        age + 
        job + 
        marital + 
        education + 
        default + 
        contact + 
        month + 
        duration + 
        campaign +
        pdays +
        previous +
        poutcome +
        emp.var.rate +
        cons.price.idx +
        cons.conf.idx +
        euribor3m +
        nr.employed, 
      data=mrkt_train, 
      family=binomial)

 #summary(mrkt_logit_build)

## ~~~~

mrkt_logit_build2_tr <- 
  glm(y ~ 
        age + 
        job + 
        marital + 
        default + 
        contact + 
        month + 
        duration + 
        campaign +
        pdays +
        previous+
        poutcome +
        emp.var.rate +
        cons.price.idx +
        cons.conf.idx +
        euribor3m +
        nr.employed, 
      data=mrkt_train, 
      family=binomial)

mrkt_logit_build2_tr2 <- 
  glm(y ~ 
        job + 
        marital + 
        default + 
        contact + 
        month + 
        duration + 
        campaign +
        pdays +
        poutcome +
        emp.var.rate +
        cons.price.idx +
        cons.conf.idx +
        euribor3m +
        nr.employed, 
      data=mrkt_train, 
      family=binomial)

mrkt_logit_build2_tr3 <- 
  glm(y ~ 
        job + 
        marital + 
        default + 
        contact + 
        month + 
        duration + 
        campaign +
        pdays +
        poutcome +
        emp.var.rate +
        cons.price.idx +
        cons.conf.idx +
        euribor3m +
        nr.employed, 
      data=mrkt_train, 
      family=binomial)


summary(mrkt_logit_build2_tr3)

## ~~~~

mrkt_logit_build2_tr4 <- 
  glm(y ~ 
        default + 
        contact + 
        month + 
        duration + 
        campaign +
        pdays +
        poutcome +
        emp.var.rate +
        cons.price.idx +
        cons.conf.idx +
        euribor3m +
        nr.employed, 
      data=mrkt_train, 
      family=binomial)


summary(mrkt_logit_build2_tr4)

## ~~~~

mrkt_logit_build2_tr5 <- 
  glm(y ~ 
        default + 
        contact + 
        month + 
        duration + 
        campaign +
        pdays +
        poutcome +
        emp.var.rate +
        cons.price.idx +
        cons.conf.idx +
        euribor3m, 
      data=mrkt_train, 
      family=binomial)


summary(mrkt_logit_build2_tr5)


## ~~~~

mrkt_logit_build2_tr6 <- 
  glm(Class ~ 
        default + 
        contact + 
        month + 
        duration + 
        campaign +
        pdays +
        poutcome +
        emp.var.rate +
        cons.price.idx +
        cons.conf.idx +
        euribor3m, 
      data=mrkt_downSample, 
      family=binomial)


summary(mrkt_logit_build2_tr6)


## ~~~~

mrkt_logit_build2_tr7 <- 
  glm(Class ~ 
        default + 
        contact + 
        month + 
        duration + 
        campaign +
        pdays +
        poutcome +
        emp.var.rate +
        cons.price.idx +
        euribor3m, 
      data=mrkt_downSample, 
      family=binomial)


summary(mrkt_logit_build2_tr7)

## ~~~~

mrkt_logit_build2_tr8 <- 
  glm(Class ~ 
        default +
        month + 
        duration + 
        campaign +
        pdays +
        poutcome +
        emp.var.rate +
        cons.price.idx +
        euribor3m, 
      data=mrkt_downSample, 
      family=binomial)


summary(mrkt_logit_build2_tr8)

## ~~~~

mrkt_logit_build2_tr9 <- 
  glm(Class ~ 
        default +
        month + 
        duration + 
        pdays +
        poutcome +
        emp.var.rate +
        cons.price.idx +
        euribor3m, 
      data=mrkt_downSample, 
      family=binomial)


summary(mrkt_logit_build2_tr9)

## ~~~~

mrkt_logit_build2_trRose <- glm(y ~ 
                       age + 
                       job + 
                       marital + 
                       default + 
                       contact + 
                       month + 
                       duration + 
                       campaign +
                       pdays +
                       previous+
                       poutcome +
                       emp.var.rate +
                       cons.price.idx +
                       cons.conf.idx +
                       euribor3m +
                       nr.employed, data=mrkt_rose, family=binomial)

mrkt_log_rose.pred <- predict(mrkt_log_rose, newdata=mrkt_test, type="response")

## ~~~~

mrkt_logit_build3_tr <- 
  glm(y ~ 
        contact + 
        duration + 
        campaign +
        pdays +
        poutcome +
        emp.var.rate +
        cons.price.idx +
        cons.conf.idx, 
      data=mrkt_train, 
      family=binomial)

# summary(mrkt_logit_build3)

## ~~~~

mrkt_logit_build4_tr <- 
  glm(y ~ ., 
      data=mrkt_train, 
      family=binomial)

## ~~~~

mrkt_logit_build_tr5 <- 
  glm(y ~ 
        contact + 
        duration +
        poutcome +
        cons.price.idx +
        cons.conf.idx, 
      data=mrkt_train, 
      family=binomial)

# summary(mrkt_logit_build5)

## ~~~~

mrkt_logit_build_tr5DS <- 
  glm(Class ~ 
        contact + 
        duration +
        poutcome +
        cons.price.idx +
        cons.conf.idx, 
      data=mrkt_downSample, 
      family=binomial)

# summary(mrkt_logit_build5)

## ~~~~

mrkt_logit_build_tr5DSinter <- 
  glm(Class ~ 
        contact * 
        duration *
        poutcome *
        cons.price.idx *
        cons.conf.idx, 
      data=mrkt_downSample, 
      family=binomial)

summary(mrkt_logit_build_tr5DSinter)

## ~~~~



mrkt_logit.pred <- predict(mrkt_logit_build_tr, newdata=mrkt_test, type="response")

cutoff<-0.5

class.mrkt_logit.pred<-factor(ifelse(mrkt_logit.pred>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred<-table(class.mrkt_logit.pred,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred

sum(diag(conf.mrkt_logit.pred))/sum(conf.mrkt_logit.pred)

 #conf.mrkt_logit.pred[3]/conf.mrkt_logit.pred[4] * 100

## ~~~~

mrkt_logit.pred2 <- predict(mrkt_logit_build2_tr, newdata=mrkt_test, type="response")

class.mrkt_logit.pred2<-factor(ifelse(mrkt_logit.pred2>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred2<-table(class.mrkt_logit.pred2,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred2

sum(diag(conf.mrkt_logit.pred2))/sum(conf.mrkt_logit.pred2)

## ~~~~

mrkt_logit.pred22 <- predict(mrkt_logit_build2_tr2, newdata=mrkt_test, type="response")

class.mrkt_logit.pred22<-factor(ifelse(mrkt_logit.pred22>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred22<-table(class.mrkt_logit.pred22,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred22

sum(diag(conf.mrkt_logit.pred22))/sum(conf.mrkt_logit.pred22)

## ~~~~

mrkt_logit.pred23 <- predict(mrkt_logit_build2_tr3, newdata=mrkt_test, type="response")

class.mrkt_logit.pred23<-factor(ifelse(mrkt_logit.pred23>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred23<-table(class.mrkt_logit.pred23,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred23

sum(diag(conf.mrkt_logit.pred23))/sum(conf.mrkt_logit.pred23)

## ~~~~

mrkt_logit.pred23 <- predict(mrkt_logit_build2_tr3, newdata=mrkt_test, type="response")

class.mrkt_logit.pred23<-factor(ifelse(mrkt_logit.pred23>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred23<-table(class.mrkt_logit.pred23,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred23

sum(diag(conf.mrkt_logit.pred23))/sum(conf.mrkt_logit.pred23)

## ~~~~

####################################

mrkt_logit.pred24 <- predict(mrkt_logit_build2_tr4, newdata=mrkt_test, type="response")

class.mrkt_logit.pred24<-factor(ifelse(mrkt_logit.pred24>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred24<-table(class.mrkt_logit.pred24,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred24

sum(diag(conf.mrkt_logit.pred24))/sum(conf.mrkt_logit.pred24)

####################################

## ~~~~

mrkt_logit.pred25 <- predict(mrkt_logit_build2_tr5, newdata=mrkt_test, type="response")

class.mrkt_logit.pred25<-factor(ifelse(mrkt_logit.pred25>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred25<-table(class.mrkt_logit.pred25,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred25

sum(diag(conf.mrkt_logit.pred25))/sum(conf.mrkt_logit.pred25)

## ~~~~

mrkt_logit.pred26 <- predict(mrkt_logit_build2_tr6, newdata=mrkt_test, type="response")

class.mrkt_logit.pred26<-factor(ifelse(mrkt_logit.pred26>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred26<-table(class.mrkt_logit.pred26,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred26

sum(diag(conf.mrkt_logit.pred26))/sum(conf.mrkt_logit.pred26)

## ~~~~

mrkt_logit.pred27 <- predict(mrkt_logit_build2_tr7, newdata=mrkt_test, type="response")

class.mrkt_logit.pred27<-factor(ifelse(mrkt_logit.pred27>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred27<-table(class.mrkt_logit.pred27,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred27

sum(diag(conf.mrkt_logit.pred27))/sum(conf.mrkt_logit.pred27)

## ~~~~

mrkt_logit.pred28 <- predict(mrkt_logit_build2_tr8, newdata=mrkt_test, type="response")

class.mrkt_logit.pred28<-factor(ifelse(mrkt_logit.pred28>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred28<-table(class.mrkt_logit.pred28,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred28

sum(diag(conf.mrkt_logit.pred28))/sum(conf.mrkt_logit.pred28)

## ~~~~

mrkt_logit.pred29 <- predict(mrkt_logit_build2_tr9, newdata=mrkt_test, type="response")

class.mrkt_logit.pred29<-factor(ifelse(mrkt_logit.pred29>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred29<-table(class.mrkt_logit.pred29,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred29

sum(diag(conf.mrkt_logit.pred29))/sum(conf.mrkt_logit.pred29)

## ~~~~

mrkt_logit.pred2Rose <- predict(mrkt_logit_build2_trRose, newdata=mrkt_test, type="response")

class.mrkt_logit.pred2Rose<-factor(ifelse(mrkt_logit.pred2Rose>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred2Rose<-table(class.mrkt_logit.pred2Rose,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred2Rose

sum(diag(conf.mrkt_logit.pred2Rose))/sum(conf.mrkt_logit.pred2Rose)

## ~~~~


mrkt_logit.pred3 <- predict(mrkt_logit_build3_tr, newdata=mrkt_test, type="response")

class.mrkt_logit.pred3<-factor(ifelse(mrkt_logit.pred3>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred3<-table(class.mrkt_logit.pred3,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred3

sum(diag(conf.mrkt_logit.pred3))/sum(conf.mrkt_logit.pred3)


## ~~~~

mrkt_logit.pred4 <- predict(mrkt_logit_build4_tr, newdata=mrkt_test, type="response")

class.mrkt_logit.pred4<-factor(ifelse(mrkt_logit.pred4>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred4<-table(class.mrkt_logit.pred4,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred4

sum(diag(conf.mrkt_logit.pred4))/sum(conf.mrkt_logit.pred4)

## ~~~~

mrkt_logit.pred5 <- predict(mrkt_logit_build_tr5, newdata=mrkt_test, type="response")

class.mrkt_logit.pred5<-factor(ifelse(mrkt_logit.pred5>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred5<-table(class.mrkt_logit.pred5,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred5

sum(diag(conf.mrkt_logit.pred5))/sum(conf.mrkt_logit.pred5)

#conf.mrkt_logit.pred[3]/conf.mrkt_logit.pred[4] * 100

## ~~~~

mrkt_logit.pred5DS <- predict(mrkt_logit_build_tr5DS, newdata=mrkt_test, type="response")

class.mrkt_logit.pred5DS<-factor(ifelse(mrkt_logit.pred5DS>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred5DS<-table(class.mrkt_logit.pred5DS,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred5DS

sum(diag(conf.mrkt_logit.pred5DS))/sum(conf.mrkt_logit.pred5DS)

## ~~~~

mrkt_logit.pred5DSinter <- predict(mrkt_logit_build_tr5DSinter, newdata=mrkt_test, type="response")

class.mrkt_logit.pred5DSinter<-factor(ifelse(mrkt_logit.pred5DSinter>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_logit.pred5DSinter<-table(class.mrkt_logit.pred5DSinter,mrkt_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_logit.pred5DSinter

sum(diag(conf.mrkt_logit.pred5DSinter))/sum(conf.mrkt_logit.pred5DSinter)

## ~~~~

roc.curve(mrkt_test$y, mrkt_logit.pred)

roc.curve(mrkt_test$y, mrkt_logit.pred2, add.roc=TRUE, col=2)

roc.curve(mrkt_test$y, mrkt_logit.pred3, add.roc=TRUE, col=3)

roc.curve(mrkt_test$y, mrkt_logit.pred4, add.roc=TRUE, col=4)

roc.curve(mrkt_test$y, mrkt_logit.pred5, add.roc=TRUE, col= 5)

dat.train.x <- model.matrix(y~., mrkt_train[-c(8, 11, 12, 13, 15, 16, 17, 18)])
dat.train.y <- mrkt_train[,c(21)]

## CAREFUL of the nlambda, I was crashing R with 100

cvfit <- cv.glmnet(dat.train.x, dat.train.y, family = "binomial", type.measure = "class", nlambda = 10)

plot(cvfit)

coef(cvfit, s = "lambda.min")

#CV misclassification error rate is little below .1

print("CV Error Rate:")

cvfit$cvm[which(cvfit$lambda==cvfit$lambda.min)]

#Optimal penalty
print("Penalty Value:")
cvfit$lambda.min


finalmodel<-glmnet(dat.train.x, dat.train.y, family = "binomial",lambda=cvfit$lambda.min)

dat.test.x<-model.matrix(y~., mrkt_test[-c(8, 11, 12, 13, 15, 16, 17, 18)])

fit.pred.lasso <- predict(finalmodel, newx = dat.test.x, type = "response")

# IF IMBALANCED CONSIDER CHANGE CUTOFF

cutoff<-0.5

class.lasso<-factor(ifelse(fit.pred.lasso>cutoff,"yes","no"),levels=c("no","yes"))


#Confusion Matrix for Lasso

conf.lasso<-table(class.lasso,mrkt_test$y)
print("Confusion matrix for LASSO")
conf.lasso

print("Overall accuracy for LASSO and Stepwise respectively")
sum(diag(conf.lasso))/sum(conf.lasso)


# Correlation Matrix of "numerics"

# correlationMatrix <- cor(mrkt_raw[,c(1,11,12,14,16,17,18,19,20)], use="pairwise.complete.obs")
# corrplot(correlationMatrix, tl.cex = 0.5)




pc.result<-prcomp(mrkt_raw[,c(1,11,12,14,16,17,18,19,20)],scale.=TRUE)
pc.scores<-pc.result$x
pc.scores<-data.frame(pc.scores)
pc.scores$y<-as.factor(mrkt_raw$y)

#Use ggplot2 to plot the first few pc's
#ggplot(data = pc.scores, aes(x = PC1, y = PC2)) +
#  geom_point(aes(col=y), size=1)+
#  ggtitle("PCA of Auto")

ggplot(data = pc.scores, aes(x = PC1, y = PC3)) +
  geom_point(aes(col=y), size=1)+
  ggtitle("PCA of Auto")

#ggplot(data = pc.scores, aes(x = PC1, y = PC4)) +
#  geom_point(aes(col=y), size=1)+
#  ggtitle("PCA of Auto")

# ggplot(data = pc.scores, aes(x = PC1, y = PC5)) +
#   geom_point(aes(col=y), size=1)+
#   ggtitle("PCA of Auto")

ggplot(data = pc.scores, aes(x = PC2, y = PC3)) +
  geom_point(aes(col=y), size=1)+
  ggtitle("PCA of Auto")


#ggplot(data = pc.scores, aes(x = PC2, y = PC4)) +
#  geom_point(aes(col=y), size=1)+
#  ggtitle("PCA of Auto")


#ggplot(data = pc.scores, aes(x = PC2, y = PC5)) +
#  geom_point(aes(col=y), size=1)+
#  ggtitle("PCA of Auto")

ggplot(data = pc.scores, aes(x = PC3, y = PC4)) +
  geom_point(aes(col=y), size=1)+
  ggtitle("PCA of Auto")

#ggplot(data = pc.scores, aes(x = PC3, y = PC5)) +
#  geom_point(aes(col=y), size=1)+
#  ggtitle("PCA of Auto")

#ggplot(data = pc.scores, aes(x = PC4, y = PC5)) +
#  geom_point(aes(col=y), size=1)+
#  ggtitle("PCA of Auto")



eigenvals<-(pc.result$sdev)^2
plot(1:9,eigenvals/sum(eigenvals),type="l",main="Scree Plot PC's",ylab="Prop. Var. Explained",ylim=c(0,1))
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
lines(1:9,cumulative.prop,lty=2)

set.seed(1234)

pca_idx <- createDataPartition(pc.scores$y, p=0.8, list = F)

pca_train <- pc.scores[pca_idx,]

pca_test <- pc.scores[-pca_idx,]

pca_downSample <- downSample(x = pca_train[, -ncol(pca_train)],
                              y = pca_train$y)



# pca_lda_ds <- 
#   lda(Class ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = pca_downSample)
# 
# pca_lda.predict <- predict(pca_lda_ds, newdata = pca_test, type="response")

pca_lda <- 
  lda(Class ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = pca_downSample)

pca_lda.predict <- predict(pca_lda, newdata = pca_test, type="response")


pca_lda.predict.posteriors <- as.data.frame(pca_lda.predict$posterior)

# Evaluate the model
pred <- prediction(pca_lda.predict.posteriors[,2], pca_test$y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

cutoff <- 0.5

class.mrkt_pca.pred<-factor(ifelse(pca_lda.predict.posteriors[,2]>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_pca<-table(class.mrkt_pca.pred,pca_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_pca

sum(diag(conf.mrkt_pca))/sum(conf.mrkt_pca)


pca_qda_ds <- 
  qda(Class ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = pca_downSample)

pca_qda.predict <- predict(pca_qda_ds, newdata = pca_test, type="response")


pca_qda.predict.posteriors <- as.data.frame(pca_qda.predict$posterior)

# Evaluate the model
pred <- prediction(pca_qda.predict.posteriors[,2], pca_test$y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

# Balancing act of overall accuracy and sensitivity/specificity

cutoff <- 0.88

class.mrkt_pca.pred<-factor(ifelse(pca_qda.predict.posteriors[,2]>cutoff,"yes","no"),levels=c("no","yes"))


conf.mrkt_pca<-table(class.mrkt_pca.pred,pca_test$y)
print("Confusion matrix for .mrkt_logit.pred")
conf.mrkt_pca

sum(diag(conf.mrkt_pca))/sum(conf.mrkt_pca)



