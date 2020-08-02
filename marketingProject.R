library(tidyverse)
library(caret) # downSample() function lives here
library(e1071) # co-package for caret
library(naniar) # has miss_var graph
library(GGally)
library(ROSE)
library(glmnet)
library(corrplot)
library(gplots)

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


unique(mrkt_raw[,c(12)]) ## 12 - campaign (numeric, includes last contact)

##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 19 18 23 14 22 25 16 17 15 20 
##       56 39 35 42 28 26 27 32 21 24 29 31 30 41 37 40 33 34 43


unique(mrkt_raw[,c(13)]) ## 13 - pdays: (numeric; 999 means client was not previously contacted)

##  [1] 999   6   4   3   5   1   0  10   7   8   9  11   2  12  13  14  15  16  
##      21  17  18  22  25  26  19  27  20

unique(mrkt_raw[,c(14)]) ## 14 - previous: (numeric)

## [1] 0 1 2 3 4 5 6 7

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


## Exploring the response variable to get a sense of what are percentage splits are

percentYeses <- dim(mrkt_raw %>% filter(mrkt_raw[,21] == 'yes'))[1]/dim(mrkt_raw)[1] * 100

percentNos <- dim(mrkt_raw %>% filter(mrkt_raw[,21] == 'no'))[1]/dim(mrkt_raw)[1] * 100

percentCheck <- percentYeses + percentNos

# There are roughly 11.3% yeses total, 88.7% nos

## Try out downsampling of the data
## Found a package called ROSE that might be better than downsampling

set.seed(22)

#mrkt_idx <- 
#  sample(
#    1:dim(mrkt_raw)[1],
#    round(dim(mrkt_raw)[1] * 0.7),
#    replace=F
#  )

mrkt_idx <- createDataPartition(mrkt_raw$y, p=0.7, list = F) 


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


mrkt_rose <- ROSE(y~., mrkt_train, seed=22)$data

mrkt_log_ds <- glm(mrkt_downSample$Class ~ ., data=mrkt_downSample, family=binomial)

mrkt_log_ds.pred <- predict(mrkt_log_ds, newdata=mrkt_test, type="response")


mrkt_log_rose <- glm(y ~ ., data=mrkt_rose, family=binomial)

mrkt_log_rose.pred <- predict(mrkt_log_rose, newdata=mrkt_test, type="response")


mrkt_logit <- glm(mrkt_train$y ~ ., data=mrkt_train, family=binomial)

mrkt_logit.pred <- predict(mrkt_logit, newdata=mrkt_test, type="response")


roc.curve(mrkt_test$y, mrkt_log_ds.pred)

roc.curve(mrkt_test$y, mrkt_log_rose.pred, add.roc=TRUE, col=2)

roc.curve(mrkt_test$y, mrkt_logit.pred, add.roc=TRUE, col=3)

dat.train.x <- model.matrix(y~., mrkt_train)
dat.train.y <- mrkt_train[,21]

## CAREFUL of the nlambda, I was crashing R with 100

cvfit <- cv.glmnet(dat.train.x, dat.train.y, family = "binomial", type.measure = "class", nlambda = 25)

plot(cvfit)

coef(cvfit, s = "lambda.min")

#CV misclassification error rate is little below .1

print("CV Error Rate:")

cvfit$cvm[which(cvfit$lambda==cvfit$lambda.min)]

#Optimal penalty
print("Penalty Value:")
cvfit$lambda.min


finalmodel<-glmnet(dat.train.x, dat.train.y, family = "binomial",lambda=cvfit$lambda.min)

dat.test.x<-model.matrix(y~., mrkt_test)

fit.pred.lasso <- predict(finalmodel, newx = dat.test.x, type = "response")

cutoff<-0.5

class.lasso<-factor(ifelse(fit.pred.lasso>cutoff,"yes","no"),levels=c("no","yes"))


#Confusion Matrix for Lasso

conf.lasso<-table(class.lasso,mrkt_test$y)
print("Confusion matrix for LASSO")
conf.lasso

print("Overall accuracy for LASSO and Stepwise respectively")
sum(diag(conf.lasso))/sum(conf.lasso)


# Correlation Matrix of "numerics"

correlationMatrix <- cor(mrkt_raw[,c(1,11,12,14,16,17,18,19,20)], use="pairwise.complete.obs")
corrplot(correlationMatrix, tl.cex = 0.5)



