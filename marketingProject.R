library(tidyverse)
library(caret) # downSample() function lives here
library(e1071) # co-package for caret
library(naniar) # has miss_var graph
library(GGally)

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
## 14 - previous: (numeric)
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


