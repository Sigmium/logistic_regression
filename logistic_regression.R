## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).

# SETUP:
# Load data and libs
library(dplyr)
library(caTools)
library(ROCR)
library(effects)
library(ggplot2)

# IMPORT:
NH11 <- readRDS("dataSets/NatHealth2011.rds")

#### 1) EVALUATE DATA: ####
str(NH11$everwrk)
summary(NH11$everwrk)
levels(NH11$everwrk)
summary(NH11$age_p)
levels(NH11$age_p)
summary(NH11$r_maritl)
levels(NH11$r_maritl)

# COPY RAW DATA INTO NEW DF & WRANGLE:
# Copy raw into new dataframe for wrandling
NH11_new <- NH11

# Dependent variable has too many levels and isnt binary.
# Collapse missing values to NA, convert to binary and set as numeric.
NH11_new$everwrk <- factor(NH11_new$everwrk, levels=c("2 No", "1 Yes"))
NH11_new <- NH11_new %>%
  filter(!is.na(NH11_new$everwrk))

NH11_new <- NH11_new %>%
  mutate(everwrk = sub('2 No', 0, everwrk)) %>%
  mutate(everwrk = sub('1 Yes', 1, everwrk))
NH11_new$everwrk <- as.factor(NH11_new$everwrk)

# Validate changes
str(NH11_new$everwrk)
table(NH11_new$everwrk)

#### 2) DETERMINE CORRECT APPROACH: ####
# I'm unsure which of the following two approaches is correct. Both
# are a variation on how I'm treating the r_maritl data. I need to
# (a) determine which is correct and why, then (b) ensure I am correctly
# running the model(s).
# APPROACH 1 = Copy r_maritl values into their own vector and re-encode to binary numeric values (Row: 151)
# APPROACH 2 = Re-encode r_maritl values as numeric, in the same vector (Row: 235 )

#### 2.a) FIRST APPROACH ####
# Copy r_maritl values into their own vector and re-encode to binary numeric values.
# Note: Missing or NAs are excluded (Ex: "0 Under 14 years", "3 Married - spouce in household unknown", and "9
# Unknown marital status").
NH11_new$married_spouse_in_house <- NH11_new$r_maritl
NH11_new$married_spouse_in_house <- sub("1 Married - spouse in household", 1, NH11_new$married_spouse_in_house)
NH11_new$married_spouse_in_house[NH11_new$married_spouse_in_house != 1] <- 0
NH11_new$married_spouse_in_house <- as.numeric(NH11_new$married_spouse_in_house)

NH11_new$married_spouse_not_in_house <- NH11_new$r_maritl
NH11_new$married_spouse_not_in_house <- sub("2 Married - spouse not in household", 1, NH11_new$married_spouse_not_in_house )
NH11_new$married_spouse_not_in_house[NH11_new$married_spouse_not_in_house  != 1] <- 0
NH11_new$married_spouse_not_in_house <- as.numeric(NH11_new$married_spouse_not_in_house )

NH11_new$widowed <- NH11_new$r_maritl
NH11_new$widowed <- sub("4 Widowed", 1, NH11_new$widowed)
NH11_new$widowed[NH11_new$widowed != 1] <- 0
NH11_new$widowed <- as.numeric(NH11_new$widowed)

NH11_new$divorced <- NH11_new$r_maritl
NH11_new$divorced <- sub("5 Divorced", 1, NH11_new$divorced )
NH11_new$divorced[NH11_new$divorced  != 1] <- 0
NH11_new$divorced <- as.numeric(NH11_new$divorced )

NH11_new$separated <- NH11_new$r_maritl
NH11_new$separated <- sub("6 Separated", 1, NH11_new$separated )
NH11_new$separated[NH11_new$separated  != 1] <- 0
NH11_new$separated <- as.numeric(NH11_new$separated )

NH11_new$never_married <- NH11_new$r_maritl
NH11_new$never_married <- sub("7 Never married", 1, NH11_new$never_married)
NH11_new$never_married[NH11_new$never_married != 1] <- 0
NH11_new$never_married <- as.numeric(NH11_new$never_married)

NH11_new$living_with_partner <- NH11_new$r_maritl
NH11_new$living_with_partner <- sub("8 Living with partner", 1, NH11_new$living_with_partner)
NH11_new$living_with_partner[NH11_new$living_with_partner != 1] <- 0
NH11_new$living_with_partner <- as.numeric(NH11_new$living_with_partner)

summary(NH11_new)
table(NH11_new$everwrk)
# Summary of output:
# 2 No = 1887, 1 Yes = 12153
# Simple baseline is 12153/14040 or 86.56% accuracy predicting has ever worked

# Running first model
LogisticModel1 <- glm(everwrk ~ age_p + married_spouse_in_house + married_spouse_not_in_house + widowed + divorced + separated + never_married + living_with_partner, data=NH11_new, family="binomial")
summary(LogisticModel1)
coef(summary(LogisticModel1))
# AIC = 10327
# age_p, divorced and living_with_partner are statistically significant

# Running second model with just statistically significant variables (divorced and ligin_with_partner)
LogisticModel2 <- glm(everwrk ~ age_p + divorced + living_with_partner, data=NH11_new, family="binomial")
summary(LogisticModel2)
coef(summary(LogisticModel2))
# AIC = 10406
# age_p, divorced and living_with_partner remain statistically significant

#ROC & AUC based on variables used in second model

# Prep test and training data:
# set.seed(1000)
split = sample.split(NH11_new$everwrk, SplitRatio = 0.65)
NEWtrainingData = subset(NH11_new, split == TRUE)
NEWtestingData = subset(NH11_new, split == FALSE)

# Create GLM using second model settings and TRAINING data
LogisticMode2 <- glm(everwrk ~ age_p + divorced + living_with_partner, data = NEWtrainingData, family = binomial)
summary(LogisticModel2)
# AIC = 10406, unchanged from modeling total data.

# Use the model to predict on TESTING data
predictTest = predict(LogisticModel2, type = "response", newdata = NEWtestingData)
table(NEWtestingData$everwrk, predictTest > 0.50)

# ROCR for out of sample AUC
ROCRpred = prediction(predictTest, NEWtestingData$everwrk)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC = 67.5%

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2, 1.7))

#### 2.b) SECOND APPROACH ####
# Convert independent variable "r_maritl" to numeric value(s)

NH11_new <- NH11_new %>%
  mutate(r_maritl = sub('0 Under 14 years', 0, r_maritl)) %>%
  mutate(r_maritl = sub('1 Married - spouse in household', 1, r_maritl)) %>%
  mutate(r_maritl = sub('2 Married - spouse not in household', 2, r_maritl)) %>%
  mutate(r_maritl = sub('3 Married - spouse in household unknown', 3, r_maritl)) %>%
  mutate(r_maritl = sub('4 Widowed', 4, r_maritl)) %>%
  mutate(r_maritl = sub('5 Divorced', 5, r_maritl)) %>%
  mutate(r_maritl = sub('6 Separated', 6, r_maritl)) %>%
  mutate(r_maritl = sub('7 Never married', 7, r_maritl)) %>%
  mutate(r_maritl = sub('8 Living with partner', 8, r_maritl)) %>%
  mutate(r_maritl = sub('9 Unknown marital status', 9, r_maritl)) %>%
  filter(r_maritl != "9")
NH11_new$r_maritl <- as.numeric(NH11_new$r_maritl)

# Check structure and summary
table(NH11_new$r_maritl)
table(NH11_new$everwrk, NH11_new$r_maritl)
table(NH11_new$everwrk)
# Summary of output:
# 2 No = 1887, 1 Yes = 12153
# Simple baseline is 12153/14040 or 86.56% accuracy predicting everwrk

LogisticModel1 <- glm(everwrk ~ age_p + r_maritl, data=NH11_new, family="binomial")
summary(LogisticModel1)
coef(summary(LogisticModel1))
# AIC = 10526
# age_p and r_maritl both statistically significant

#ROC & AUC

# Prep test and training data:
# set.seed(1000)
split = sample.split(NH11_new$everwrk, SplitRatio = 0.65)
NEWtrainingData = subset(NH11_new, split == TRUE)
NEWtestingData = subset(NH11_new, split == FALSE)

# Create GLM for TRAINING data
LogisticModel2 <- glm(everwrk ~ age_p + r_maritl, data = NEWtrainingData, family = binomial)
summary(LogisticModel2)
# AIC = 6819, large drop from total data
# age_p and r_maritl both statistically significant

# Use the model to predict on TESTING data
predictTest = predict(LogisticModel2, type = "response", newdata = NEWtestingData)
table(NEWtestingData$everwrk, predictTest > 0.50)

# ROCR for out of sample AUC
ROCRpred = prediction(predictTest, NEWtestingData$everwrk)
as.numeric(performance(ROCRpred, "auc")@y.values)
# AUC = 65.4%

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2, 1.7))

#### 3) PREDICT PROBABILITY OF WORKING BY AGE ####
##   2. Predict the probability of working for each level of marital
##      status.

# Predict probability of ever working by age, using effects library
LogisticModel_Q2 <- glm(everwrk ~ age_p + r_maritl, data=NH11_new, family="binomial")
plot(allEffects(LogisticModel_Q2))