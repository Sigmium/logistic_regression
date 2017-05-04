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

NH11 <- readRDS("dataSets/NatHealth2011.rds")

# Evaluate:
str(NH11$everwrk)
summary(NH11$everwrk)
levels(NH11$everwrk)
summary(NH11$age_p)
levels(NH11$age_p)
summary(NH11$r_maritl)
levels(NH11$r_maritl)

# Collapse missing values to NA
NH11$everwrk <- factor(NH11$everwrk, levels=c("2 No", "1 Yes"))

# Regression:
LogisticModel1 <- glm(everwrk ~ age_p + r_maritl, data=NH11, family="binomial")
summary(LogisticModel1)
summary(LogisticModel1)

# Conclusion:
# Need to group by marital status, creating a binary variable for each, to effectivly preduct for each group?


##   2. Predict the probability of working for each level of marital
##      status.

# Load required libraries
library(dplyr)
library(caTools)
library(ROCR)

# Load raw data and wrangle:
NH11 <- readRDS("dataSets/NatHealth2011.rds")
NH11$everwrk <- factor(NH11$everwrk, levels=c("2 No", "1 Yes"))
NH11_simple <- filter(NH11, !is.na(everwrk))

# Prep test and training data:
set.seed(1000)
split = sample.split(NH11_simple$everwrk, SplitRatio = 0.65)
trainingData = subset(NH11_simple, split == TRUE)
testingData = subset(NH11_simple, split == FALSE)

# Create GLM on training data
model1 <- glm(everwrk ~ r_maritl, data = trainingData, family = binomial)
summary(model1)

# Predict & confusion matrix
predictTest <- predict(model1, type = "response", newdata = testingData)
table(testingData$everwrk, predictTest > 0.50)
table(testingData$everwrk, predictTest > 0.60)
table(testingData$everwrk, predictTest > 0.85)

# ROCR
ROCRpred = prediction(predictTest, testingData$everwrk)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Findings:
# Donald is 100% confident he is confused from too much time spent in the confusion matrix

# Alternative 1
# Is this the correct approch? Necessary to split and test?

library(dplyr)
library(caTools)

# Load raw data and wrangle:
NH11 <- readRDS("dataSets/NatHealth2011.rds")
NH11$everwrk <- factor(NH11$everwrk, levels=c("2 No", "1 Yes"))
NH11_simple <- filter(NH11, !is.na(everwrk))

# Set binary variables for valid r_marital responses:
married_spouse_in_house <- NH11_simple$r_maritl
married_spouse_in_house <- sub("1 Married - spouse in household", 1, married_spouse_in_house)
married_spouse_in_house[married_spouse_in_house != 1] <- 0
married_spouse_in_house <- factor(married_spouse_in_house)

married_spouse_not_in_house <- NH11_simple$r_maritl
married_spouse_not_in_house <- sub("2 Married - spouse not in household", 1, married_spouse_not_in_house)
married_spouse_not_in_house[married_spouse_not_in_house != 1] <- 0
married_spouse_not_in_house <- factor(married_spouse_not_in_house)

widowed <- NH11_simple$r_maritl
widowed <- sub("Widowed", 1, widowed)
widowed[widowed != 1] <- 0
widowed <- factor(widowed)

divorced <- NH11_simple$r_maritl
divorced <- sub("Divorced", 1, divorced)
divorced[divorced != 1] <- 0
divorced <- factor(divorced)

separated <- NH11_simple$r_maritl
separated <- sub("Separated", 1, separated)
separated[separated != 1] <- 0
separated <- factor(separated)

never_married <- NH11_simple$r_maritl
never_married <- sub("Never married", 1, never_married)
never_married[never_married != 1] <- 0
never_married <- factor(never_married)

living_with_partner <- NH11_simple$r_maritl
living_with_partner <- sub("Living with partner", 1, living_with_partner)
living_with_partner[living_with_partner != 1] <- 0
living_with_partner <- factor(living_with_partner)

NH11_simple <- NH11_simple %>%
  select(everwrk, r_maritl) %>%
  mutate(married_spouse_in_house, married_spouse_not_in_house, widowed, divorced, separated, never_married, living_with_partner)

# Prep test and training data:
set.seed(1000)
split = sample.split(NH11_simple$everwrk, SplitRatio = 0.65)
trainingData = subset(NH11_simple, split == TRUE)
testingData = subset(NH11_simple, split == FALSE)

# Create GLM on training data
model1 <- glm(everwrk ~ ., data = trainingData, family = binomial)
summary(model1)

# Predict
predictTest <- predict(model1, type = "response", newdata = testingData)
table(testingData$everwrk, predictTest > 0.75)

library(ROCR)

ROCRpred = prediction(predictTest, testingData$everwrk)
as.numeric(performance(ROCRpred, "auc")@y.values)

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.
