load("RFResultsWorkspace.RData")
source("data_munge_funcs.R")
library(caret)
library(rpart)
library(rpart.plot)
library(glmnet)
library(tidyr)
RNGkind(sample.kind = "Rounding")

# Load Raw hospital data
hosp <- read.csv("dataset.csv")

# Create 2 data sets (MICE data set created externally)
# 1. MICE imputed data set
# 2. Drop NA
# 3. Mean/Mode Replaced sets
#   3a. Mode used as replacement for factors
#   3b. Mean used as replacement for numeric vals

# First do some data munging
hosp_drop <- hosp_munge(hosp)
hosp_mm <- hosp_munge(hosp)

# Drop na for dropped set
hosp_drop <- drop_na(hospital)

# Replace na with mode/mean for hosp_mm
hosp_mm <- hosp_mode_mean(hosp_mm)

# Rename the MICE data set
hosp_mice <- hospital

# Data Partitioning
set.seed(0)
p_drop <- partition.2(hosp_drop, 0.7)
drop.test <- p_drop$data.test
drop.train <- p_drop$data.train

set.seed(0)
p_mm <- partition.2(hosp_mm, 0.7)
mm.test <- p_mm$data.test
mm.train <- p_mm$data.train

set.seed(0)
p_mice <- partition.2(hosp_mice, 0.7)
mice.test <- p_mice$data.test
mice.train <- p_mice$data.train


# ==============================================================================
# MODELS
# Each model is built over each of the 3 data sets:
# MICE: the MICE imputed data set
# DROP: the NA dropped data set
# MM  : the NA replaced with column-wise Means/Modes data set 

tC <- trainControl(method = "cv", number = 10)


# --------------------------
# Classification Trees (CT)

# CT_MICE
set.seed(0)
CT_MICE <- train(hospital_death ~., data = mice.train, method = "rpart", 
                 trControl = tC, tuneLength = 10, metric = "Kappa")
CT_MICE_PRED <- predict(CT_MICE$finalModel, mice.test, type='class')
CT_MICE_PROB <- predict(CT_MICE$finalModel, mice.test, type='prob')


# CT_DROP
set.seed(0)
CT_DROP <- train(hospital_death ~., data = drop.train, method = "rpart", 
                 trControl = tC, tuneLength = 10, metric = "Kappa")
CT_DROP_PRED <- predict(CT_DROP$finalModel, drop.test, type='class')
CT_DROP_PROB <- predict(CT_DROP$finalModel, drop.test, type='prob')


# CT_MM
set.seed(0)
CT_MM <- train(hospital_death ~., data = mm.train, method = "rpart", 
               trControl = tC, tuneLength = 10, metric = "Kappa")
CT_MM_PRED <- predict(CT_MM$finalModel, mm.test, type = 'class')
CT_MM_PROB <- predict(CT_MM$finalModel, mm.test, type = 'prob')



# -------------------------------------------
# Logistic Regression Classification (LASSO)
# Lasso regression was chosen.

# LASSO_MICE
set.seed(0)
LASSO_MICE <- train(hospital_death ~., data = mice.train, method = "glmnet", 
                    metric = "Kappa", family = "binomial", trControl = tC, 
                    tuneGrid = expand.grid(
                      alpha = 1, lambda = seq(0.001, 0.1, by = 0.001)))

LASSO_MICE_PRED <- predict(LASSO_MICE, s = LASSO_MICE$bestTune, mice.test, 
                           type = "prob")


# LASSO_DROP
set.seed(0)
LASSO_DROP <- train(hospital_death ~., data = drop.train, method = "glmnet", 
                    metric = "Kappa", family = "binomial", trControl = tC, 
                    tuneGrid = expand.grid(
                      alpha = 1, lambda = seq(0.001, 0.1, by = 0.001)))

LASSO_DROP_PRED <- predict(LASSO_DROP, s = LASSO_DROP$bestTune, drop.test, 
                           type = "prob")

# LASSO_MM
set.seed(0)
LASSO_MM <- train(hospital_death ~., data = mm.train, method = "glmnet", 
                  metric = "Kappa", family = "binomial", trControl = tC, 
                  tuneGrid = expand.grid(
                    alpha = 1, lambda = seq(0.001, 0.1, by = 0.001)))

LASSO_MM_PRED <- predict(LASSO_MM, s = LASSO_MM$bestTune, mm.test, 
                         type = "prob")



# ---------------------------------------------------------------
# Bagged Trees (Boot Strap: method 'treebag' Classification (BAG)
# Random forest models were extremely costly and ineffective to 
# produce, so instead we are trying out bagging for it's slightly
# simpler algorithm. Using 75 trees (default is 25) to try and 
# improve accuracy.
set.seed(0)
nbag = 75;

# BAG_MICE
BAG_MICE <- train(hospital_death ~., data = mice.train, method = "treebag", 
                  trControl = tC, nbagg = nbag)

# prediction
BAG_MICE_PRED <- predict(BAG_MICE$finalModel, mice.test, type = 'prob')

# BAG_DROP
set.seed(0)
BAG_DROP <- train(hospital_death ~., data = drop.train, method = "treebag", 
                  trControl = tC, nbagg = nbag)

# prediction
BAG_DROP_PRED <- predict(BAG_DROP$finalModel, drop.test, type = 'prob')

# BAG_MM
set.seed(0)
BAG_MM <- train(hospital_death ~., data = mm.train, method = "treebag", 
                trControl = tC, nbagg = nbag)

# prediction
BAG_MM_PRED <- predict(BAG_MM$finalModel, mm.test, type = 'prob')


# ----------------------------------
# Random Forest Classification (RF)
# Number of predictors is 80 -> sqrt(80) is between 8-9: use mtry = 8, 9, then add outliers.
tG <- expand.grid(mtry = c(2, 8, 9, 40))

# RF_MICE
set.seed(0)
RF_MICE <- train(hospital_death ~., data = mice.train, method = "rf", 
                 ntree = 100, trControl = tC, tuneGrid = tG)


#RF_MICE_PRED <- predict(RF_MICE, mice.test, type = 'prob')


# RF_DROP
set.seed(0)
RF_DROP <- train(hospital_death ~., data = drop.train, method = "rf", 
                 ntree = 100, trControl = tC, tuneGrid = tG)

#RF_DROP_PRED <- predict(RF_DROP, drop.test, type = 'prob')


# RF_MM
set.seed(0)
RF_Mm <- train(hospital_death ~., data = mm.train, method = "rf", 
                 ntree = 100, trControl = tC, tuneGrid = tG)

#RF_Mm_PRED <- predict(RF_MM, mm.test, type = 'prob')






