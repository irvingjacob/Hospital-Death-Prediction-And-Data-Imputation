load("RFResultsWorkspace.RData")
source("data_munge_funcs.R")
library(caret)
library(tidyr)

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


# ==============================================================================
# MODELS
# Each model is built over each of the 3 data sets:
# MICE: the MICE imputed data set
# DROP: the NA dropped data set
# MM  : the NA replaced with column-wise Means/Modes data set 


# --------------------------
# Classification Trees (CT)

# CT_MICE

# CT_DROP

# CT_MM



# ----------------------------------
# Random Forest Classification (RF)

# RF_MICE

# RF_DROP

# RF_MM



# ----------------------------------------
# Logistic Regression Classification (LR)

# LR_MICE

# LR_DROP

# LR_MM
