load("RFResultsWorkspace.RData")
source("data_munge_funcs.R")
library(caret)
library(tidyr)

# Load Raw hospital data
hosp <- read.csv("dataset.csv")

# Drop all NA values and save as new data set

# First do some data munging
hosp_na <- hosp_munge(hosp)

# Drop
hosp_na <- drop_na(hospital)
# 