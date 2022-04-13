
# Load the data - FROM DOWNLOADED FILE: April 13, 2022
raw <- read.csv("archive/dataset.csv")
raw_x <- subset(raw, select = -c(X))

# Remove:
#   encounter_id
#   patient_id
#   icu_id
#   
subset_1 <- subset(raw_x, select= -c(encounter_id, patient_id))

# how much of the data is missing
sum(!complete.cases(subset_1))

cols_with_na = c()
cols_wo_na = c()

for (i in 1:length(subset_1)) {
  if (sum(is.na(subset_1[,i])) > 0 ){
    cols_with_na <- append(cols_with_na, i)
  } else {
    cols_wo_na <- append(cols_wo_na, i)
  }
  
}

cols_with_na
cols_wo_na
