### R function for data munging the hospital data.

# function for doing the primary data munging (factors, drops, etc.)
hosp_munge <- function(hospital) {
  hospital$hospital_death <- as.factor(hospital$hospital_death)
  levels(hospital$hospital_death) <- c("0", "1")
  hospital = subset(hospital, select = -c(encounter_id, patient_id, apache_4a_hospital_death_prob, apache_4a_icu_death_prob, X))
  
  hospital$ethnicity <- as.factor(hospital$ethnicity)
  hospital$ethnicity <- ifelse(hospital$ethnicity == "Caucasian", 1, hospital$ethnicity)
  hospital$gender <- as.factor(hospital$gender)
  hospital$gender <- ifelse(hospital$ethnicity == "M", 0, hospital$gender)
  hospital$gender <- ifelse(hospital$ethnicity == "F", 1, hospital$gender)
  hospital$icu_admit_source <- as.factor(hospital$icu_admit_source)
  hospital$icu_admit_source <- ifelse(hospital$icu_admit_source == "Accident & Emergency", 0, hospital$icu_admit_source)
  hospital$icu_admit_source <- ifelse(hospital$icu_admit_source == "Operating Room / Recovery", 1, hospital$icu_admit_source)
  hospital$icu_admit_source <- ifelse(hospital$icu_admit_source == "Floor", 2, hospital$icu_admit_source)
  hospital$icu_admit_source <- ifelse(hospital$icu_admit_source == "Other Hospital", 3, hospital$icu_admit_source)
  hospital$icu_admit_source <- ifelse(hospital$icu_admit_source == "Other ICU", 4, hospital$icu_admit_source)
  hospital$icu_stay_type <- as.factor(hospital$icu_stay_type)
  hospital$icu_stay_type <- ifelse(hospital$icu_stay_type == "admit", 0, hospital$icu_stay_type)
  hospital$icu_stay_type <- ifelse(hospital$icu_stay_type == "readmit", 1, hospital$icu_stay_type)
  hospital$icu_stay_type <- ifelse(hospital$icu_stay_type == "transfer", 2, hospital$icu_stay_type)
  hospital$icu_type <- as.factor(hospital$icu_type)
  hospital$icu_type <- ifelse(hospital$icu_type == "Med-Surg ICU", 0, hospital$icu_type)
  hospital$icu_type <- ifelse(hospital$icu_type == "MICU", 1, hospital$icu_type)
  hospital$icu_type <- ifelse(hospital$icu_type == "CTICU", 2, hospital$icu_type)
  hospital$icu_type <- ifelse(hospital$icu_type == "CCU-CTICU", 3, hospital$icu_type)
  hospital$icu_type <- ifelse(hospital$icu_type == "Neuro ICU", 4, hospital$icu_type)
  hospital$icu_type <- ifelse(hospital$icu_type == "SICU", 5, hospital$icu_type)
  hospital$apache_3j_bodysystem <- as.factor(hospital$apache_3j_bodysystem)
  hospital$apache_3j_bodysystem <- ifelse(hospital$apache_3j_bodysystem == "Cardiovascular", 0, hospital$apache_3j_bodysystem)
  hospital$apache_3j_bodysystem <- ifelse(hospital$apache_3j_bodysystem == "Metabolic", 1, hospital$apache_3j_bodysystem)
  hospital$apache_3j_bodysystem <- ifelse(hospital$apache_3j_bodysystem == "Nuerological", 2, hospital$apache_3j_bodysystem)
  hospital$apache_3j_bodysystem <- ifelse(hospital$apache_3j_bodysystem == "Gastrointestinal", 3, hospital$apache_3j_bodysystem)
  hospital$apache_3j_bodysystem <- ifelse(hospital$apache_3j_bodysystem == "Metabolic", 4, hospital$apache_3j_bodysystem)
  hospital$apache_3j_bodysystem <- ifelse(hospital$apache_3j_bodysystem == "Musculoskeletal/Skin", 5, hospital$apache_3j_bodysystem)
  hospital$apache_3j_bodysystem <- ifelse(hospital$apache_3j_bodysystem == "Respiratory", 6, hospital$apache_3j_bodysystem)
  hospital$apache_3j_bodysystem <- ifelse(hospital$apache_3j_bodysystem == "Trauma", 7, hospital$apache_3j_bodysystem)
  hospital$apache_3j_bodysystem <- ifelse(hospital$apache_3j_bodysystem == "Genitourinary", 8, hospital$apache_3j_bodysystem)
  hospital$apache_2_bodysystem <- as.factor(hospital$apache_2_bodysystem)
  hospital$apache_2_bodysystem <- ifelse(hospital$apache_2_bodysystem == "Cardiovascular", 0, hospital$apache_2_bodysystem)
  hospital$apache_2_bodysystem <- ifelse(hospital$apache_2_bodysystem == "Metabolic", 1, hospital$apache_2_bodysystem)
  hospital$apache_2_bodysystem <- ifelse(hospital$apache_2_bodysystem == "Nuerological", 2, hospital$apache_2_bodysystem)
  hospital$apache_2_bodysystem <- ifelse(hospital$apache_2_bodysystem == "Gastrointestinal", 3, hospital$apache_2_bodysystem)
  hospital$apache_2_bodysystem <- ifelse(hospital$apache_2_bodysystem == "Metabolic", 4, hospital$apache_2_bodysystem)
  hospital$apache_2_bodysystem <- ifelse(hospital$apache_2_bodysystem == "Musculoskeletal/Skin", 5, hospital$apache_2_bodysystem)
  hospital$apache_2_bodysystem <- ifelse(hospital$apache_2_bodysystem == "Respiratory", 6, hospital$apache_2_bodysystem)
  hospital$apache_2_bodysystem <- ifelse(hospital$apache_2_bodysystem == "Trauma", 7, hospital$apache_2_bodysystem)
  hospital$apache_2_bodysystem <- ifelse(hospital$apache_2_bodysystem == "Genitourinary", 8, hospital$apache_2_bodysystem)
  
  return(hospital)
  
}


# function for replacing NA values of factors with the mode of the column
hosp_mode_mean <- function(hospital) {
  cols = length(hospital)
  
  # iterate over columns of data table
  for (i in 1:cols) {
    # check if column is a factor
    if (is.factor(hospital[,i])) {
      # iterate over values, compute mode, replace NAs with mode.
      samp <- c()
      for (j in 1:length(hospital[,i])) {
        if (!is.na(hospital[j,i])) {
          samp <- append(samp, hospital[j,i])
        }
      }
      uniq_samp <- unique(samp)
      mode_samp <- uniq_samp[which.max(tabulate(match(samp, uniq_samp)))]
      hospital[,i] <- replace_na(hospital[,i], replace = mode_samp)
    } else {
      # in this case the values are numeric and need to be replaced with mean value
      hospital[is.na(hospital[,i]),i] <- mean(hospital[,i], na.rm=TRUE)
    }
  }
  
  return(hospital)
}


# function for cutoff optimization
cutoff_opt <- function(test_data, userpred, pos_val) {
  # pos_val = "1" or "0"
  # userpred = prediction values from the model
  # usermod = the model
  
  cutoffs = as.vector(seq(0.01, 0.9, by = 0.01))
  
  res_mat <- matrix(vector(), nrow = length(cutoffs), ncol = 3, dimnames = list(c(), c("Cutoff", "Accuracy", "Kappa")))
  
  for (i in 1:length(cutoffs)) {
    
    the_cut = cutoffs[i]
    
    model_preds <- ifelse(userpred[,2] > the_cut, "1", "0")
    confMat <- confusionMatrix(as.factor(model_preds), 
                               as.factor(test_data$hospital_death), 
                               positive = pos_val)
    res_mat[i,1] = the_cut
    res_mat[i,2] = confMat$overall[1]
    res_mat[i,3] = confMat$overall[2]
  
  }
  
  best_acc <- res_mat[which.max(res_mat[,2]),]
  best_kap <- res_mat[which.max(res_mat[,3]),]
  
  return(list(best_kappa = best_kap, best_acc = best_acc, res_matrix = res_mat))
  
}



### R function for creating 2 partitions with training and test set
### This tool courtesy of Dr. Shuchismita Sarkar.
### Input: original dataset name (data)
###        proportion of records assigned to training set (prop.train)
### Output: data.train, data.test

partition.2 <- function(data, prop.train){
  # select a random sample of size = prop.train % of total records
  selected <- sample(1:nrow(data), round(nrow(data)*prop.train), replace = FALSE) 
  # create training data which has prop.train % of total records
  data.train <- data[selected,]
  # create validation data
  rest <- setdiff(1:nrow(data), selected)
  data.test <- data[rest,]
  return(list(data.train=data.train, data.test=data.test))
}


# cross_compare <- function(model_list_1, model_list_2) {
#   # model_list = c(model, test_data, type)
# 
#   # extract inputs
#   mod.1 <- model_list_1[1]
#   mod.2 <- model_list_2[1]
#   test.1 <- model_list_1[2]
#   test.2 <- model_list_2[2]
#   type.1 <- model_list_1[3]
#   type.2 <- model_list_2[3]
# 
#   # compute predictions with cross test data
#   if (type.1 == "CT") {
#     model.1.pred <- predict(mod.1, test.2, type = "prob")
#   } else {
#     model.1.pred <- predict(mod.1, test.2, type = "prob")
#   }
# 
#   if (type.2 == "CT") {
#     model.2.pred <- predict(mod.2, test.1, type = "prob")
#   } else {
#     model.2.pred <- predict(mod.2, test.1, type = "prob")
#   }
# 
#   # iterate to determine best cutoff
#   cut.res.1 <- cutoff_opt(test.2, model.1.pred, pos_val = "1")
#   cut.res.2 <- cutoff_opt(test.1, model.2.pred, pos_val = "1")
# 
#   # best cutoffs
#   cut.1.best <- cut.res.1$best_kappa[1]
#   cut.2.best <- cut.res.2$best_kappa[1]
# 
#   # update predictions with cutoff values
#   preds.1 <- ifelse(model.1.pred[,2] > cut.1.best, "1", "0")
#   preds.2 <- ifelse(model.2.pred[,2] > cut.2.best, "1", "0")
# 
#   # generate confusion matrices
#   conf.1 <- confusionMatrix(as.factor(preds.1),
#                             as.factor(test.2$hospital_death), positive = "1")
#   conf.2 <- confusionMatrix(as.factor(preds.2),
#                             as.factor(test.1$hospital_death), positive = "1")
# 
# 
#   # return results
#   return(list(conf.1 = conf.1, conf.2 = conf.2))
# 
# 
# }
