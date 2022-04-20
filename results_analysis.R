source("data_munge_funcs.R")
library(caret)
library(rpart)
library(rpart.plot)
library(glmnet)
library(tidyr)
RNGkind(sample.kind = "Rounding")
load("6modelResults.RData")


# Compute Optimal Cut-off values for the 
# Lasso Logistic Regression and Classification Tree Models
lasso_mice_cutoffs <- cutoff_opt(mice.test, LASSO_MICE_PRED, pos_val = "0")
lasso_drop_cutoffs <- cutoff_opt(drop.test, LASSO_DROP_PRED, pos_val = "0")
lasso_mm_cutoffs <- cutoff_opt(mm.test, LASSO_MM_PRED, pos_val = "0")

lasso_mice_cutoffs$best_kappa
lasso_drop_cutoffs$best_kappa
lasso_mm_cutoffs$best_kappa

plot(lasso_mice_cutoffs$res_matrix[,c(1,3)], col="blue", type='p', lwd = 2, main="Lasso Regression Cut-off Optimization")
lines(lasso_drop_cutoffs$res_matrix[,c(1,3)], col="green", type='l', lwd = 2)
lines(lasso_mm_cutoffs$res_matrix[,c(1,3)], col = "red", type = 'p', lwd = 2)
grid(nx = NULL, ny=NULL, col = "lightgray")
legend(x = "topright", legend = c("MICE Data Model","NA Dropped Data Model", 
                                  "Mean/Mode Replaced Data Model"), 
       lty = c(0,1,0), lwd = c(2,2,2), pch = c(1,NA,1), 
       col = c("blue", "green", "red"), cex = .8)
















# cutoffs = as.vector(seq(0.01, 0.8, by=0.01))
# 
# res_mat <- matrix(vector(), nrow = length(cutoffs), ncol = 3)
# res_acc <- c()
# res_kap <- c()
# for (i in 1:length(cutoffs)){
#   
#   the_cut = cutoffs[i]
#   
#   model_preds <- ifelse(LASSO_MICE_PRED[,2] > the_cut, "1", "0")
#   
#   c <- confusionMatrix(as.factor(model_preds), 
#                        as.factor(mice.test$hospital_death), positive = "0")
#   res_acc[i] = c$overall[1]
#   res_kap[i] = c$overall[2]
#   
#   
# }
# res_mat[,1] = cutoffs
# res_mat[,2] = res_acc
# res_mat[,3] = res_kap
# res_mat
# 
# res_mat[which.max(res_mat[,2]),]
# res_mat[which.max(res_mat[,3]),]

