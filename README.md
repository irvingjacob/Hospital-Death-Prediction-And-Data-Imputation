# Hospital-Death-Prediction-And-Data-Imputation
Final Project Spring 2022 STAT6440 

Jacob Mitchell Dan Mrachko

## Data 
Data Source: [https://www.kaggle.com/datasets/mitishaagarwal/patient](https://www.kaggle.com/datasets/mitishaagarwal/patient) 

dataset.csv contains the raw dataset with no data imputations done on it

dataset_mice_output.csv is the raw dataset using mice imputation to fill missing data

## Scripts

TermProject.Rmd: Contains Data Impuations and Data Munging

initial_cleaning_work.R: contains data cleaning in one location so it doesn't need to be repeated. 

ClassificationTree.Rmd: Contains classification tree models and rnadom forest models

logisticLasso.Rmd: Containts logistic regression model using Lasso

drm_models.r: Contains a file with all models consolidated into one script

## Workspace
FRResultsWorkspace.RData included so models don't need to be ran. Models can take several minutes to be built. 
