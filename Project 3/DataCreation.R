################### Read in data and remove NA
stroke <- read.csv("healthcare-dataset-stroke-data.csv")
stroke <- stroke[stroke$gender != "Other",]
stroke$id <- NULL
stroke$gender <- as.factor(stroke$gender)
stroke$work_type <- as.factor(stroke$work_type)
stroke$smoking_status <- as.factor(stroke$smoking_status)
stroke$ever_married <- as.factor(stroke$ever_married)
stroke$Residence_type <- as.factor(stroke$Residence_type)
summary(stroke)

# create the bmi data that doesn't have NA
bmi_data <- stroke[stroke$bmi != "N/A",]
# can't use stroke to predict bmi
bmi_model <- lm(bmi ~ .-stroke, data = bmi_data)
summary(bmi_model)
# remove insignificant variables
bmi_model <- lm(bmi ~ .-stroke-gender-Residence_type+age*hypertension*ever_married-heart_disease, data = bmi_data)
summary(bmi_model)

stroke$bmi[stroke$bmi == "N/A"] = predict(bmi_model, stroke[stroke$bmi == "N/A",])
stroke$bmi <- as.numeric(stroke$bmi)

###################################### Define some functions
set.seed(42)

normalize <- function(x) {
  if(max(x) - min(x) == 0){
    return (x)
  }
  return ((x - min(x)) / (max(x) - min(x)))
}

set_splits <- function(x, norm) {
  if(norm){
    x = as.data.frame(lapply(as.data.frame(model.matrix(~.-1,x)), normalize))
  }
  testrows <- sample(1:nrow(x), 0.25*nrow(x))
  return (list(x[-testrows,], x[testrows,]))
}

# Now create all splits w/o norm and one hot encoding

# you can access the dataframe with age_young_data[[1]] for train, age_young_data[[2]] for test
general <- set_splits(stroke, norm = FALSE)
age_young_data = set_splits(stroke[stroke$age<=55,], norm = FALSE)
age_old_data = set_splits(stroke[stroke$age>55,], norm = FALSE)
bmi_risk_data = set_splits(stroke[stroke$bmi>26.5 & stroke$bmi<32,], norm = FALSE)
bmi_safe_data = set_splits(stroke[stroke$bmi<=26.5 | stroke$bmi>=32,], norm = FALSE)

# alternatively do it with the norm; you'll only need one of the splits
general_norm <- set_splits(stroke, norm = TRUE)
age_young_data_norm = set_splits(stroke[stroke$age<=55,], norm = TRUE)
age_old_data_norm = set_splits(stroke[stroke$age>55,], norm = TRUE)
bmi_risk_data_norm = set_splits(stroke[stroke$bmi>26.5 & stroke$bmi<32,], norm = TRUE)
bmi_safe_data_norm = set_splits(stroke[stroke$bmi<=26.5 | stroke$bmi>=32,], norm = TRUE)

############## example with the SVM model
# this is the general model
library(kernlab)
stroke_svm <- ksvm(stroke ~ ., data = general_norm[[1]], kernel = "rbfdot")
svm_pred <- predict(stroke_svm, general_norm[[2]])
svm_pred = ifelse(svm_pred>0, 0, 1)
library(caret)
confusionMatrix(as.factor(svm_pred), as.factor(general_norm[[2]]$stroke))

####### now make two models for age
# first is young model
young_svm <- ksvm(stroke ~ ., data = age_young_data_norm[[1]], kernel = "rbfdot")
young_pred <- predict(young_svm, age_young_data_norm[[2]])
young_pred = ifelse(young_pred>0, 0, 1)
# now old model
old_svm <- ksvm(stroke ~ ., data = age_old_data_norm[[1]], kernel = "rbfdot")
old_pred <- predict(old_svm, age_old_data_norm[[2]])
old_pred = ifelse(old_pred>0, 0, 1)

# now combine them together
combined_pred = c(young_pred, old_pred)
# get the true labels
true_label = c(age_young_data_norm[[2]]$stroke, age_old_data_norm[[2]]$stroke)
# make confusion matrix, if you do as.factor it usually
# solves any issues you get with named nums from the model predictions
confusionMatrix(as.factor(combined_pred), as.factor(true_label))


####### similar for BMI
risk_svm <- ksvm(stroke ~ ., data = bmi_risk_data_norm[[1]], kernel = "rbfdot")
risk_pred <- predict(risk_svm, bmi_risk_data_norm[[2]])
risk_pred = ifelse(risk_pred>0, 0, 1)

safe_svm <- ksvm(stroke ~ ., data = bmi_safe_data_norm[[1]], kernel = "rbfdot")
safe_pred <- predict(safe_svm, bmi_safe_data_norm[[2]])
safe_pred = ifelse(safe_pred>0, 0, 1)

combined_pred = c(risk_pred, safe_pred)
true_label = c(bmi_risk_data_norm[[2]]$stroke, bmi_safe_data_norm[[2]]$stroke)
confusionMatrix(as.factor(combined_pred), as.factor(true_label))
