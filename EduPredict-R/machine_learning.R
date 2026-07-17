# ==========================================
# EduPredict
# File : 05_machine_learning.R
# Student Performance Prediction
# ==========================================

# -------------------------------
# Required Packages
# -------------------------------
library(mongolite)
library(dplyr)
library(caret)
library(randomForest)

# -------------------------------
# MongoDB Connection
# -------------------------------
student_collection <- mongo(
  collection = "datastudents",
  db = "studentdata",
  url = "mongodb://localhost:27017"
)

students <- student_collection$find()

cat("Data Loaded Successfully\n")

# -------------------------------
# Attendance to Numeric
# -------------------------------
students$attendancePercentage <-
  as.numeric(gsub("%","",students$attendancePercentage))

# -------------------------------
# Convert Target Variable
# -------------------------------
students$passFailStatus <-
  as.factor(students$passFailStatus)

# -------------------------------
# Keep Only Numeric Columns
# -------------------------------
numeric_columns <-
  names(students)[sapply(students,is.numeric)]

# Remove unwanted numeric IDs
numeric_columns <-
  setdiff(
    numeric_columns,
    c(
      "rollNumber",
      "maxPossibleMarks"
    )
  )

# -------------------------------
# Machine Learning Dataset
# -------------------------------
ml_data <-
  students[,c(numeric_columns,
              "passFailStatus")]

# -------------------------------
# Remove Missing Values
# -------------------------------
ml_data <- na.omit(ml_data)

cat("Training Records :",nrow(ml_data),"\n")

# -------------------------------
# Train Test Split
# -------------------------------
set.seed(123)

trainIndex <-
  createDataPartition(
    ml_data$passFailStatus,
    p=0.80,
    list=FALSE
  )

trainData <-
  ml_data[trainIndex,]

testData <-
  ml_data[-trainIndex,]

# -------------------------------
# Random Forest Model
# -------------------------------
rf_model <-
  randomForest(
    passFailStatus~.,
    data=trainData,
    ntree=300,
    importance=TRUE
  )

print(rf_model)

# -------------------------------
# Prediction
# -------------------------------
prediction <-
  predict(
    rf_model,
    testData
  )

# -------------------------------
# Accuracy
# -------------------------------
confusion <-
  confusionMatrix(
    prediction,
    testData$passFailStatus
  )

print(confusion)

cat("\nAccuracy : ",
    round(confusion$overall["Accuracy"]*100,2),
    "%\n")

# -------------------------------
# Variable Importance
# -------------------------------
importance(rf_model)

varImpPlot(rf_model)

# -------------------------------
# Save Model
# -------------------------------
saveRDS(
  rf_model,
  "student_performance_model.rds"
)

# -------------------------------
# Save Prediction
# -------------------------------
prediction_result <-
  testData

prediction_result$Predicted_Result <-
  prediction

write.csv(
  prediction_result,
  "prediction_results.csv",
  row.names=FALSE
)

cat("\nMachine Learning Completed Successfully.\n")