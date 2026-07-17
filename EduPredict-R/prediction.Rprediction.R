# ==========================================
# EduPredict
# File : 06_prediction.R
# Student Performance Prediction
# ==========================================

# -------------------------------
# Required Packages
# -------------------------------
library(mongolite)
library(randomForest)

# -------------------------------
# MongoDB Connection
# -------------------------------
student_collection <- mongo(
  collection = "datastudents",
  db = "studentdata",
  url = "mongodb://localhost:27017"
)

# -------------------------------
# Read Student Data
# -------------------------------
students <- student_collection$find()

cat("Student Data Loaded Successfully\n")

# -------------------------------
# Attendance to Numeric
# -------------------------------
students$attendancePercentage <-
  as.numeric(gsub("%", "", students$attendancePercentage))

# -------------------------------
# Load Saved Machine Learning Model
# -------------------------------
rf_model <- readRDS("student_performance_model.rds")

cat("Machine Learning Model Loaded Successfully\n")

# -------------------------------
# Remove Columns Not Required
# -------------------------------
prediction_data <- students

prediction_data$passFailStatus <- NULL

# Remove non-numeric columns
prediction_data <- prediction_data[
  ,
  sapply(prediction_data, is.numeric)
]

# Remove unwanted numeric columns
prediction_data$rollNumber <- NULL
prediction_data$maxPossibleMarks <- NULL

# -------------------------------
# Predict Student Result
# -------------------------------
predicted_result <- predict(
  rf_model,
  prediction_data
)

# -------------------------------
# Add Prediction Column
# -------------------------------
students$Predicted_Result <- predicted_result

# -------------------------------
# Show First 10 Predictions
# -------------------------------
print(
  students[
    ,
    c(
      "studentId",
      "firstName",
      "lastName",
      "class",
      "percentage",
      "Predicted_Result"
    )
  ][1:10, ]
)

# -------------------------------
# Save Prediction File
# -------------------------------
write.csv(
  students,
  "student_prediction_results.csv",
  row.names = FALSE
)

cat("\nPrediction Completed Successfully.\n")
cat("\nPrediction file saved as student_prediction_results.csv\n")