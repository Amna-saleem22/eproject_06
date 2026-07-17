# ==========================================
# EduPredict
# File : exploratory_analysis.R
# ==========================================

# Load Packages
library(mongolite)
library(dplyr)

# ------------------------------------------
# MongoDB Connection
# ------------------------------------------

student_collection <- mongo(
  collection = "datastudents",
  db = "studentdata",
  url = "mongodb://localhost:27017"
)

# ------------------------------------------
# Read Data
# ------------------------------------------

students <- student_collection$find()

cat("=====================================\n")
cat("EXPLORATORY DATA ANALYSIS (EDA)\n")
cat("=====================================\n\n")

# ------------------------------------------
# Dataset Information
# ------------------------------------------

cat("Total Students :", nrow(students), "\n")
cat("Total Columns :", ncol(students), "\n\n")

# ------------------------------------------
# Structure
# ------------------------------------------

str(students)

# ------------------------------------------
# Summary
# ------------------------------------------

summary(students)

# ==========================================
# Gender Distribution
# ==========================================

cat("\nGender Distribution\n")
print(table(students$gender))

# ==========================================
# Class Distribution
# ==========================================

cat("\nStudents in Each Class\n")
print(table(students$class))

# ==========================================
# Section Distribution
# ==========================================

cat("\nStudents in Each Section\n")
print(table(students$section))

# ==========================================
# City Distribution
# ==========================================

cat("\nStudents by City\n")
print(table(students$city))

# ==========================================
# Province Distribution
# ==========================================

cat("\nStudents by Province\n")
print(table(students$province))

# ==========================================
# Religion Distribution
# ==========================================

cat("\nReligion Distribution\n")
print(table(students$religion))

# ==========================================
# Blood Group Distribution
# ==========================================

cat("\nBlood Group Distribution\n")
print(table(students$bloodGroup))

# ==========================================
# Average Age
# ==========================================

cat("\nAverage Age :", mean(students$age), "\n")

cat("Minimum Age :", min(students$age), "\n")

cat("Maximum Age :", max(students$age), "\n")

# ==========================================
# Attendance Analysis
# ==========================================

students$attendancePercentage <-
  as.numeric(gsub("%","",students$attendancePercentage))

cat("\nAverage Attendance :",
    round(mean(students$attendancePercentage),2), "%\n")

cat("Highest Attendance :",
    max(students$attendancePercentage), "%\n")

cat("Lowest Attendance :",
    min(students$attendancePercentage), "%\n")

# ==========================================
# Missing Values
# ==========================================

cat("\nMissing Values\n")
print(colSums(is.na(students)))

# ==========================================
# Duplicate Student IDs
# ==========================================

duplicate_students <-
  students %>%
  group_by(studentId) %>%
  filter(n()>1)

cat("\nDuplicate Student IDs :", nrow(duplicate_students), "\n")

# ==========================================
# Save EDA Report
# ==========================================

write.csv(
  students,
  "EDA_Dataset.csv",
  row.names = FALSE
)

cat("\nEDA Completed Successfully.\n")