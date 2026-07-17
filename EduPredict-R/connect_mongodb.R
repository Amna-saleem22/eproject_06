# ==========================================
# EduPredict
# MongoDB Connection
# File: connect_mongodb.R
# ==========================================

# Load Package
library(mongolite)

# MongoDB Connection
student_collection <- mongo(
  collection = "datastudents",
  db = "studentdata",
  url = "mongodb://localhost:27017"
)

# Read All Students
students <- student_collection$find()

# Show First 10 Records
head(students, 10)

# Structure of Data
str(students)

# Summary
summary(students)

# Total Students
cat("Total Students :", nrow(students))