# ==========================================
# EduPredict
# File : data_cleaning.R
# ==========================================

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

cat("Total Records Before Cleaning :", nrow(students), "\n")

# ==========================================
# Remove Duplicate Student IDs
# ==========================================

students <- students %>%
  distinct(studentId, .keep_all = TRUE)

# ==========================================
# Remove Empty Student IDs
# ==========================================

students <- students %>%
  filter(studentId != "")

# ==========================================
# Remove Empty First Name
# ==========================================

students <- students %>%
  filter(firstName != "")

# ==========================================
# Remove Empty Last Name
# ==========================================

students <- students %>%
  filter(lastName != "")

# ==========================================
# Remove Empty Class
# ==========================================

students <- students %>%
  filter(class != "")

# ==========================================
# Remove Empty Section
# ==========================================

students <- students %>%
  filter(section != "")

# ==========================================
# Remove Invalid Age
# ==========================================

students <- students %>%
  filter(age >= 4 & age <= 20)

# ==========================================
# Attendance Percentage
# ==========================================

students$attendancePercentage <-
  as.numeric(gsub("%","",students$attendancePercentage))

students <- students %>%
  filter(attendancePercentage >=0 &
           attendancePercentage <=100)

# ==========================================
# Remove Empty Gender
# ==========================================

students <- students %>%
  filter(gender != "")

# ==========================================
# Remove Empty Father Name
# ==========================================

students <- students %>%
  filter(fatherName != "")

# ==========================================
# Remove Empty Mobile
# ==========================================

students <- students %>%
  filter(parentMobileNumber != "")

# ==========================================
# Remove Empty Email
# ==========================================

students <- students %>%
  filter(parentEmail != "")

# ==========================================
# Remove Empty Address
# ==========================================

students <- students %>%
  filter(completeAddress != "")

# ==========================================
# Remove Empty City
# ==========================================

students <- students %>%
  filter(city != "")

# ==========================================
# Remove Empty Province
# ==========================================

students <- students %>%
  filter(province != "")

# ==========================================
# Remove Empty Postal Code
# ==========================================

students <- students %>%
  filter(postalCode != "")

cat("Total Records After Cleaning :", nrow(students), "\n")

# ==========================================
# Preview Clean Data
# ==========================================

head(students)

summary(students)

str(students)

# ==========================================
# Save Clean Dataset
# ==========================================

write.csv(
  students,
  "clean_students.csv",
  row.names = FALSE
)

cat("Clean Dataset Saved Successfully.\n")