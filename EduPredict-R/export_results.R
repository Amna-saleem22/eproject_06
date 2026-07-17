# ==========================================
# EduPredict
# File : 07_export_results.R
# ==========================================

library(mongolite)

# MongoDB Connection
student_collection <- mongo(
  collection = "datastudents",
  db = "studentdata",
  url = "mongodb://localhost:27017"
)

students <- student_collection$find()

# Export CSV
write.csv(
  students,
  "Final_Student_Dataset.csv",
  row.names = FALSE
)

# Export RDS
saveRDS(
  students,
  "Final_Student_Dataset.rds"
)

cat("====================================\n")
cat("Export Completed Successfully\n")
cat("====================================\n")
cat("CSV File : Final_Student_Dataset.csv\n")
cat("RDS File : Final_Student_Dataset.rds\n")