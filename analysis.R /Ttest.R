# Define columns
categorical_columns <- c("gender", "parental.level.of.education", "lunch", "test.preparation.course")
numeric_columns <- c("math.score", "reading.score", "writing.score", "cgpa_score", "package_score")


# Function to perform t-test
perform_t_test <- function(data, category_column, numeric_column) {
  t_test_result <- t.test(data[[numeric_column]] ~ data[[category_column]])
  return(t_test_result)
}


# Perform t-tests
t_test_results <- list()
for (category_col in categorical_columns) {
  for (numeric_col in numeric_columns) {
    t_test_result <- perform_t_test(student_cleaned, category_col, numeric_col)
    t_test_results[[paste(category_col, numeric_col, sep = " vs. ")]] <- t_test_result
  }
}


# Printing the results of t-tests
cat("T-Test Results:\n")
for (test_name in names(t_test_results)) {
  cat(test_name, "\n")
  cat("-------------------------------------------------------------\n")
  cat(t_test_results[[test_name]], "\n\n")
}

}
