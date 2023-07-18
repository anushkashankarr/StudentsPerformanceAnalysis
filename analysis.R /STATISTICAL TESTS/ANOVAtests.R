# Define columns
categorical_columns <- c("gender", "parental.level.of.education", "lunch", "test.preparation.course")
numeric_columns <- c("math.score", "reading.score", "writing.score", "cgpa_score", "package_score")


# Function to perform ANOVA test
perform_anova_test <- function(data, category_column, numeric_column) {
  anova_result <- aov(data[[numeric_column]] ~ data[[category_column]])
  return(summary(anova_result))
}


# Perform ANOVA tests
anova_results <- list()
for (category_col in categorical_columns) {
  for (numeric_col in numeric_columns) {
    anova_result <- perform_anova_test(student_cleaned, category_col, numeric_col)
    anova_results[[paste(category_col, numeric_col, sep = " vs. ")]] <- anova_result
  }
}


# Printing the results of ANOVA tests
cat("ANOVA Test Results:\n")
for (test_name in names(anova_results)) {
  cat(test_name, "\n")
  cat("-------------------------------------------------------------\n")
  cat(anova_results[[test_name]], "\n\n")
}
