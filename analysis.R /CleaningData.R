

#------------------------------------------------------------------- Data cleaning and manipulation-------------------------------------------------------


# making the column names handy.
student <- student %>% rename(parental_education = parental.level.of.education,
                              math_score = math.score,
                              writing_score = writing.score,
                              reading_score = reading.score,
                              ethnicity = race.ethnicity,
                              test_prep_course = test.preparation.course)



# Missing values and duplicates
sum(is.na(student))
duplicates <- student %>% duplicated()
duplicates_count <- table(duplicates)
duplicates_count


student_cleaned <- student %>%
  rename(package_score = package, cgpa_score = cgpa) %>%
  mutate(across(where(is.character), as.factor))

# Fill missing values with mean
numeric_columns <- c("math.score", "reading.score", "writing.score")
categorical_columns <- c("parental.level.of.education", "lunch", "test.preparation.course", "gender", "race.ethnicity")

for (col in numeric_columns) {
  student_cleaned[[col]] <- ifelse(is.na(student_cleaned[[col]]), mean(student_cleaned[[col]], na.rm = TRUE), student_cleaned[[col]])
}

for (col in categorical_columns) {
  student_cleaned[[col]] <- ifelse(is.na(student_cleaned[[col]]), as.character(mode(student_cleaned[[col]], na.rm = TRUE)), student_cleaned[[col]])
}


# Verify missing values have been filled
sum(is.na(student_cleaned))


# Updated dataset with cleaned data
head(student_cleaned)


# Data cleaning and manipulation of cgpa and package column
student_cleaned <- student %>%
  rename(package_score = package, cgpa_score = cgpa) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))



