# Mean and standard deviation tables
gender_table <- student %>%
  group_by(gender) %>%
  summarize(reading_mean = mean(reading_score, na.rm = TRUE),
            writing_mean = mean(writing_score, na.rm = TRUE),
            math_mean = mean(math_score, na.rm = TRUE),
            reading_sd = sd(reading_score),
            writing_sd = sd(writing_score),
            math_sd = sd(math_score))

gender_table

ethnic_table <- student %>%
  group_by(ethnicity) %>%
  summarize(reading_mean = mean(reading_score, na.rm = TRUE),
            writing_mean = mean(writing_score, na.rm = TRUE),
            math_mean = mean(math_score, na.rm = TRUE),
            reading_sd = sd(reading_score),
            writing_sd = sd(writing_score),
            math_sd = sd(math_score))

ethnic_table

education_table <- student %>%
  group_by(parental_education) %>%
  summarize(reading_mean = mean(reading_score, na.rm = TRUE),
            writing_mean = mean(writing_score, na.rm = TRUE),
            math_mean = mean(math_score, na.rm = TRUE),
            reading_sd = sd(reading_score),
            writing_sd = sd(writing_score),
            math_sd = sd(math_score))

education_table

lunch_table <- student %>%
  group_by(lunch) %>%
  summarize(reading_mean = mean(reading_score, na.rm = TRUE),
            writing_mean = mean(writing_score, na.rm = TRUE),
            math_mean = mean(math_score, na.rm = TRUE),
            reading_sd = sd(reading_score),
            writing_sd = sd(writing_score),
            math_sd = sd(math_score))

lunch_table

test_prep_table <- student %>%
  group_by(test_prep_course) %>%
  summarize(reading_mean = mean(reading_score, na.rm = TRUE),
            writing_mean = mean(writing_score, na.rm = TRUE),
            math_mean = mean(math_score, na.rm = TRUE),
            reading_sd = sd(reading_score),
            writing_sd = sd(writing_score),
            math_sd = sd(math_score))

test_prep_table

ethnic_gender_table <- student %>%
  group_by(ethnicity, gender) %>%
  summarize(reading_mean = mean(reading_score, na.rm = TRUE),
            writing_mean = mean(writing_score, na.rm = TRUE),
            math_mean = mean(math_score, na.rm = TRUE),
            reading_sd = sd(reading_score),
            writing_sd = sd(writing_score),
            math_sd = sd(math_score))

ethnic_gender_table

test_lunch_table <- student %>%
  group_by(test_prep_course, lunch) %>%
  summarize(reading_mean = mean(reading_score, na.rm = TRUE),
            writing_mean = mean(writing_score, na.rm = TRUE),
            math_mean = mean(math_score, na.rm = TRUE),
            reading_sd = sd(reading_score),
            writing_sd = sd(writing_score),
            math_sd = sd(math_score))

test_lunch_table
