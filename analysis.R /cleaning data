student <- student %>% rename(parental_education = parental.level.of.education,
                              math_score = math.score,
                              writing_score = writing.score,
                              reading_score = reading.score,
                              ethnicity = race.ethnicity,
                              test_prep_course = test.preparation.course)


student_cleaned <- student %>%
  filter(!is.na(cgpa) & !is.na(package))
