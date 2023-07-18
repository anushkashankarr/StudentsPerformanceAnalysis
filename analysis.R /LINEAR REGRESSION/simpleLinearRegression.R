# Simple Linear Regression
ggplot(student, aes(x = reading_score, y = writing_score, color = gender)) +
  geom_point() +
  geom_smooth(aes(group = gender), method = "lm") +
  labs(x = "Reading Score", y = "Writing Score")

male_fit_read_write <- lm(writing_score ~ reading_score, subset = gender == "male", data = student)
summary(male_fit_read_write)

female_fit_read_write <- lm(writing_score ~ reading_score, subset = gender == "female", data = student)
summary(female_fit_read_write)

ggplot(student, aes(x = reading_score, y = math_score, color = gender)) +
  geom_point() +
  geom_smooth(aes(group = gender), method = "lm") +
  labs(x = "Reading Score", y = "Math Score")

male_fit_math_read <- lm(math_score ~ reading_score, subset = gender == "male", data = student)
summary(male_fit_math_read)

female_math_read <- lm(math_score ~ reading_score, subset = gender == "female", data = student)
summary(female_math_read)

ggplot(student, aes(x = writing_score, y = math_score, color = gender)) +
  geom_point() +
  geom_smooth(aes(group = gender), method = "lm") +
  labs(x = "Writing Score", y = "Math Score")

male_fit_math_write <- lm(math_score ~ writing_score, subset = gender == "male", data = student)
summary(male_fit_math_write)

female_math_write <- lm(math_score ~ writing_score, subset = gender == "female", data = student)
summary(female_math_write)
