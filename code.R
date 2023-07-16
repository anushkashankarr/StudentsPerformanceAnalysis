# -----------------FINDING EFFECTS OF VARIOUS EVENTS ON CHILD'S PERFORMANCE ----------------------


# Load required packages
library(dplyr)
library(ggplot2)

# Read the data
student <- read.csv("~/R datasets/StudentsPerformance.csv")

# Data cleaning and manipulation
student <- student %>% rename(parental_education = parental.level.of.education,
                              math_score = math.score,
                              writing_score = writing.score,
                              reading_score = reading.score,
                              ethnicity = race.ethnicity,
                              test_prep_course = test.preparation.course)

# Data exploration
head(student)
colnames(student)
summary(student)
str(student)

# Missing values and duplicates
sum(is.na(student))
duplicates <- student %>% duplicated()
duplicates_count <- table(duplicates)
duplicates_count

# Gender analysis
count_gender <- student %>% count(gender)
freq_gender <- prop.table(table(student$gender))
count_gender
freq_gender

# Ethnicity analysis
count_ethnicity <- student %>% count(ethnicity)
freq_ethnicity <- prop.table(table(student$ethnicity))
count_ethnicity
freq_ethnicity

# Parental education analysis
count_parental_education <- student %>% count(parental_education)
freq_parental <- prop.table(table(student$parental_education))
count_parental_education
freq_parental

# Test prep analysis
count_test_prep <- student %>% count(test_prep_course)
freq_test <- prop.table(table(student$test_prep_course))
count_test_prep
freq_test

# Lunch analysis
count_lunch <- student %>% count(lunch)
freq_lunch <- prop.table(table(student$lunch))
count_lunch
freq_lunch

# Boxplots and outliers
boxplot(student$reading_score)
boxplot(student$writing_score)
boxplot(student$math_score)
which.min(student$math_score)
print(student[60, ])
range(student$reading_score)
range(student$writing_score)
range(student$math_score)
student <- subset(student, math_score != 0)
count(student)
count_gender <- student %>% count(gender)
count_gender

# Distribution analysis
ggplot(data = student, aes(reading_score)) +
  geom_histogram(aes(y = ..density..), color = "darkblue", fill = "lightblue", binwidth = 5) +
  labs(title = "Distribution of Reading Scores", x = "Reading Score", y = "Total Count") +
  geom_density(alpha = 0.3, color = "skyblue", fill = "skyblue")

ggplot(data = student, aes(writing_score)) +
  geom_histogram(aes(y = ..density..), color = "darkgreen", fill = "lightgreen", binwidth = 5) +
  labs(title = "Distribution of Writing Scores", x = "Writing Score", y = "Total Count") +
  geom_density(alpha = 0.2, color = "darkgreen", fill = "lightgreen")

ggplot(data = student, aes(math_score)) +
  geom_histogram(aes(y = ..density..), color = "darkred", fill = "red", binwidth = 5) +
  labs(title = "Distribution of Math Scores", x = "Math Score", y = "Total Count") +
  geom_density(alpha = 0.2, color = "black", fill = "red")

ggplot(student, aes(x = reading_score)) +
  geom_histogram(aes(color = gender, fill = gender), position = "identity", binwidth = 5, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(title = "Distribution of Reading Scores Between Gender", x = "Reading Score", y = "Total Count")

ggplot(student, aes(x = writing_score)) +
  geom_histogram(aes(color = gender, fill = gender), position = "identity", binwidth = 5, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(title = "Distribution of Writing Scores Between Gender", x = "Writing Score", y = "Total Count")

ggplot(student, aes(x = math_score)) +
  geom_histogram(aes(color = gender, fill = gender), position = "identity", binwidth = 5, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(title = "Distribution of Math Scores Between Gender", x = "Math Score", y = "Total Count")

ggplot(student, aes(gender, reading_score, fill = gender, color = gender)) +
  geom_boxplot() +
  labs(title = "Reading Score for Each Gender", x = "Gender", y = "Reading Score") +
  scale_fill_manual(values = c("purple", "lightblue")) +
  scale_color_manual(values = c("black", "gray5"))

ggplot(student, aes(gender, writing_score, fill = gender, color = gender)) +
  geom_boxplot() +
  labs(title = "Writing Score for Each Gender", x = "Gender", y = "Reading Score") +
  scale_fill_manual(values = c("purple", "lightblue")) +
  scale_color_manual(values = c("black", "gray5"))

ggplot(student, aes(gender, math_score, fill = gender, color = gender)) +
  geom_boxplot() +
  labs(title = "Math Score for Each Gender", x = "Gender", y = "Math Score") +
  scale_fill_manual(values = c("purple", "lightblue")) +
  scale_color_manual(values = c("black", "gray5"))

ggplot(student, aes(ethnicity, reading_score, fill = ethnicity, color = ethnicity)) +
  geom_boxplot() +
  labs(title = "Reading Score for Each Ethnicity Group", x = "Ethnicity", y = "Reading Score") +
  scale_fill_manual(values = c("blue", "red", "yellow", "orange", "purple")) +
  scale_color_manual(values = c("hotpink", "purple3", "black", "royalblue3", "violetred1"))

ggplot(student, aes(ethnicity, writing_score, fill = ethnicity, color = ethnicity)) +
  geom_boxplot() +
  labs(title = "Writing Score for Each Ethnicity Group", x = "Ethnicity", y = "Writing Score") +
  scale_fill_manual(values = c("blue", "red", "yellow", "orange", "purple")) +
  scale_color_manual(values = c("hotpink", "purple3", "black", "royalblue3", "violetred1"))

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

# T-testing
t_test <- t.test(reading_score ~ gender, data = student)
t_test

# ANOVA testing
parent_aov <- aov(reading_score ~ parental_education, data = student)
summary(parent_aov)

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

# Exam Score Prediction Models
test_fit_writing_read <- lm(writing_score ~ gender + reading_score + test_prep_course, data = student)
summary(test_fit_writing_read)

predict_test_writing_read <- predict(test_fit_writing_read, interval = "prediction")
actual_pred_test_1 <- data.frame(cbind(prediction = predict_test_writing_read, actual = student$writing_score))
head(actual_pred_test_1, 10)
cor(actual_pred_test_1)

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(student), replace = TRUE, prob = c(0.75, 0.25))
train <- student[sample, ]
test <- student[!sample, ]

lm_practice <- lm(formula = writing_score ~ gender + reading_score + test_prep_course, data = train)
lm_predict <- predict(lm_practice, newdata = test)
actual_pred_1 <- data.frame(cbind(prediction = lm_predict, actual = test$writing_score))
head(actual_pred_1, 10)
cor(actual_pred_1)



# -----------------CGPA and Package Analysis-------------------



student_cleaned <- student %>%
  filter(!is.na(cgpa) & !is.na(package))

summary_table <- student %>%
  group_by(gender) %>%
  summarize(cgpa_mean = mean(cgpa, na.rm = TRUE),
            package_mean = mean(package, na.rm = TRUE),
            cgpa_sd = sd(cgpa),
            package_sd = sd(package))

summary_table

ggplot(student, aes(x = cgpa)) +
  geom_histogram(aes(color = gender, fill = gender), position = "identity", binwidth = 0.5, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(title = "Distribution of CGPA Between Gender", x = "CGPA", y = "Total Count")

ggplot(student, aes(x = package)) +
  geom_histogram(aes(color = gender, fill = gender), position = "identity", binwidth = 1, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(title = "Distribution of Package Between Gender", x = "Package", y = "Total Count")


# Model training and evaluation
set.seed(42)
train_indices <- sample(1:nrow(student_cleaned), 0.7 * nrow(student_cleaned))
train_data <- student_cleaned[train_indices, ]
test_data <- student_cleaned[-train_indices, ]

set.seed(42)
train_indices <- createDataPartition(student$package_score, p = 0.7, list = FALSE)
train_data <- student[train_indices, ]
test_data <- student[-train_indices, ]

# Model Training
model <- lm(package ~ cgpa, data = train_data)

# Model Evaluation
predictions <- predict(model, newdata = test_data)
mse <- mean((predictions - test_data$package_score)^2)

# Model Summary
summary(model)
