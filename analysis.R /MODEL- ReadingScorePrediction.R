# Model training and evaluation
test_fit_reading_read <- lm(reading_score ~ gender + writing_score + test_prep_course, data = student_cleaned)
summary(test_fit_reading_read)

# Prediction using the model
predict_test_reading_read <- predict(test_fit_reading_read, interval = "prediction")
actual_pred_test_2 <- data.frame(cbind(prediction = predict_test_reading_read, actual = student_cleaned$reading_score))
head(actual_pred_test_2, 10)
cor(actual_pred_test_2)

# Data splitting
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(student_cleaned), replace = TRUE, prob = c(0.75, 0.25))
train <- student_cleaned[sample, ]
test <- student_cleaned[!sample, ]

# Train a linear regression model using the training data
lm_practice_reading <- lm(formula = reading_score ~ gender + writing_score + test_prep_course, data = train)

# Make predictions using the test data
lm_predict_reading <- predict(lm_practice_reading, newdata = test)

# Compare predicted and actual values
actual_pred_reading <- data.frame(cbind(prediction = lm_predict_reading, actual = test$reading_score))
head(actual_pred_reading, 10)
cor(actual_pred_reading)
