# Model training and evaluation
test_fit_writing_read <- lm(writing_score ~ gender + reading_score + test_prep_course, data = student_cleaned)
summary(test_fit_writing_read)

# Prediction using the model
predict_test_writing_read <- predict(test_fit_writing_read, interval = "prediction")
actual_pred_test_1 <- data.frame(cbind(prediction = predict_test_writing_read, actual = student_cleaned$writing_score))
head(actual_pred_test_1, 10)
cor(actual_pred_test_1)

# Data splitting
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(student_cleaned), replace = TRUE, prob = c(0.75, 0.25))
train <- student_cleaned[sample, ]
test <- student_cleaned[!sample, ]

# Train a linear regression model using the training data
lm_practice <- lm(formula = writing_score ~ gender + reading_score + test_prep_course, data = train)

# Make predictions using the test data
lm_predict <- predict(lm_practice, newdata = test)

# Compare predicted and actual values
actual_pred_1 <- data.frame(cbind(prediction = lm_predict, actual = test$writing_score))
head(actual_pred_1, 10)
cor(actual_pred_1)
