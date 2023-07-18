# Model training and evaluation
test_fit_math_read <- lm(math_score ~ gender + writing_score + test_prep_course, data = student_cleaned)
summary(test_fit_math_read)

# Prediction using the model
predict_test_math_read <- predict(test_fit_math_read, interval = "prediction")
actual_pred_test_3 <- data.frame(cbind(prediction = predict_test_math_read, actual = student_cleaned$math_score))
head(actual_pred_test_3, 10)
cor(actual_pred_test_3)

# Data splitting
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(student_cleaned), replace = TRUE, prob = c(0.75, 0.25))
train <- student_cleaned[sample, ]
test <- student_cleaned[!sample, ]

# Train a linear regression model using the training data
lm_practice_math <- lm(formula = math_score ~ gender + writing_score + test_prep_course, data = train)

# Make predictions using the test data
lm_predict_math <- predict(lm_practice_math, newdata = test)

# Compare predicted and actual values
actual_pred_math <- data.frame(cbind(prediction = lm_predict_math, actual = test$math_score))
head(actual_pred_math, 10)
cor(actual_pred_math)
