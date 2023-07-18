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
