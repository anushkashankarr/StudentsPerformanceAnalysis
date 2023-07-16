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
