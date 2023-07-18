# Boxplot of CGPA by Gender
ggplot(student, aes(x = gender, y = cgpa, fill = gender)) +
  geom_boxplot() +
  labs(title = "Boxplot of CGPA by Gender", x = "Gender", y = "CGPA")

# Boxplot of Package by Gender
ggplot(student, aes(x = gender, y = package, fill = gender)) +
  geom_boxplot() +
  labs(title = "Boxplot of Package by Gender", x = "Gender", y = "Package")



# Density plot of CGPA by Gender
ggplot(student, aes(x = cgpa, fill = gender, color = gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of CGPA by Gender", x = "CGPA")



# Density plot of Package by Gender
ggplot(student, aes(x = package, fill = gender, color = gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Package by Gender", x = "Package")



# Scatter plot of CGPA and Package by Gender
ggplot(student, aes(x = cgpa, y = package, color = gender)) +
  geom_point() +
  labs(title = "Scatter Plot of CGPA and Package by Gender", x = "CGPA", y = "Package")


#Histograms of CGPA by Gender:
ggplot(student, aes(x = cgpa, fill = gender)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.7) +
  facet_wrap(~gender, ncol = 2) +
  labs(title = "Histogram of CGPA by Gender", x = "CGPA", y = "Count")

#Histograms of Package by Gender:
ggplot(student, aes(x = package, fill = gender)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  facet_wrap(~gender, ncol = 2) +
  labs(title = "Histogram of Package by Gender", x = "Package", y = "Count")

#Histograms of CGPA and Package by Test Preparation Course:
ggplot(student, aes(x = cgpa, fill = test_prep_course)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.7) +
  facet_wrap(~test_prep_course, ncol = 2) +
  labs(title = "Histogram of CGPA by Test Preparation Course", x = "CGPA", y = "Count")

ggplot(student, aes(x = package, fill = test_prep_course)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  facet_wrap(~test_prep_course, ncol = 2) +
  labs(title = "Histogram of Package by Test Preparation Course", x = "Package", y = "Count")



