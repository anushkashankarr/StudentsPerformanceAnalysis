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
