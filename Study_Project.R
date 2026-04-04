library(ggplot2)

study_hours <- c(2, 3, 4, 5, 6, 7, 8, 2, 4, 6, 5, 7, 3, 8, 9)
exam_scores <- c(50, 55, 60, 65, 70, 75, 80, 52, 63, 72, 67, 78, 58, 82, 90)

data <- data.frame(study_hours, exam_scores)

ggplot(data, aes(x = study_hours, y = exam_scores)) +
  geom_point()

ggplot(data, aes(x = study_hours, y = exam_scores)) + 
  geom_point(size = 3, color = "darkblue") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs( 
    title = "Study Hours vs Exam Scores", 
    x = "Study Hours", 
    y = "Exam Score") +
  theme_minimal()

cor(data$study_hours, data$exam_scores)
cor.test(data$study_hours, data$exam_scores)
