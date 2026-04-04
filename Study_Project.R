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

data$group_anova <- cut(data$study_hours, breaks=c(0,4,6,10), labels=c("Low","Medium","High"))

anova_result <- aov(exam_scores ~ group_anova, data = data)
summary(anova_result)

p_anova <- ggplot(data, aes(x = group_anova, y = exam_scores, fill = group_anova)) +
  geom_boxplot() +
  labs(title = "ANOVA: Exam Scores by Study Hours Group",
       x = "Study Hours Group",
       y = "Exam Scores") +
  theme_classic() +
  scale_fill_manual(values = c("Low"="#1f77b4", "Medium"="#2ca02c", "High"="#ff7f0e"))

print(p_anova)



