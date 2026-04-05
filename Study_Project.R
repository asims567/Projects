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

p_cor <- ggplot(data, aes(x = study_hours, y = exam_scores)) +
  geom_point(size = 3, color = "blue") + 
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(title = "Correlation: Study Hours vs Exam Scores", 
       x = "Study Hours", 
       y = "Exam Scores") +
  theme_classic() 

print(p_cor)

r_value <- round(cor(data$study_hours, data$exam_scores), 2)
p_cor + annotate("text", x = 3, y = 85, label = paste("r =", r_value), size = 5, color = "black")

data$group <- ifelse(data$study_hours < 5, "Low", "High")
data$pass_fail <- ifelse(data$exam_scores >= 65, "Pass", "Fail")

p_chi <- ggplot(data, aes(x = group, fill = pass_fail)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Chi-Square: Pass/Fail by Study Group",
    x = "Study Group",
    y = "Number of Students"
  ) +
  scale_fill_manual(values = c("Fail" = "#d62728", "Pass" = "#2ca02c")) +
  theme_classic()

print(p_chi)

data$group <- ifelse(data$study_hours < 5, "Low", "High")

t_result <- t.test(exam_scores ~ group, data = data)
print(t_result)

t_value <- t_result$statistic
p_value <- t_result$p.value

print(t_value)
print(p_value)

library(ggplot2)

p_ttest <- ggplot(data, aes(x = group, y = exam_scores, fill = group)) +
  geom_boxplot() +
  labs(
    title = "t-test: Exam Scores by Study Group",
    x = "Study Group",
    y = "Exam Scores"
  ) +
  scale_fill_manual(values = c("Low" = "#1f77b4", "High" = "#ff7f0e")) +
  theme_classic()

print(p_ttest)
