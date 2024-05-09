library(car)
library(carData)
library(ggplot2)
library(ggcorrplot)
# Import data
setwd("/Users/icaroredepaolini/Personale/uni/AndroidAuto_vs_CarPlay/gaze_analysis/analysis_outputs")

df <- read.csv("gaze_analysis_data.csv")

df <- df[df$taskID != "T0", ]
tot <- df[df$subjectID != "26", ]$time_on_task

upper_bound <- boxplot.stats(tot)$stats[5]
lower_bound <- boxplot.stats(tot)$stats[1]

# Create boxplot for time on task by platform
ggplot(df, aes(x = platformID, y = time_on_task)) +
    geom_boxplot(fill = c("skyblue", "lightgreen"), color = c("darkblue", "darkgreen")) +
    xlab("Platform") +
    ylim(0, 100) +
    ylab("Time on task") +
    ggtitle("Time on task by platform")

ggsave("time_on_task_plat.png")
# Create boxplot for time on task by task
ggplot(df, aes(x = taskID, y = time_on_task)) +
    geom_boxplot(fill = c("#F7766D", "#00BB38", "#629DFF"), color = c("darkred", "darkgreen", "darkblue")) +
    xlab("Task") +
    ylim(0, 100) +
    ylab("Time on task") +
    ggtitle("Time on task by task")

ggsave("time_on_task_by_task.png")

# Create boxplot for time on task by platform and task
ggplot(df, aes(x = platformID, y = time_on_task, fill = taskID)) +
    geom_boxplot() +
    xlab("Platform") +
    ylim(0, 100) +
    ylab("Time on task") +
    ggtitle("Time on task by platform and task")

ggsave("time_on_task_boxplot.png")

# Anova for time on task by platform and task

aov_time_on_task <- Anova(lm(time_on_task ~ platformID * taskID, data = df), type = "III")

aov_game_error <- Anova(lm(average_game_error ~ platformID * taskID, data = df), type = "III")

t_T1 <- t.test(df$time_on_task[df$platformID == "AA" & df$taskID == "T1"], df$time_on_task[df$platformID == "CP" & df$taskID == "T1"], alternative = "two.sided", paired = TRUE, var.equal = FALSE)

t_T2 <- t.test(df$time_on_task[df$platformID == "AA" & df$taskID == "T2"], df$time_on_task[df$platformID == "CP" & df$taskID == "T2"], alternative = "two.sided", paired = TRUE, var.equal = FALSE)

t_T3 <- t.test(df$time_on_task[df$platformID == "AA" & df$taskID == "T3"], df$time_on_task[df$platformID == "CP" & df$taskID == "T3"], alternative = "two.sided", paired = TRUE, var.equal = FALSE)

# P-value correction
number_of_tests <- 3
alpha <- 0.05
p_adjust <- alpha / number_of_tests

# Check if the t-test is significant
print(t_T1$p.value < p_adjust)
print(t_T2$p.value < p_adjust)
print(t_T3$p.value < p_adjust) # Only T3 is significant

# Check cor btw time on task and game error
cor(df$time_on_task, df$average_game_error)
# Check task and game error by platform
cor(df$time_on_task[df$platformID == "AA"], df$average_game_error[df$platformID == "AA"])

cor(df$time_on_task[df$platformID == "CP"], df$average_game_error[df$platformID == "CP"])


# Check cor btw time on task and game error by task
cor(df$time_on_task[df$taskID == "T1"], df$average_game_error[df$taskID == "T1"])

cor(df$time_on_task[df$taskID == "T2"], df$average_game_error[df$taskID == "T2"])

cor(df$time_on_task[df$taskID == "T3"], df$average_game_error[df$taskID == "T3"])
