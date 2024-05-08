library(car)
library(carData)
library(ggplot2)
# Import data
df <- read.csv("gaze_analysis_data.csv")

df <- df[df$taskID != "T0", ]
tot <- df[df$subjectID != "26", ]$time_on_task

upper_bound <- boxplot.stats(tot)$stats[5]
lower_bound <- boxplot.stats(tot)$stats[1]

# Create boxplot for time on task by platform
ggplot(df, aes(x = platformID, y = time_on_task)) +
    geom_boxplot(fill = c("skyblue", "lightgreen"), color = c("darkblue", "darkgreen")) +
    xlab("Platform") +
    ylab("Time on task") +
    ggtitle("Time on task by platform")

ggsave("time_on_task_plat.png")
# Create boxplot for time on task by task
ggplot(df, aes(x = taskID, y = time_on_task)) +
    geom_boxplot(fill = c("#F7766D", "#00BB38", "#629DFF"), color = c("darkred", "darkgreen", "darkblue")) +
    xlab("Task") +
    ylab("Time on task") +
    ggtitle("Time on task by task")

ggsave("time_on_task_by_task.png")

# Create boxplot for time on task by platform and task
ggplot(df, aes(x = platformID, y = time_on_task, fill = taskID)) +
    geom_boxplot() +
    xlab("Platform") +
    ylab("Time on task") +
    ggtitle("Time on task by platform and task")

ggsave("time_on_task_boxplot.png")

# Check if CP is faster than AA in T3
t.test(df$time_on_task[df$platformID == "AA" & df$taskID == "T3"], df$time_on_task[df$platformID == "CP" & df$taskID == "T3"], alternative = "less", paired = TRUE, var.equal = FALSE)

# Anova for time on task by platform and task
aov_time_on_task <- aov(time_on_task ~ platformID * taskID, data = df)
print(summary(aov_time_on_task))
