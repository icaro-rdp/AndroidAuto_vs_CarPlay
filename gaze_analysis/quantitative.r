library(ggplot2)
library(ggcorrplot)
library(ez)
library(dplyr)
library(gridExtra)

# Import data
setwd("/Users/icaroredepaolini/Personale/uni/AndroidAuto_vs_CarPlay/gaze_analysis/analysis_outputs")

df <- read.csv("gaze_analysis_data.csv")

baseline <- df[df$taskID == "T0", ][, c("subjectID", "average_game_error")]
df <- df[df$taskID != "T0", ]

# subtract to each subjectID his own baseline error
df$average_game_error <- df$average_game_error - baseline$average_game_error[match(df$subjectID, baseline$subjectID)]


# Create boxplot for time on task by platform
ggplot(df, aes(x = platformID, y = time_on_task)) +
    geom_boxplot(fill = c("skyblue", "lightgreen"), color = c("darkblue", "darkgreen")) +
    xlab("Platform") +
    ylim(0, 150) +
    ylab("Time on task") +
    ggtitle("Time on task by platform")

ggsave("time_on_task_plat.png")
# Create boxplot for time on task by task
ggplot(df, aes(x = taskID, y = time_on_task)) +
    geom_boxplot(fill = c("#F7766D", "#00BB38", "#629DFF"), color = c("darkred", "darkgreen", "darkblue")) +
    xlab("Task") +
    ylim(0, 150) +
    ylab("Time on task") +
    ggtitle("Time on task by task")

ggsave("time_on_task_by_task.png")

# Create boxplot for time on task by platform and task
ggplot(df, aes(x = platformID, y = time_on_task, fill = taskID)) +
    geom_boxplot() +
    xlab("Platform") +
    ylim(0, 150) +
    ylab("Time on task") +
    ggtitle("Time on task by platform and task")

ggsave("time_on_task_boxplot.png")

# Anova for time on task by platform and task

df_anova <- df[, c("subjectID", "platformID", "taskID", "time_on_task", "average_game_error")]
colnames(df_anova) <- c("subjectID", "platform", "task", "time_on_task", "average_game_error")

# Convert to factors for ANOVA
df_anova$subjectID <- as.factor(df_anova$subjectID)
df_anova$platform <- as.factor(df_anova$platform)
df_anova$task <- as.factor(df_anova$task)


# Library for not reshaping the data
anova_tot <- ezANOVA(
    data = df_anova,
    dv = time_on_task,
    wid = subjectID,
    within = .(platform, task)
)

anova_game_err <- ezANOVA(
    data = df_anova,
    dv = average_game_error,
    wid = subjectID,
    within = .(platform, task)
)


# create a table with the result of ANOVA and save it as a png
anova_tot_table <- data.frame("Source" = c("platform", "task", "platform:task"), "p" = c(format(anova_tot$ANOVA$p[1], scientific = FALSE), format(anova_tot$ANOVA$p[2], scientific = FALSE), format(anova_tot$ANOVA$p[3], scientific = FALSE)))
png("anova_tot_table.png", width = 2000, height = 2000, res = 350, bg = "white")
grid.table(anova_tot_table)
dev.off()

anova_game_err_table <- data.frame("Source" = c("platform", "task", "platform:task"), "p" = c(format(anova_game_err$ANOVA$p[1], scientific = FALSE), format(anova_game_err$ANOVA$p[2], scientific = FALSE), format(anova_game_err$ANOVA$p[3], scientific = FALSE)))
png("anova_game_err_table.png", width = 2000, height = 2000, res = 350, bg = "white")
grid.table(anova_game_err_table)
dev.off()

# plot 3 windows of boxplots for the time on task variable by platform
ggplot(df_anova, aes(x = platform, y = time_on_task, fill = task)) +
    geom_boxplot() +
    facet_wrap(~task) +
    xlab("Platform") +
    ylab("Time on task") +
    ggtitle("Time on task by platform and task")

ggsave("time_on_task_boxplot_facet.png")

# plot the interaction between platform and task for time on task
ggplot(df_anova, aes(x = platform, y = time_on_task, color = task)) +
    geom_point() +
    geom_line(aes(group = task)) +
    xlab("Platform") +
    ylab("Time on task") +
    ggtitle("Time on task by platform and task")

ggsave("time_on_task_interaction.png")

t_T1 <- t.test(df$time_on_task[df$platformID == "AA" & df$taskID == "T1"], df$time_on_task[df$platformID == "CP" & df$taskID == "T1"], alternative = "two.sided", paired = TRUE, var.equal = FALSE)

t_T2 <- t.test(df$time_on_task[df$platformID == "AA" & df$taskID == "T2"], df$time_on_task[df$platformID == "CP" & df$taskID == "T2"], alternative = "two.sided", paired = TRUE, var.equal = FALSE)

t_T3 <- t.test(df$time_on_task[df$platformID == "AA" & df$taskID == "T3"], df$time_on_task[df$platformID == "CP" & df$taskID == "T3"], alternative = "two.sided", paired = TRUE, var.equal = FALSE)

# P-value correction
number_of_tests <- 3
alpha <- 0.05
p_adjust <- alpha / number_of_tests

# Check if the t-test is significant
print("Time on task:")
print(t_T1$p.value < p_adjust)
print(t_T2$p.value < p_adjust)
print(t_T3$p.value < p_adjust) # ! Only T3 is significant

tot_t_tests_table <- data.frame("Task" = c("T1", "T2", "T3"), "p" = c(t_T1$p.value, t_T2$p.value, t_T3$p.value))
png("tot_t_tests_table.png", width = 2000, height = 2000, res = 350, bg = "white")
grid.table(tot_t_tests_table)
dev.off()


t_T1_game_err <- t.test(df$average_game_error[df$platformID == "AA" & df$taskID == "T1"], df$average_game_error[df$platformID == "CP" & df$taskID == "T1"], alternative = "two.sided", paired = TRUE, var.equal = FALSE)

t_T2_game_err <- t.test(df$average_game_error[df$platformID == "AA" & df$taskID == "T2"], df$average_game_error[df$platformID == "CP" & df$taskID == "T2"], alternative = "two.sided", paired = TRUE, var.equal = FALSE)

t_T3_game_err <- t.test(df$average_game_error[df$platformID == "AA" & df$taskID == "T3"], df$average_game_error[df$platformID == "CP" & df$taskID == "T3"], alternative = "two.sided", paired = TRUE, var.equal = FALSE)

# Check if the t-test is significant
print("Game error:")
print(t_T1_game_err$p.value < p_adjust)
print(t_T2_game_err$p.value < p_adjust)
print(t_T3_game_err$p.value < p_adjust)
# ! No significant differences in game error between platforms for any task found

tot_t_tests_table_game_err <- data.frame("Task" = c("T1", "T2", "T3"), "p" = c(t_T1_game_err$p.value, t_T2_game_err$p.value, t_T3_game_err$p.value))
png("tot_t_tests_table_game_err.png", width = 2000, height = 2000, res = 350, bg = "white")
grid.table(tot_t_tests_table_game_err)
dev.off()


# Create a data frame with your correlation values and labels
cor_data <- data.frame(
    Group = c("Overall", "Platform AA", "Platform CP", "Task T1", "Task T2", "Task T3"),
    Correlation = c(
        cor(df$time_on_task, df$average_game_error),
        cor(df$time_on_task[df$platformID == "AA"], df$average_game_error[df$platformID == "AA"]),
        cor(df$time_on_task[df$platformID == "CP"], df$average_game_error[df$platformID == "CP"]),
        cor(df$time_on_task[df$taskID == "T1"], df$average_game_error[df$taskID == "T1"]),
        cor(df$time_on_task[df$taskID == "T2"], df$average_game_error[df$taskID == "T2"]),
        cor(df$time_on_task[df$taskID == "T3"], df$average_game_error[df$taskID == "T3"])
    )
)
png("cor_tot_game_err_table.png", width = 2000, height = 2000, res = 350, bg = "white")
grid.table(cor_data)
dev.off()
