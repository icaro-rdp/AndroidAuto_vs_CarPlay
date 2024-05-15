library(ggplot2)
library(ggcorrplot)
library(ez)
library(dplyr)
library(gridExtra)
library(reshape2)

# Import data
setwd("/Users/icaroredepaolini/Personale/uni/AndroidAuto_vs_CarPlay/gaze_analysis/analysis_outputs")

df <- read.csv("gaze_analysis_data.csv")

baseline <- df[df$taskID == "T0", ][, c("subjectID", "average_game_error")]
df <- df[df$taskID != "T0", ]

# subtract to each subjectID his own baseline error
df$average_game_error <- df$average_game_error - baseline$average_game_error[match(df$subjectID, baseline$subjectID)]

# Anova for time on task by platform and task

task_metrics_df <- df[, c("subjectID", "platformID", "taskID", "time_on_task", "average_game_error", "percentage_of_distracting_gazes")]
colnames(task_metrics_df) <- c("subjectID", "platform", "task", "time_on_task", "average_game_error", "percentage_of_distracting_gazes")

# Convert to factors for ANOVA
task_metrics_df$subjectID <- as.factor(task_metrics_df$subjectID)
task_metrics_df$platform <- as.factor(task_metrics_df$platform)
task_metrics_df$task <- as.factor(task_metrics_df$task)


# Library for not reshaping the data
anova_tot <- ezANOVA(
    data = task_metrics_df,
    dv = time_on_task,
    wid = subjectID,
    within = .(platform, task)
)

anova_game_err <- ezANOVA(
    data = task_metrics_df,
    dv = average_game_error,
    wid = subjectID,
    within = .(platform, task)
)

perc_distr_gaze <- ezANOVA(
    data = task_metrics_df,
    dv = percentage_of_distracting_gazes,
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

anova_perc_distr_gaze_table <- data.frame("Source" = c("platform", "task", "platform:task"), "p" = c(format(perc_distr_gaze$ANOVA$p[1], scientific = FALSE), format(perc_distr_gaze$ANOVA$p[2], scientific = FALSE), format(perc_distr_gaze$ANOVA$p[3], scientific = FALSE)))
png("anova_perc_distr_gaze_table.png", width = 2000, height = 2000, res = 350, bg = "white")
grid.table(anova_perc_distr_gaze_table)
dev.off()

# plot 3 windows of boxplots for the time on task variable by platform
ggplot(task_metrics_df, aes(x = platform, y = time_on_task, fill = task)) +
    geom_boxplot() +
    facet_wrap(~task) +
    xlab("Platform") +
    ylab("Time on task") +
    ggtitle("Time on task by platform and task")

ggsave("time_on_task_boxplot_facet.png")

ggplot(task_metrics_df, aes(x = platform, y = percentage_of_distracting_gazes, fill = task)) +
    geom_boxplot() +
    facet_wrap(~task) +
    xlab("Platform") +
    ylab("Percentage of distracting gazes") +
    ggtitle("Percentage of distracting gazes by platform and task")

ggsave("percentage_gaze_boxplot_facet.png")

ggplot(task_metrics_df, aes(
    x = platform, y = average_game_error,
    fill = task
)) +
    geom_boxplot() +
    facet_wrap(~task) +
    xlab("Platform") +
    ylab("Avg game error") +
    ggtitle("Average game error by platform and task")

ggsave("avg_game_err_boxplot_facet.png")


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

# Post hoc analysis for percentage of distracting gazes
t_T1_perc <- t.test(df$percentage_of_distracting_gazes[df$platformID == "AA" & df$taskID == "T1"], df$percentage_of_distracting_gazes[df$platformID == "CP" & df$taskID == "T1"], alternative = "two.sided", paired = TRUE, var.equal = FALSE)

t_T2_perc <- t.test(df$percentage_of_distracting_gazes[df$platformID == "AA" & df$taskID == "T2"], df$percentage_of_distracting_gazes[df$platformID == "CP" & df$taskID == "T2"], alternative = "two.sided", paired = TRUE, var.equal = FALSE)

t_T3_perc <- t.test(df$percentage_of_distracting_gazes[df$platformID == "AA" & df$taskID == "T3"], df$percentage_of_distracting_gazes[df$platformID == "CP" & df$taskID == "T3"], alternative = "two.sided", paired = TRUE, var.equal = FALSE)


tot_t_tests_table <- data.frame("Task" = c("T1", "T2", "T3"), "p" = c(t_T1_perc$p.value, t_T2_perc$p.value, t_T3_perc$p.value))
png("tot_t_tests_table_perc.png", width = 2000, height = 2000, res = 350, bg = "white")
grid.table(tot_t_tests_table)
dev.off()

# Check if the t-test is significant
print("Percentage of distracting gazes:")
print(t_T1_perc$p.value)
print(t_T2_perc$p.value)
print(t_T3_perc$p.value) # ! Only T3 is significant




# Create a data frame with your correlation values and labels
cor_data <- data.frame(
    Group = c("Overall", "Platform AA", "Platform CP", "Task T1", "Task T2", "Task T3"),
    corr_with_game_error = c(
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

questionnaire_data <- read.csv("transformed_questionnaire.csv")
trimmed_questionnaire_data <- questionnaire_data[1:25, c("Age", "Gender", "Phone_type", "license_years", "AA_fam", "CP_fam")]
trimmed_questionnaire_data$subjectID <- as.factor(unique(df$subjectID))

sus_eou_data <- read.csv("data_analysis_questionnaire.csv")
trimmed_sus_eou_data <- sus_eou_data[1:25, ]
merged_questionnaire_df <- merge(trimmed_sus_eou_data, trimmed_questionnaire_data, by = "subjectID")


# Create a df useful for the analysis of the correlation between SUS and game error

task_metrics_df_wide <- dcast(task_metrics_df, subjectID ~ platform + task, value.var = "average_game_error")
task_metrics_df_wide$subjectID <- as.factor(task_metrics_df_wide$subjectID)

merged_metrics_questionnaire_df <- merge(task_metrics_df_wide, merged_questionnaire_df, by = "subjectID")

merged_metrics_questionnaire_df$subjectID <- as.factor(merged_metrics_questionnaire_df$subjectID)
merged_metrics_questionnaire_df$Phone_type <- as.factor(merged_metrics_questionnaire_df$Phone_type)
merged_metrics_questionnaire_df$Gender <- as.factor(merged_metrics_questionnaire_df$Gender)
merged_metrics_questionnaire_df$AA_fam <- as.factor(merged_metrics_questionnaire_df$AA_fam)
merged_metrics_questionnaire_df$CP_fam <- as.factor(merged_metrics_questionnaire_df$CP_fam)

# H1 : There will be an inverse correlation between usability (SUS) and impact on visual attention. Meaning that better usability will correlate with lower impact on visual attention.

cor_game_err_sus <- data.frame(
    Group = c("Average AA", "Average CP", "AA T1", "AA T2 ", "AA T3", "CP T1", "CP T2", "CP T3"),
    corr_game_err_sus = c(
        cor(merged_metrics_questionnaire_df$SUS_AA, apply(merged_metrics_questionnaire_df[, c("AA_T1", "AA_T2", "AA_T3")], 1, mean)),
        cor(merged_metrics_questionnaire_df$SUS_CP, apply(merged_metrics_questionnaire_df[, c("CP_T1", "CP_T2", "CP_T3")], 1, mean)),
        cor(merged_metrics_questionnaire_df$SUS_AA, merged_metrics_questionnaire_df$AA_T1),
        cor(merged_metrics_questionnaire_df$SUS_AA, merged_metrics_questionnaire_df$AA_T2),
        cor(merged_metrics_questionnaire_df$SUS_AA, merged_metrics_questionnaire_df$AA_T3),
        cor(merged_metrics_questionnaire_df$SUS_CP, merged_metrics_questionnaire_df$CP_T1),
        cor(merged_metrics_questionnaire_df$SUS_CP, merged_metrics_questionnaire_df$CP_T2),
        cor(merged_metrics_questionnaire_df$SUS_CP, merged_metrics_questionnaire_df$CP_T3)
    )
)

png("cor_game_err_sus_table.png", width = 2000, height = 2000, res = 350, bg = "white")
grid.table(cor_game_err_sus)
dev.off()

# H2: Familiarity with a platform will have an impact on performance and usability

never_used_AA <- merged_metrics_questionnaire_df[merged_metrics_questionnaire_df$AA_fam == "Never used", ]$SUS_AA

occasionally_used_AA <- merged_metrics_questionnaire_df[merged_metrics_questionnaire_df$AA_fam == "Used occasionally", ]$SUS_AA

mean_usability_AA_never_used <- mean(never_used_AA)
mean_usability_AA_occasionally_used <- mean(occasionally_used_AA)

never_used_CP <- merged_metrics_questionnaire_df[merged_metrics_questionnaire_df$CP_fam == "Never used", ]$SUS_CP

occasionally_used_CP <- merged_metrics_questionnaire_df[merged_metrics_questionnaire_df$CP_fam == "Used occasionally", ]$SUS_CP

mean_usability_CP_never_used <- mean(never_used_CP)
mean_usability_CP_occasionally_used <- mean(occasionally_used_CP)


# Check if the t-test is significant better "greater" for used occasionally than never used for AA
t_AA <- t.test(occasionally_used_AA, never_used_AA, alternative = "greater", paired = FALSE, var.equal = FALSE)

# Check if the t-test is significant better "greater" for used occasionally than never used for CP
t_CP <- t.test(occasionally_used_CP, never_used_CP, alternative = "greater", paired = FALSE, var.equal = FALSE)

t_fam_sus_table <- data.frame("Platform" = c("AA", "CP"), "p" = c(t_AA$p.value, t_CP$p.value), "Never_used_mean" = c(mean_usability_AA_never_used, mean_usability_CP_never_used), "Used_occasionally_mean" = c(mean_usability_AA_occasionally_used, mean_usability_CP_occasionally_used))
png("t_fam_sus_table.png", width = 2000, height = 2000, res = 350, bg = "white")
grid.table(t_fam_sus_table)
dev.off()

# H3: Familiarity with a OS will have an impact on performance and usability

Android_AA <- merged_metrics_questionnaire_df[merged_metrics_questionnaire_df$Phone_type == "Android", ]$SUS_AA

Apple_AA <- merged_metrics_questionnaire_df[merged_metrics_questionnaire_df$Phone_type == "Apple", ]$SUS_AA

mean_usability_Android_AA <- mean(Android_AA)
mean_usability_Apple_AA <- mean(Apple_AA)

Android_CP <- merged_metrics_questionnaire_df[merged_metrics_questionnaire_df$Phone_type == "Android", ]$SUS_CP

Apple_CP <- merged_metrics_questionnaire_df[merged_metrics_questionnaire_df$Phone_type == "Apple", ]$SUS_CP

mean_usability_Android_CP <- mean(Android_CP)
mean_usability_Apple_CP <- mean(Apple_CP)


# Check if the t-test is significant better "greater" for used occasionally than never used for AA
t_pt_AA <- t.test(Android_AA, Apple_AA, paired = FALSE, var.equal = FALSE)

# Check if the t-test is significant better "greater" for used occasionally than never used for CP
t_pt_CP <- t.test(Android_CP, Apple_CP, paired = FALSE, var.equal = FALSE)

t_fam_sus_table <- data.frame("Platform" = c("AA", "CP"), "p" = c(t_pt_AA$p.value, t_pt_CP$p.value))
png("t_phone_type_sus_table.png", width = 2000, height = 2000, res = 350, bg = "white")
grid.table(t_fam_sus_table)
dev.off()
