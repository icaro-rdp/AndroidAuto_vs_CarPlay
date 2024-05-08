# Import data
df <- read.csv("gaze_analysis_data.csv")

tot <- df$time_on_task

upper_bound <- boxplot.stats(tot)$stats[5]
lower_bound <- boxplot.stats(tot)$stats[1]

mean_tot_AA_t1 <- mean(df$time_on_task[df$platformID == "AA" & df$taskID == "T1"])
mean_tot_CP_t1 <- mean(df$time_on_task[df$platformID == "CP" & df$taskID == "T1"])

mean_tot_AA_t2 <- mean(df$time_on_task[df$platformID == "AA" & df$taskID == "T2"])
mean_tot_CP_t2 <- mean(df$time_on_task[df$platformID == "CP" & df$taskID == "T2"])

mean_tot_AA_t3 <- mean(df$time_on_task[df$platformID == "AA" & df$taskID == "T3"])
mean_tot_CP_t3 <- mean(df$time_on_task[df$platformID == "CP" & df$taskID == "T3"])

print(paste("Mean time on task for AA in T1: ", mean_tot_AA_t1))
print(paste("Mean time on task for CP in T1: ", mean_tot_CP_t1))

print(paste("Mean time on task for AA in T2: ", mean_tot_AA_t2))
print(paste("Mean time on task for CP in T2: ", mean_tot_CP_t2))

print(paste("Mean time on task for AA in T3: ", mean_tot_AA_t3))
print(paste("Mean time on task for CP in T3: ", mean_tot_CP_t3))

# Perform t-test

# Check if CP is faster than AA in T3
t.test(df$time_on_task[df$platformID == "AA" & df$taskID == "T3"], df$time_on_task[df$platformID == "CP" & df$taskID == "T3"], alternative = "less", paired = TRUE, var.equal = FALSE)
