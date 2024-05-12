library(gridExtra)
library(ggplot2)

setwd("/Users/icaroredepaolini/Personale/uni/AndroidAuto_vs_CarPlay/gaze_analysis/analysis_outputs")
df <- read.csv("transformed_questionnaire.csv")

AA_EOU <- df[, c("AA_eou_1", "AA_eou_2", "AA_eou_3")]
CP_EOU <- df[, c("CP_eou_1", "CP_eou_2", "CP_eou_3")]

AA_SUS <- df[, c("AA_sus_1", "AA_sus_2", "AA_sus_3", "AA_sus_4", "AA_sus_5", "AA_sus_6", "AA_sus_7", "AA_sus_8", "AA_sus_9", "AA_sus_10")]

CP_SUS <- df[, c("CP_sus_1", "CP_sus_2", "CP_sus_3", "CP_sus_4", "CP_sus_5", "CP_sus_6", "CP_sus_7", "CP_sus_8", "CP_sus_9", "CP_sus_10")]

calculateSus <- function(df) {
    sus_odd <- rowSums(df[, seq(1, ncol(df), 2)]) - 5
    sus_even <- 25 - rowSums(df[, seq(2, ncol(df), 2)])
    sus_total <- (sus_odd + sus_even) * 2.5
    return(list("SUS" = sus_total, "MEAN" = mean(sus_total), "SD" = round(sd(sus_total), 2)))
}

SUS_AA <- calculateSus(AA_SUS)

SUS_CP <- calculateSus(CP_SUS)

calculateEou <- function(df) {
    eou <- rowSums(df)
    return(list("EOU" = eou, "MEAN" = mean(eou), "SD" = round(sd(eou), 2)))
}

EOU_AA <- calculateEou(AA_EOU)
EOU_CP <- calculateEou(CP_EOU)

# Create a table with the results
SUS_table <- data.frame("Platform" = c("AA", "CP"), "SUS" = c(SUS_AA$MEAN, SUS_CP$MEAN), "SD" = c(SUS_AA$SD, SUS_CP$SD))

EOU_table <- data.frame("Platform" = c("AA", "CP"), "EOU" = c(EOU_AA$MEAN, EOU_CP$MEAN), "SD" = c(EOU_AA$SD, EOU_CP$SD))

png("SUS_table.png", width = 1000, height = 1000, res = 350, bg = "white")
grid.table(SUS_table)
dev.off()

png("EOU_table.png", width = 1000, height = 1000, res = 350, bg = "white")
grid.table(EOU_table)
dev.off()

plot_SUS_scores <- function(df, title) {
    df_cols <- colnames(df)
    for (i in seq_along(df_cols)) {
        if (i %% 2 == 0) {
            df[, i] <- 5 - df[, i]
        } else {
            df[, i] <- df[, i] - 1
        }
    }
    colors <- c(
        "#ffd700",
        "#ffb14e",
        "#fa8775",
        "#ea5f94",
        "#cd34b5",
        "#9d02d7",
        "#4545f5",
        "#0202c3",
        "#140282",
        "#000000"
    )
    barplot(colMeans(df), names.arg = seq_along(df_cols), col = colors, main = title, xlab = "SUS_scores", ylab = "Score", ylim = c(0, 5))
}

plot_SUS_scores(AA_SUS, "Android Auto SUS scores")
plot_SUS_scores(CP_SUS, "Apple Carplay SUS scores")

t_test_SUS <- t.test(SUS_AA$SUS, SUS_CP$SUS, paired = TRUE, alternative = "two.sided")

t_test_EOU <- t.test(EOU_AA$EOU, EOU_CP$EOU, paired = TRUE, alternative = "two.sided")

png("SUS_scores.png", width = 1500, height = 1500, res = 300, bg = "white")
t_test_table <- data.frame("Variable" = c("SUS", "EOU"), "t-value" = c(t_test_SUS$statistic, t_test_EOU$statistic), "p-value" = c(t_test_SUS$p.value, t_test_EOU$p.value), "mean_AA" = c(SUS_AA$MEAN, EOU_AA$MEAN), "mean_CP" = c(SUS_CP$MEAN, EOU_CP$MEAN))
grid.table(t_test_table)
dev.off()

# Create a data frame with SUS and EOU values
data_analysis_questionnaire <- data.frame(
    SUS_AA = SUS_AA$SUS,
    SUS_CP = SUS_CP$SUS,
    EOU_AA = EOU_AA$EOU,
    EOU_CP = EOU_CP$EOU
)
num_rows <- nrow(data_analysis_questionnaire)
subject_id <- seq(1, num_rows)
data_analysis_questionnaire <- cbind(subject_id, data_analysis_questionnaire)
colnames(data_analysis_questionnaire) <- c("subjectID", "SUS_AA", "SUS_CP", "EOU_AA", "EOU_CP")
# save
write.csv(data_analysis_questionnaire, "data_analysis_questionnaire.csv", row.names = FALSE)
