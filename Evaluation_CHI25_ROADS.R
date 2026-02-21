library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

library(easystats)
library(see)
library(ggplot2)
library(ARTool)
library(tidyr)
library(dplyr)
library(reporttools)
library(PMCMRplus)
library(FSA)
library(ggstatsplot)
library(stats)

library(colleyRstats)
colleyRstats::colleyRstats_setup()

main_df <- readxl::read_xlsx(path = "results-roads-chi25.xlsx", sheet = "Results")
main_df <- as.data.frame(main_df)
names(main_df)

# replace all negative values with the actual values
main_df <- colleyRstats::replace_values(main_df, c("neg2", "neg1"), c("-2", "-1"))

# TLX
main_df$tlxScore <- (main_df$TLX1 + main_df$TLX2 + main_df$TLX3 + main_df$TLX4 + main_df$TLX5 + main_df$TLX6) / 6.0

# SUS
main_df$SUSScore <- ((main_df$SUS1 - 1) + (main_df$SUS3 - 1) + (main_df$SUS5 - 1) + (main_df$SUS7 - 1) + (main_df$SUS9 - 1) + (5 - main_df$SUS2) + (5 - main_df$SUS4) + (5 - main_df$SUS6) + (5 - main_df$SUS8) + (5 - main_df$SUS10)) * 2.5


main_df$aoa1 <- as.numeric(main_df$aoa1)
main_df$aoa2 <- as.numeric(main_df$aoa2)
main_df$aoa3 <- as.numeric(main_df$aoa3)
main_df$aoa4 <- as.numeric(main_df$aoa4)
main_df$aoa5 <- as.numeric(main_df$aoa5)
main_df$aoa6 <- as.numeric(main_df$aoa6)
main_df$aoa7 <- as.numeric(main_df$aoa7)
main_df$aoa8 <- as.numeric(main_df$aoa8)
main_df$aoa9 <- as.numeric(main_df$aoa9)


# Assessment of Acceptance - van der Laan
main_df$AOAUsefulness <- ((3 - main_df$aoa1) + (-3 + main_df$aoa3) + (3 - main_df$aoa5) + (3 - main_df$aoa7) + (3 - main_df$aoa9)) / 5.0
main_df$AOASatisfying <- ((3 - main_df$aoa2) + (3 - main_df$aoa4) + (-3 + main_df$aoa6) + (-3 + main_df$aoa8)) / 4.0



main_df$interaction <- as.factor(ifelse(main_df$Condition %in% c("1", "4", "7", "10"), "Trajectory", ifelse(main_df$Condition %in% c("2", "5", "8", "11"), "Path planning", "Waypoint")))
main_df$interaction <- relevel(main_df$interaction, "Waypoint")

main_df$numberOfRequests <- factor(ifelse(main_df$Condition %in% c("1", "2", "3"), "One", ifelse(main_df$Condition %in% c("4", "5", "6"), "Two", ifelse(main_df$Condition %in% c("7", "8", "9"), "Three", "Four"))), levels = c("One", "Two", "Three", "Four"))
main_df$numberOfRequests <- relevel(main_df$numberOfRequests, "One")



main_df$UserID <- as.factor(main_df$UserID)
main_df$ConditionID <- as.factor(main_df$ConditionID)


levels(main_df$interaction)
levels(main_df$numberOfRequests)


vars <- main_df[, c("tlxScore", "SUSScore", "AOAUsefulness", "AOASatisfying")]
group <- main_df[, c("interaction")]


# see https://stackoverflow.com/questions/25389683/obtaining-separate-summary-statistics-by-categorical-variable-with-stargazer-pac
## display default statistics, only use a subset of observations, grouped analysis
tableContinuous(vars = vars, group = group, prec = 2, cap = "Table of scores grouped by interaction concept.", lab = "tab:_descr_stat_by_concept", stats = c("min", "q1", "median", "mean", "q3", "max", "s", "iqr"))



group <- main_df[, c("numberOfRequests")]

## display default statistics, only use a subset of observations, grouped analysis
tableContinuous(vars = vars, group = group, prec = 2, cap = "Table of scores grouped by number of requests.", lab = "tab:_descr_stat", stats = c("min", "q1", "median", "mean", "q3", "max", "s", "iqr"))

# There was one EW17 instead of EW13 -> until now not adjusted in LimeSurvey, only in .xslx
# there was jm29 and JM29 -> all JM29 now but again only in .xslx
main_df |>
  group_by(UserID, interaction) |>
  count() |>
  print(n = 300)




#### TLX ####
checkAssumptionsForAnova(data = main_df, y = "tlxScore", factors = c("interaction", "numberOfRequests"))


modelArt <- art(tlxScore ~ interaction * numberOfRequests + Error(UserID / (interaction * numberOfRequests)), data = main_df) |> anova()
modelArt
reportART(modelArt, dv = "TLX score")


PMCMRplus::kwAllPairsDunnTest(x = main_df$numberOfRequests, g=main_df$tlxScore)

## Dunn's all-pairs comparison test
ans <- kwAllPairsDunnTest(tlxScore ~ numberOfRequests, data = main_df,
                          p.adjust.method = "holm")
summary(ans)

ans <- durbinAllPairsTest(y = main_df$tlxScore, groups = main_df$numberOfRequests, blocks = main_df$numberOfRequests,
                          p.adjust.method = "holm")
summary(ans)


d <- dunnTest(tlxScore ~ numberOfRequests, data = main_df, method = "holm")
d
reportDunnTest(data = main_df, d = d, iv = "numberOfRequests", dv = "tlxScore")

d <- dunnTest(tlxScore ~ interaction, data = main_df, method = "holm")
d
reportDunnTest(data = main_df, d = d, iv = "interaction", dv = "tlxScore")

reportMeanAndSD(data = main_df, iv = "numberOfRequests", dv = "tlxScore")

library(ggsignif)

ggwithinstats(
  data = main_df, x = interaction, y = tlxScore, type = "np", centrality.type = "p", ylab = "TLX Score", xlab = "",
  centrality.path = FALSE, centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue", shape = 10),
  p.adjust.method = "holm", boxplot.args = list(width = 0.1, alpha = 0.5, na.rm = TRUE, color = "black"), pairwise.display = "none",
  ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), results.subtitle = FALSE
) 
ggsave("plots/interaction_nasa_tlx.pdf", width = 12, height = 9, device = cairo_pdf)



ggwithinstats(
  data = main_df, x = numberOfRequests, y = tlxScore, type = "np", centrality.type = "p", ylab = "TLX Score", xlab = "",
  centrality.path = FALSE, centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue", shape = 10),
  p.adjust.method = "holm", boxplot.args = list(width = 0.1, alpha = 0.5, na.rm = TRUE, color = "black"), pairwise.display = "none",
  ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), ggsignif.args = list(textsize = -1, tip_length = 0.01), results.subtitle = FALSE
)
ggsave("plots/numberOfRequests_nasa_tlx.pdf", width = 12, height = 9, device = cairo_pdf)



# Boxplot for tlxScore grouped by 'interaction'
ggplot(main_df, aes(x = interaction, y = tlxScore)) +
  geom_boxplot() +
  labs(title = "Boxplot of TLX Score by Interaction",
       x = "Interaction", y = "TLX Score") +
  theme_minimal()

# Boxplot for tlxScore grouped by 'numberOfRequests'
ggplot(main_df, aes(x = factor(numberOfRequests), y = tlxScore)) +
  geom_boxplot() +
  labs(title = "Boxplot of TLX Score by Number of Requests",
       x = "Number of Requests", y = "TLX Score") +
  theme_minimal()





#### SUSScore ####
checkAssumptionsForAnova(data = main_df, y = "SUSScore", factors = c("interaction", "numberOfRequests"))


modelArt <- art(SUSScore ~ interaction * numberOfRequests + Error(UserID / (interaction * numberOfRequests)), data = main_df) |> anova()
modelArt
reportART(modelArt, dv = "SUSScore")

d <- dunnTest(SUSScore ~ numberOfRequests, data = main_df, method = "holm")
d
reportDunnTest(data = main_df, d = d, iv = "numberOfRequests", dv = "SUSScore")

d <- dunnTest(SUSScore ~ interaction, data = main_df, method = "holm")
d
reportDunnTest(data = main_df, d = d, iv = "interaction", dv = "SUSScore")


ggwithinstats(
  data = main_df, x = interaction, y = SUSScore, type = "np", centrality.type = "p", ylab = "SUS Score", xlab = "",
  centrality.path = FALSE, centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue", shape = 10),
  p.adjust.method = "holm", boxplot.args = list(width = 0.1, alpha = 0.5, na.rm = TRUE, color = "black"), pairwise.display = "none",
  ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), results.subtitle = FALSE
) 
ggsave("plots/interaction_sus.pdf", width = 12, height = 9, device = cairo_pdf)



ggwithinstats(
  data = main_df, x = numberOfRequests, y = SUSScore, type = "np", centrality.type = "p", ylab = "SUS Score", xlab = "",
  centrality.path = FALSE, centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue", shape = 10),
  p.adjust.method = "holm", boxplot.args = list(width = 0.1, alpha = 0.5, na.rm = TRUE, color = "black"), pairwise.display = "none",
  ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), ggsignif.args = list(textsize = -1, tip_length = 0.01), results.subtitle = FALSE
)
ggsave("plots/numberOfRequests_sus.pdf", width = 12, height = 9, device = cairo_pdf)




#### AOAUsefulness ####
checkAssumptionsForAnova(data = main_df, y = "AOAUsefulness", factors = c("interaction", "numberOfRequests"))



modelArt <- art(AOAUsefulness ~ interaction * numberOfRequests + Error(UserID / (interaction * numberOfRequests)), data = main_df) |> anova()
modelArt
reportART(modelArt, dv = "AOAUsefulness")


dunnTest(AOAUsefulness ~ numberOfRequests, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "numberOfRequests", dv = "AOAUsefulness")
dunnTest(AOAUsefulness ~ interaction, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "interaction", dv = "AOAUsefulness")



ggwithinstats(
  data = main_df, x = interaction, y = AOAUsefulness, type = "np", centrality.type = "p", ylab = "Usefulness", xlab = "",
  centrality.path = FALSE, centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue", shape = 10),
  p.adjust.method = "holm", boxplot.args = list(width = 0.1, alpha = 0.5, na.rm = TRUE, color = "black"), pairwise.display = "none",
  ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), results.subtitle = FALSE
) 
ggsave("plots/interaction_AOAUsefulness.pdf", width = 12, height = 9, device = cairo_pdf)



ggwithinstats(
  data = main_df, x = numberOfRequests, y = AOAUsefulness, type = "np", centrality.type = "p", ylab = "Usefulness", xlab = "",
  centrality.path = FALSE, centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue", shape = 10),
  p.adjust.method = "holm", boxplot.args = list(width = 0.1, alpha = 0.5, na.rm = TRUE, color = "black"), pairwise.display = "none",
  ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), ggsignif.args = list(textsize = -1, tip_length = 0.01), results.subtitle = FALSE
)
ggsave("plots/numberOfRequests_AOAUsefulness.pdf", width = 12, height = 9, device = cairo_pdf)









#### AOASatisfying ####
checkAssumptionsForAnova(data = main_df, y = "AOASatisfying", factors = c("interaction", "numberOfRequests"))


modelArt <- art(AOASatisfying ~ interaction * numberOfRequests + Error(UserID / (interaction * numberOfRequests)), data = main_df) |> anova()
modelArt
reportART(modelArt, dv = "AOASatisfying")


dunnTest(AOASatisfying ~ numberOfRequests, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "numberOfRequests", dv = "AOASatisfying")
dunnTest(AOASatisfying ~ interaction, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "interaction", dv = "AOASatisfying")

main_df %>% ggplot() +
  aes(x = interaction, y = AOASatisfying, fill = numberOfRequests, colour = numberOfRequests, group = numberOfRequests) +
  scale_color_see() +
  ylab("AOASatisfying") +
  labs(subtitle = "range: -2 to +2") +
  theme(legend.position.inside = c(0.85, 0.25)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 2) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1)) # 95 % mean_cl_boot is 95% confidence intervals
# ggsave("plots/AOASatisfying_interaction.pdf", width = 12, height = 9, device = cairo_pdf)


ggwithinstats(
  data = main_df, x = interaction, y = AOASatisfying, type = "np", centrality.type = "p", ylab = "Satisfying", xlab = "",
  centrality.path = FALSE, centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue", shape = 10),
  p.adjust.method = "holm", boxplot.args = list(width = 0.1, alpha = 0.5, na.rm = TRUE, color = "black"), pairwise.display = "none",
  ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), results.subtitle = FALSE
) 
ggsave("plots/interaction_AOASatisfying.pdf", width = 12, height = 9, device = cairo_pdf)



ggwithinstats(
  data = main_df, x = numberOfRequests, y = AOASatisfying, type = "np", centrality.type = "p", ylab = "Satisfying", xlab = "",
  centrality.path = FALSE, centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue", shape = 10),
  p.adjust.method = "holm", boxplot.args = list(width = 0.1, alpha = 0.5, na.rm = TRUE, color = "black"), pairwise.display = "none",
  ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), ggsignif.args = list(textsize = -1, tip_length = 0.01), results.subtitle = FALSE
)
ggsave("plots/numberOfRequests_AOASatisfying.pdf", width = 12, height = 9, device = cairo_pdf)












##### DEMO and FINAL ####
final_df <- readxl::read_xlsx(path = "results-roads-final-chi25.xlsx")
final_df <- as.data.frame(final_df)
names(final_df)





ranking <- NULL
ranking <- final_df[, 7:9] # only the rankings
ranking

# Landmark
ranking$path <- which(apply(ranking[, c(1:3)], 1, function(x) grepl("path", x)), arr.ind = TRUE)[, 1]

#
ranking$way <- which(apply(ranking[, c(1:3)], 1, function(x) grepl("way", x)), arr.ind = TRUE)[, 1]

#
ranking$traj <- which(apply(ranking[, c(1:3)], 1, function(x) grepl("traj", x)), arr.ind = TRUE)[, 1]


ranking_number <- ranking[, c(4:6)]
names(ranking_number)

data_long <- gather(ranking_number, key = ConditionID, value = rank, factor_key = TRUE)
data_long

rank_x_lab <- c("path" = "Path", "way" = "Waypoints", "traj" = "Trajectory")


ggwithinstatsWithPriorNormalityCheck(data = data_long, x = "ConditionID", y = "rank", ylab = "Rank (lower is better)", xlabels = rank_x_lab)
ggsave("plots/rank_devices.pdf", width = 12 + 5, height = 9, device = cairo_pdf)



p <- ggwithinstats(data = data_long, x = ConditionID, y = rank, type = "np")

reportggstatsplot(p, iv = "ConditionID", dv = "ranking")





# Education: A3 High school A4 College
# job A2: Student (college) A3 employees
report::report_participants(final_df, )


# mean hours gaming per week
mean(final_df$gaming)
sd(final_df$gaming)


mean(final_df$interest)
sd(final_df$interest)

mean(final_df$ease)
sd(final_df$ease)

mean(final_df$reality)
sd(final_df$reality)










#### Multivariate Multiple Regression  ####
# Assuming main_df is your dataframe and the necessary packages are installed
library(lme4)


# model_tlxScore <- lm(tlxScore ~ interaction * numberOfRequests, data = main_df)
# model_SUSScore <- lm(SUSScore ~ interaction * numberOfRequests, data = main_df)
# model_AOAUsefulness <- lm(AOAUsefulness ~ interaction * numberOfRequests, data = main_df)
# model_AOASatisfying <- lm(AOASatisfying ~ interaction * numberOfRequests, data = main_df)


mlm_model <- lm(cbind(tlxScore, SUSScore, AOAUsefulness, AOASatisfying) ~ interaction * numberOfRequests, data = main_df)
combined_model <- stats::manova(mlm_model)
summary(combined_model)

# report::report(combined_model)

