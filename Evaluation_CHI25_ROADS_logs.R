library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

library(devtools)
source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")


library(DirichletReg)
library(networkD3) # Sankey
library(dplyr)
library(plotly)
library(GGally)
library(igraph)


filesTimeStamps <- list.files(path = "./ClearedLogs/", recursive = TRUE, pattern = "TimestampLog.*\\.csv", full.names = TRUE)
# exclude those with a "-"
filesTimeStamps <- filesTimeStamps[!grepl("/.+-", filesTimeStamps)]


filesLog <- list.files(path = "./ClearedLogs/", recursive = TRUE, pattern = "log.*\\.csv", full.names = TRUE)
# exclude those with a "-"
filesLog <- filesLog[!grepl("/.+-", filesLog)]

filesOverviewLog <- list.files(path = "./ClearedLogs/", recursive = TRUE, pattern = "OverviewLog.*\\.csv", full.names = TRUE)
# exclude those with a "-"
filesOverviewLog <- filesOverviewLog[!grepl("/.+-", filesOverviewLog)]
filesOverviewLog

# Exclude those with a "-" and those containing "Tag1"
filesOverviewLog1 <- filesOverviewLog[!grepl("/.+-|Tag1", filesOverviewLog)]
filesOverviewLog1





#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################





# Dauer der kompletten Interaktion (muss immer unter 120s sein): TimeStamps
# schauen wie viele der requests finished wurden: TimeStamps

# Read all CSV files with separator ";"
data_list <- lapply(filesTimeStamps, function(file) {
  read.csv(file, sep = ";")
})

# Combine all data frames into a single data frame
all_data_timestamps <- do.call(rbind, data_list)

# Replace the comma with a period in the elapsedTime column and convert it to numeric
all_data_timestamps$elapsedTime <- as.numeric(gsub(",", ".", all_data_timestamps$elapsedTime))



all_data_timestamps$interaction <- as.factor(ifelse(all_data_timestamps$scenarioID %in% c("1", "4", "7", "10"), "Trajectory", ifelse(all_data_timestamps$scenarioID %in% c("2", "5", "8", "11"), "Path planning", "Waypoint")))
all_data_timestamps$numberOfRequests <- factor(ifelse(all_data_timestamps$scenarioID %in% c("1", "2", "3"), "One", ifelse(all_data_timestamps$scenarioID %in% c("4", "5", "6"), "Two", ifelse(all_data_timestamps$scenarioID %in% c("7", "8", "9"), "Three", "Four"))), levels = c("One", "Two", "Three", "Four"))

all_data_timestamps$numberOfRequestsNumber <- as.numeric(ifelse(all_data_timestamps$scenarioID %in% c("1", "2", "3"), "1", ifelse(all_data_timestamps$scenarioID %in% c("4", "5", "6"), "2", ifelse(all_data_timestamps$scenarioID %in% c("7", "8", "9"), "3", "4"))))




# Function to count occurrences and get last elapsedTime
count_and_last_time <- function(status_col, time_col, numberOfRequestsNumber_col) {
  count <- sum(status_col == "RequestFinished")
  last_time <- if (count > 0) {
    max(time_col[status_col == "RequestFinished"])
  } else {
    NA
  }
  return(data.frame(count, last_time, numberOfRequestsNumber = unique(numberOfRequestsNumber_col)))
}


# Group and summarize
result_finished <- all_data_timestamps %>%
  group_by(userID, interaction, numberOfRequests) %>%
  do(count_and_last_time(.$timeStampEvent, .$elapsedTime, .$numberOfRequestsNumber))

result_finished$missingRequests <- result_finished$numberOfRequestsNumber - result_finished$count





result_finished$userID <- as.factor(result_finished$userID)

# Remove rows where userID is "DB06", "FF23", or "MJ28"
# --> TODO should we do this#
# TODO why do we do this? --> first three in day 1?
# filtered_df <- result_finished %>%
#  filter(!userID %in% c("DB06", "FF23", "MJ28"))


checkAssumptionsForAnovaTwoFactors(data = result_finished, y = "missingRequests", factor_1 = "interaction", factor_2 = "numberOfRequests")

modelArt <- art(missingRequests ~ interaction * numberOfRequests + Error(userID / (interaction * numberOfRequests)), data = result_finished) |> anova()
modelArt
reportART(modelArt, dv = "missingRequests")


result_finished %>% ggplot() +
  aes(x = interaction, y = missingRequests, fill = numberOfRequests, colour = numberOfRequests, group = numberOfRequests) +
  # scale_colour_manual(values = wes_palette("Cavalcanti1", n = 4)) +
  scale_color_see() +
  ylab("Not finished requests") +
  theme(legend.position.inside = c(0.85, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) # 95 % mean_cl_boot is 95% confidence intervals
ggsave("plots/missing_requests_interaction.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



checkAssumptionsForAnova(data = result_finished, y = "last_time", factors = c("interaction", "numberOfRequests"))


# some values are missing --> avoid
# filtered_df_no_NA <- na.omit(result_finished)
# filtered_df_no_NA |> group_by(userID) |> count() |> print(n=30)
#
#
# modelArt <- art(last_time ~ interaction * numberOfRequests + Error(userID / (interaction * numberOfRequests)), data = result_finished) |> anova()
# modelArt
# reportART(modelArt, dv = "last_time")
#
#
# filtered_df %>% ggplot() +
#
#   aes(x = interaction, y = last_time, fill = numberOfRequests, colour = numberOfRequests, group = numberOfRequests) +
#   scale_colour_manual(values = wes_palette("Cavalcanti1", n = 5)) +
#   ylab("last_time") +
#   theme( legend.position.inside = c(0.85, 0.85)) +
#   xlab("") +
#   stat_summary(fun = mean, geom = "point", size = 4.0) +
#   stat_summary(fun = mean, geom = "line", linewidth = 2) +
#   stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1)) # 95 % mean_cl_boot is 95% confidence intervals
# #ggsave("plots/last_time_interaction.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################


# Eye Gaze mit AOIs request-panel (_requestList), mid panel (_mainView), info-panel und secondary (_secondaryView) + OffScreen
# in UnitEyeLog

filesEyeGaze <- list.files(path = "./ClearedLogs/", recursive = TRUE, pattern = "UnitEyeLog.*\\.csv", full.names = TRUE)
# exclude those with a "-"
filesEyeGaze <- filesEyeGaze[!grepl("/.+-", filesEyeGaze)]


# Read the files and add UserID and ScenarioID
all_data <- lapply(filesEyeGaze, function(file) {
  UserID <- sub(".*/(.*)/.*", "\\1", dirname(file))
  ScenarioID <- basename(dirname(file))
  data <- read.csv(file, sep = ";")
  data <- data %>%
    mutate(UserID = UserID, ScenarioID = ScenarioID)
  return(data)
})

# Combine all the data frames into a single data frame
all_data <- bind_rows(all_data)


df_null <- all_data %>%
  group_by(UserID, ScenarioID) %>%
  summarise(sumFixationsNull = sum(AOI.List == "OffScreen"))
df_request <- all_data %>%
  group_by(UserID, ScenarioID) %>%
  summarise(sumFixationsRequest = sum(AOI.List == "_requestList"))
df_main <- all_data %>%
  group_by(UserID, ScenarioID) %>%
  summarise(sumFixationsMain = sum(AOI.List == "_mainView"))
df_secondary <- all_data %>%
  group_by(UserID, ScenarioID) %>%
  summarise(sumFixationsSecondary = sum(AOI.List == "_secondaryView"))


test <- merge(df_null, df_request, by = c("UserID", "ScenarioID")) %>%
  merge(., df_main, by = c("UserID", "ScenarioID")) %>%
  merge(., df_secondary, by = c("UserID", "ScenarioID"))

test$totalFixations <- test$sumFixationsNull + test$sumFixationsRequest + test$sumFixationsMain + test$sumFixationsSecondary

# define percentages
test$sumFixationsNull_Percentage <- test$sumFixationsNull / test$totalFixations
test$sumFixationsRequest_Percentage <- test$sumFixationsRequest / test$totalFixations
test$sumFixationsMain_Percentage <- test$sumFixationsMain / test$totalFixations
test$sumFixationsSecondary_Percentage <- test$sumFixationsSecondary / test$totalFixations


# only select relevant columns
library(dplyr)
test2 <- dplyr::select(test, !(sumFixationsNull:totalFixations))

data_long <- gather(test2, aoi, measurement, sumFixationsNull_Percentage:sumFixationsSecondary_Percentage, factor_key = TRUE)
data_long

# remove unnecessary strings
data_long$aoi <- gsub("sumFixations", "", data_long$aoi)
data_long$aoi <- gsub("_Percentage", "", data_long$aoi)

data_long$aoi <- as.factor(data_long$aoi)


data_long$interaction <- as.factor(ifelse(data_long$ScenarioID %in% c("1", "4", "7", "10"), "Trajectory", ifelse(data_long$ScenarioID %in% c("2", "5", "8", "11"), "Path planning", "Waypoint")))
data_long$numberOfRequests <- factor(ifelse(data_long$ScenarioID %in% c("1", "2", "3"), "One", ifelse(data_long$ScenarioID %in% c("4", "5", "6"), "Two", ifelse(data_long$ScenarioID %in% c("7", "8", "9"), "Three", "Four"))), levels = c("One", "Two", "Three", "Four"))


# Combining main and secondary AOI if numberOfRequests is "One"
data_long <- data_long %>%
  group_by(UserID, ScenarioID, aoi) %>%
  mutate(measurement = ifelse(numberOfRequests == "One" & aoi == "Main", measurement + sum(measurement[aoi == "Secondary"]), measurement)) %>%
  ungroup()




# Attention: when only one request is there, the far right of that panel is recorded as secondary, therefore, combine the two

# Create a temporary data frame for the combined main and secondary measurements
combined_measurements <- data_long %>%
  filter(numberOfRequests == "One", aoi %in% c("Main", "Secondary")) %>%
  group_by(UserID, ScenarioID) %>%
  summarise(combined_measurement = sum(measurement)) %>%
  ungroup()

# Join the temporary data frame back to the original data
data_long <- data_long %>%
  left_join(combined_measurements, by = c("UserID", "ScenarioID")) %>%
  mutate(measurement = ifelse(numberOfRequests == "One" & aoi == "Main", combined_measurement, measurement)) %>%
  select(-combined_measurement) # Remove the temporary column


# Setting secondary to 0 if numberOfRequests is "One"
data_long$measurement[data_long$numberOfRequests == "One" & data_long$aoi == "Secondary"] <- 0







data_long %>% ggplot() +
  aes(x = ScenarioID, y = measurement, fill = aoi, colour = aoi, group = aoi) +
  # scale_colour_manual(values=wes_palette("Cavalcanti1", n=11, type = "continuous")) +
  scale_color_see() +
  ylab("Percentage Fixation") +
  theme(legend.position.inside = c(0.78, 0.58)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, position = position_dodge(width = .1)) # 95 % mean_cl_boot is 95% confidence intervals




data_long_1 <- subset(data_long, aoi != "Null")

data_long_1 %>% ggplot() +
  aes(x = ScenarioID, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  # scale_colour_manual(values=wes_palette("Cavalcanti1", n=11, type = "continuous")) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null") +
  theme(legend.position.inside = c(0.9, 0.91)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi)) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  # stat_summary(fun = mean, geom = "line", linewidth = 1, aes(linetype= aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1)
# ggsave("plots/Fixation_without_null.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


data_long_1 %>% ggplot() +
  aes(x = interaction, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  # scale_colour_manual(values=wes_palette("Cavalcanti1", n=11, type = "continuous")) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null") +
  theme(legend.position.inside = c(0.9, 0.68)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi)) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  # stat_summary(fun = mean, geom = "line", linewidth = 1, aes(linetype= aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1)
ggsave("plots/Fixation_without_null_no_zero_interaction.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



data_long_1 %>% ggplot() +
  aes(x = numberOfRequests, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  # scale_colour_manual(values=wes_palette("Cavalcanti1", n=11, type = "continuous")) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null") +
  theme(legend.position.inside = c(0.9, 0.88)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi)) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  # stat_summary(fun = mean, geom = "line", linewidth = 1, aes(linetype= aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1)
ggsave("plots/Fixation_without_null_no_zero_numberOfRequests.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




# remove when was not visible
data_long_1 <- subset(data_long_1, subset = measurement != 0)

data_long_1 %>% ggplot() +
  theme_bw(base_size = myfontsize - 12) +
  aes(x = ScenarioID, y = measurement * 100, colour = aoi, group = aoi) +
  # scale_colour_manual(values=wes_palette("Cavalcanti1", n=11, type = "continuous")) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null") +
  theme(legend.position.inside = c(0.9, 0.68), axis.text = element_text(size = 10)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi)) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  # stat_summary(fun = mean, geom = "line", size = 1, aes(linetype= aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1)
# ggsave("plots/Fixation_without_null_no_zero.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)





data_wide <- data_long %>%
  pivot_wider(names_from = aoi, values_from = measurement)


data_wide$interaction <- as.factor(ifelse(data_wide$ScenarioID %in% c("1", "4", "7", "10"), "Trajectory", ifelse(data_wide$ScenarioID %in% c("2", "5", "8", "11"), "Path planning", "Waypoint")))
data_wide$interaction <- relevel(data_wide$interaction, "Waypoint")

data_wide$numberOfRequests <- factor(ifelse(data_wide$ScenarioID %in% c("1", "2", "3"), "One", ifelse(data_wide$ScenarioID %in% c("4", "5", "6"), "Two", ifelse(data_wide$ScenarioID %in% c("7", "8", "9"), "Three", "Four"))), levels = c("One", "Two", "Three", "Four"))
data_wide$numberOfRequests <- relevel(data_wide$numberOfRequests, "One")






### Dirichlet Regression ####

data_wide$Y <- DR_data(data_wide[, 5:8])

data_wide$y <- DR_data(data_wide[, 5:8])

DirichletReg::DirichReg(formula = Y ~ interaction * numberOfRequests, data = data_wide)

# fit a quadratic Dirichlet regression models ("common")
res1 <- DirichletReg::DirichReg(formula = Y ~ interaction * numberOfRequests, data = data_wide)
summary(res1)
DirichletReg::print.DirichletRegModel(res1)


# library(brms)
# library(dplyr)
# bind <- function(...) cbind(...)
# brms::dirichlet()
#
# data_wide$y <- with(data_wide, cbind(Null, Request, Main, Secondary))
#
# make_stancode(bind(Null, Request, Main, Secondary) ~ interaction * numberOfRequests, data_wide, dirichlet())
# > make_standata(bind(Null, Request, Main, Secondary) ~ interaction * numberOfRequests, data_wide, dirichlet())
# Error: Family 'dirichlet' requires response greater than 0.
# make_standata(bind(Null, Request, Main, Secondary) ~ interaction * numberOfRequests, data_wide, dirichlet())
#
#
# fit <- brm(bind(Null, Request, Main, Secondary) ~ interaction * numberOfRequests, data_wide, dirichlet())
# summary(fit)



# fit a Dirichlet regression with quadratic predictor for the mean and
# a linear predictor for precision ("alternative")
# res2 <- DirichletReg::DirichReg(formula = Y ~ interaction * numberOfRequests, data = data_wide,  model="alternative")
# # test both models
# anova(res1, res2)
# res1
# summary(res2)




#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################


# car deviation: absolute currentLaneDeviation in m in log_*


# Read all CSV files with separator ";" and ensure that all have the same columns
data_list <- lapply(filesLog, function(file) {
  data <- read.csv(file, sep = ";")


  if (!"cameraPannedDistanceSum" %in% colnames(data)) {
    data$cameraPannedDistanceSum <- NA
  }
  if (!"cameraTravelledDistanceSum" %in% colnames(data)) {
    data$cameraTravelledDistanceSum <- NA
  }
  if (!"zoomSize" %in% colnames(data)) {
    data$zoomSize <- NA
  }
  if (!"followVehicleFocusTimeSum" %in% colnames(data)) {
    data$followVehicleFocusTimeSum <- NA
  }
  if (!"followPathEndFocusTimeSum" %in% colnames(data)) {
    data$followPathEndFocusTimeSum <- NA
  }


  return(data)
})

# Combine all data frames into a single data frame
all_data_deviation <- do.call(rbind, data_list)

# names(data_list[[97]])
# names(data_list[[96]])
#
# setdiff(names(data_list[[97]]), names(data_list[[96]]))


# Replace the comma with a period in the elapsedTime column and convert it to numeric
all_data_deviation$currentLaneDeviation <- as.numeric(gsub(",", ".", all_data_deviation$currentLaneDeviation))

all_data_deviation$interaction <- as.factor(ifelse(all_data_deviation$ScenarioID %in% c("1", "4", "7", "10"), "Trajectory", ifelse(all_data_deviation$ScenarioID %in% c("2", "5", "8", "11"), "Path planning", "Waypoint")))
all_data_deviation$numberOfRequests <- factor(ifelse(all_data_deviation$ScenarioID %in% c("1", "2", "3"), "One", ifelse(all_data_deviation$ScenarioID %in% c("4", "5", "6"), "Two", ifelse(all_data_deviation$ScenarioID %in% c("7", "8", "9"), "Three", "Four"))), levels = c("One", "Two", "Three", "Four"))



result_deviation <- all_data_deviation %>%
  group_by(UserID, interaction, numberOfRequests) %>%
  summarise(sumCurrentLaneDeviation = sum(abs(currentLaneDeviation), na.rm = TRUE)) %>%
  ungroup()

# result_deviation$sumCurrentLaneDeviation

result_deviation$UserID <- as.factor(result_deviation$UserID)

checkAssumptionsForAnovaTwoFactors(data = result_deviation, y = "sumCurrentLaneDeviation", factor_1 = "interaction", factor_2 = "numberOfRequests")

modelArt <- art(sumCurrentLaneDeviation ~ interaction * numberOfRequests + Error(UserID / (interaction * numberOfRequests)), data = result_deviation) |> anova()
modelArt
reportART(modelArt, dv = "absolute sum of lane deviation")


dunnTest(sumCurrentLaneDeviation ~ numberOfRequests, data = result_deviation, method = "holm") |> reportDunnTest(data = result_deviation, iv = "numberOfRequests", dv = "sumCurrentLaneDeviation")
dunnTest(sumCurrentLaneDeviation ~ interaction, data = result_deviation, method = "holm") |> reportDunnTest(data = result_deviation, iv = "interaction", dv = "sumCurrentLaneDeviation")

# dunnTest(sumCurrentLaneDeviation ~ numberOfRequests, data = result_deviation, method = "holm") |> reportDunnTestTable(data = result_deviation, iv = "numberOfRequests", dv = "sumCurrentLaneDeviation", orderText = TRUE)
# dunnTest(sumCurrentLaneDeviation ~ interaction, data = result_deviation, method = "holm") |> reportDunnTestTable(data = result_deviation, iv = "interaction", dv = "sumCurrentLaneDeviation")



result_deviation %>% ggplot() +
  aes(x = interaction, y = sumCurrentLaneDeviation, fill = numberOfRequests, colour = numberOfRequests, group = numberOfRequests) +
  # scale_colour_manual(values = wes_palette("Cavalcanti1", n = 5)) +
  scale_color_see() +
  ylab("sumCurrentLaneDeviation") +
  theme(legend.position.inside = c(0.85, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) # 95 % mean_cl_boot is 95% confidence intervals
# ggsave("plots/sumCurrentLaneDeviation_interaction.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)





#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################


# failure case: currentlyNeglectedTime (ist pro incidence, man muss also immer die hoechste Zahl aufaddieren) --> durchschnitt whs, in log_*

# Read all CSV files with separator ";" and ensure that all have the same columns
data_list <- lapply(filesLog, function(file) {
  data <- read.csv(file, sep = ";")


  if (!"cameraPannedDistanceSum" %in% colnames(data)) {
    data$cameraPannedDistanceSum <- NA
  }
  if (!"cameraTravelledDistanceSum" %in% colnames(data)) {
    data$cameraTravelledDistanceSum <- NA
  }
  if (!"zoomSize" %in% colnames(data)) {
    data$zoomSize <- NA
  }
  if (!"followVehicleFocusTimeSum" %in% colnames(data)) {
    data$followVehicleFocusTimeSum <- NA
  }
  if (!"followPathEndFocusTimeSum" %in% colnames(data)) {
    data$followPathEndFocusTimeSum <- NA
  }


  return(data)
})

# Combine all data frames into a single data frame
all_data_neglected_time <- do.call(rbind, data_list)

# Replace the comma with a period in the elapsedTime column and convert it to numeric
all_data_neglected_time$currentlyNeglectedTime <- as.numeric(gsub(",", ".", all_data_neglected_time$currentlyNeglectedTime))


subset_all_data_neglected_time <- subset(all_data_neglected_time, UserID == "AS21")

subset_wanted <- subset_all_data_neglected_time[, c("UserID", "ScenarioID", "currentlyNeglectedTime")]


# Function to calculate average of max values before a 0 and count transitions to 0
avgMax_and_count_zero_transition <- function(x) {
  # Check if all values are 0
  if (all(x == 0)) {
    return(data.frame(avgMax = 0, zero_transition_count = 0))
  }

  # Check if only the last value is not 0
  if (all(x[-length(x)] == 0)) {
    return(data.frame(avgMax = x[length(x)], zero_transition_count = 0))
  }


  current_max <- 0
  max_list <- c()
  zero_transition_count <- 0
  prev_val <- NULL

  for (val in x) {
    if (val > current_max) {
      current_max <- val
    }

    if (val == 0 && !is.null(prev_val) && prev_val != 0) {
      zero_transition_count <- zero_transition_count + 1
      max_list <- c(max_list, current_max)
      current_max <- 0
    }

    prev_val <- val
  }

  # Handle case where max_list is empty (no transitions to zero)
  avgMax <- ifelse(length(max_list) == 0, 0, mean(max_list))

  return(data.frame(avgMax = avgMax, zero_transition_count = zero_transition_count))
}



# Compute the average max and zero transition count for each combination
result <- all_data_neglected_time %>%
  group_by(UserID, ScenarioID) %>%
  do(avgMax_and_count_zero_transition(.$currentlyNeglectedTime))



result$interaction <- as.factor(ifelse(result$ScenarioID %in% c("1", "4", "7", "10"), "Trajectory", ifelse(result$ScenarioID %in% c("2", "5", "8", "11"), "Path planning", "Waypoint")))
result$numberOfRequests <- factor(ifelse(result$ScenarioID %in% c("1", "2", "3"), "One", ifelse(result$ScenarioID %in% c("4", "5", "6"), "Two", ifelse(result$ScenarioID %in% c("7", "8", "9"), "Three", "Four"))), levels = c("One", "Two", "Three", "Four"))


result$UserID <- as.factor(result$UserID)
result$interaction <- as.factor(result$interaction)
result$numberOfRequests <- as.factor(result$numberOfRequests)

checkAssumptionsForAnovaTwoFactors(data = result, y = "avgMax", factor_1 = "interaction", factor_2 = "numberOfRequests")

modelArt <- art(avgMax ~ interaction * numberOfRequests + Error(UserID / (interaction * numberOfRequests)), data = result) |> anova()
modelArt
reportART(modelArt, dv = "avgMax")


result %>% ggplot() +
  aes(x = interaction, y = avgMax, fill = numberOfRequests, colour = numberOfRequests, group = numberOfRequests) +
  # scale_colour_manual(values = wes_palette("Cavalcanti1", n = 5)) +
  scale_color_see() +
  ylab("Average of maximum neglected time") +
  theme(legend.position.inside = c(0.85, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) # 95 % mean_cl_boot is 95% confidence intervals
ggsave("plots/avgMax_interaction.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)











#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################



#### Sankey Diagram ####
# diagramm elpased time mit isMainRequest	isSecondaryRequest
# SankeyNetwork
# Library
# see https://r-graph-gallery.com/321-introduction-to-interactive-sankey-diagram-2.html
# kann besser mit TimeStamp (additionalInfo ist ID)



# Read all CSV files with separator ";"
data_list <- lapply(filesTimeStamps, function(file) {
  read.csv(file, sep = ";")
})

# Combine all data frames into a single data frame
all_data_timestamps <- do.call(rbind, data_list)

# Replace the comma with a period in the elapsedTime column and convert it to numeric
all_data_timestamps$elapsedTime <- as.numeric(gsub(",", ".", all_data_timestamps$elapsedTime))

all_data_timestamps$interaction <- as.factor(ifelse(all_data_timestamps$scenarioID %in% c("1", "4", "7", "10"), "Trajectory", ifelse(all_data_timestamps$scenarioID %in% c("2", "5", "8", "11"), "Path planning", "Waypoint")))
all_data_timestamps$numberOfRequests <- factor(ifelse(all_data_timestamps$scenarioID %in% c("1", "2", "3"), "One", ifelse(all_data_timestamps$scenarioID %in% c("4", "5", "6"), "Two", ifelse(all_data_timestamps$scenarioID %in% c("7", "8", "9"), "Three", "Four"))), levels = c("One", "Two", "Three", "Four"))




# Create the 'source' and 'target' columns by shifting the timeStampEvent column
transitions <- data.frame(
  elapsedTime = all_data_timestamps$elapsedTime,
  timeStampEvent = all_data_timestamps$timeStampEvent,
  userID = all_data_timestamps$userID,
  interaction = all_data_timestamps$interaction,
  numberOfRequests = all_data_timestamps$numberOfRequests,
  additionalInfo = all_data_timestamps$additionalInfo,
  source = all_data_timestamps$timeStampEvent,
  target = c(tail(all_data_timestamps$timeStampEvent, -1), NA)
)

# Remove the last row as it will have a NA target
transitions <- transitions[-nrow(transitions), ]

# Extracting unique event names from the source and target columns
nodes <- unique(c(transitions$source, transitions$target))
nodes <- nodes[!is.na(nodes)] # Remove NA if present

# Creating a data frame for the nodes
nodes_df <- data.frame(
  name = c(as.character(transitions$source), as.character(transitions$target)) %>%
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
transitions$IDsource <- match(transitions$source, nodes_df$name) - 1
transitions$IDtarget <- match(transitions$target, nodes_df$name) - 1

transitions$value <- 0.01


# Adding a nodes column to the transitions data frame, repeating the nodes to match the number of rows
transitions$nodes <- rep(nodes_df$name, length.out = nrow(transitions))




p <- networkD3::sankeyNetwork(
  Links = transitions, Nodes = nodes_df,
  Source = "IDsource", Target = "IDtarget", NodeID = "name", Value = "value", units = "TWh", sinksRight = FALSE, iterations = 2
)
p

networkD3::saveNetwork(p, "sn.html", selfcontained = TRUE)

# Remove transitions that go to the same node again
transitions <- transitions[transitions$IDsource != transitions$IDtarget, ]

p <- networkD3::sankeyNetwork(
  Links = transitions, Nodes = nodes_df,
  Source = "IDsource", Target = "IDtarget", NodeID = "name", Value = "value", units = "TWh", sinksRight = FALSE, iterations = 2
)
networkD3::saveNetwork(p, "sn_no_same_node.html", selfcontained = TRUE)

# Ensure transitions are only in one direction
# transitions <- transitions[!duplicated(t(apply(transitions[, c("IDsource", "IDtarget")], 1, sort))),]

# p <- networkD3::sankeyNetwork(Links = transitions, Nodes = nodes_df,
#                               Source = "IDsource", Target = "IDtarget", NodeID = "name", Value = "value", units = 'TWh', sinksRight = FALSE, iterations = 2)
# networkD3::saveNetwork(p, "sn_no_same_node_one_direction.html", selfcontained = TRUE)



# Create an edge list from the transitions data
edges <- data.frame(
  from = transitions$source,
  to = transitions$target
)


# Count the number of transitions between each source and target
transition_counts <- aggregate(x = transitions["source"], by = list(transitions$source, transitions$target), FUN = length)
colnames(transition_counts) <- c("source", "target", "count")

# Create an edge list with the counts
edges <- data.frame(
  from = transition_counts$source,
  to = transition_counts$target,
  weight = transition_counts$count
)

g <- graph_from_data_frame(edges, directed = TRUE)

# Choose a layout (Fruchterman-Reingold in this case)
layout <- layout_with_fr(g)

# Customize vertex and edge attributes
V(g)$size <- 15 # Vertex size
V(g)$label.cex <- 0.8 # Vertex label size
V(g)$color <- "lightblue" # Vertex color
E(g)$arrow.size <- 0.5 # Arrow size for directed edges
E(g)$color <- "grey" # Edge color

# Plot using tkplot with customized parameters
tkplot(g,
  layout = layout,
  edge.width = E(g)$weight / 10,
  edge.label = E(g)$weight,
  edge.label.cex = 0.6, # Edge label size
  vertex.label.color = "black", # Vertex label color
  vertex.frame.color = "darkblue"
) # Vertex border color


# tkplot(g, edge.width = E(g)$weight/10, edge.label = E(g)$weight)





# Get unique scenarioIDs and controlModes
unique_interaction <- unique(transitions$interaction)
unique_numberOfRequests <- unique(transitions$numberOfRequests)

# Loop through each combination of scenarioID and controlMode
for (interaction in unique_interaction) {
  for (numberOfRequests in unique_numberOfRequests) {
    # Subset data based on current scenarioID and controlMode
    subset_data <- subset(transitions, interaction == interaction & numberOfRequests == numberOfRequests)

    # Skip empty subsets
    if (nrow(subset_data) == 0) {
      next
    }

    # Create an edge list from the subset data
    edges <- data.frame(
      from = subset_data$source,
      to = subset_data$target
    )

    # Count the number of transitions between each source and target
    transition_counts <- aggregate(x = subset_data["source"], by = list(subset_data$source, subset_data$target), FUN = length)
    colnames(transition_counts) <- c("source", "target", "count")

    # Create an edge list with the counts
    edges <- data.frame(
      from = transition_counts$source,
      to = transition_counts$target,
      weight = transition_counts$count
    )

    # Create graph
    g <- graph_from_data_frame(edges, directed = TRUE)

    # Customize layout and attributes
    # layout <- layout_with_fr(g, dim = 2, niter = 1000)
    layout <- layout_with_graphopt(g)
    V(g)$size <- 15
    V(g)$label.cex <- 0.8
    V(g)$color <- "lightblue"
    E(g)$arrow.size <- 2.8
    E(g)$color <- "grey"



    # Generate filename using scenarioID and controlMode
    filename <- paste0("Graph_interaction_", interaction, "_numberOfRequests_", numberOfRequests, ".pdf")

    # Save the plot to PNG format
    pdf(filename)
    plot(g,
      layout = layout,
      edge.width = E(g)$weight / 100,
      edge.label = E(g)$weight,
      vertex.label.color = "black",
      # vertex.frame.color = "darkblue",
      vertex.frame.color = "#0000FF80", # Blue with alpha = 0.5
      edge.color = adjustcolor("grey", alpha.f = 0.5) # Grey with alpha
    )
    dev.off()

    # if we want interactive:

    # Plot using tkplot
    # tkplot(g,
    #        layout = layout,
    #        edge.width = E(g)$weight/10,
    #        edge.label = E(g)$weight,
    #        edge.label.cex = 0.6,
    #        vertex.label.color = "black",
    #        vertex.frame.color = "darkblue"
    # )
  }
}
















#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################




#### Mouse Movements ####
# wie viel wurde Maus bewegt: OverviewLog_* : MouseTravelDistanceInCm (ACHUNG erst ab Tag 2)

# Read all CSV files with separator ";"

data_list <- lapply(filesOverviewLog1, function(file) {
  data <- read.csv(file, sep = ";")

  # was not available on first day
  if (!"MouseTravelDistanceInCm" %in% colnames(data)) {
    data$MouseTravelDistanceInCm <- NA
  }


  return(data)
})


# Combine all data frames into a single data frame
all_data_mouse <- do.call(rbind, data_list)


all_data_mouse$interaction <- as.factor(ifelse(all_data_mouse$scenarioID %in% c("1", "4", "7", "10"), "Trajectory", ifelse(all_data_mouse$scenarioID %in% c("2", "5", "8", "11"), "Path planning", "Waypoint")))
all_data_mouse$numberOfRequests <- factor(ifelse(all_data_mouse$scenarioID %in% c("1", "2", "3"), "One", ifelse(all_data_mouse$scenarioID %in% c("4", "5", "6"), "Two", ifelse(all_data_mouse$scenarioID %in% c("7", "8", "9"), "Three", "Four"))), levels = c("One", "Two", "Three", "Four"))

all_data_mouse$userID <- as.factor(all_data_mouse$userID)


result_mouse <- all_data_mouse %>%
  group_by(userID, interaction, numberOfRequests) %>%
  summarise(maxMouseTravelDistanceInCm = max(MouseTravelDistanceInCm, na.rm = TRUE)) %>%
  ungroup()

# number is with ","

result_mouse$maxMouseTravelDistanceInCm <- as.numeric(gsub(",", ".", result_mouse$maxMouseTravelDistanceInCm))





checkAssumptionsForAnovaTwoFactors(data = result_mouse, y = "maxMouseTravelDistanceInCm", factor_1 = "interaction", factor_2 = "numberOfRequests")

# remove the three from first day
result_mouse <- na.omit(result_mouse)

modelArtMouse <- art(maxMouseTravelDistanceInCm ~ interaction * numberOfRequests + Error(userID / (interaction * numberOfRequests)), data = result_mouse) |> anova()
modelArtMouse
reportART(modelArtMouse, dv = "maxMouseTravelDistanceInCm")

result_mouse %>% ggplot() +
  aes(x = interaction, y = maxMouseTravelDistanceInCm, fill = numberOfRequests, colour = numberOfRequests, group = numberOfRequests) +
  # scale_colour_manual(values = wes_palette("Cavalcanti1", n = 5)) +
  scale_color_see() +
  ylab("Max Mouse Travel Distance (cm)") +
  theme(legend.position.inside = c(0.91, 0.25)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.1), alpha = 0.5) # 95 % mean_cl_boot is 95% confidence intervals
ggsave("plots/maxMouseTravelDistanceInCm_interaction.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)
