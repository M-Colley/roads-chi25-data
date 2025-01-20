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
library(Cairo)

library(cowplot) # plot merge
library(patchwork) # plot merge



filesTimeStamps <- list.files(path = "./ClearedLogs/", recursive = TRUE, pattern = "TimestampLog.*\\.csv", full.names = TRUE)
# exclude those with a "-"
filesTimeStamps <- filesTimeStamps[!grepl("/.+-", filesTimeStamps)]


filesLog <- list.files(path = "./ClearedLogs/", recursive = TRUE, pattern = "log.*\\.csv", full.names = TRUE)
# exclude those with a "-"
filesLog <- filesLog[!grepl("/.+-", filesLog)]
filesLog

filesOverviewLog <- list.files(path = "./ClearedLogs/", recursive = TRUE, pattern = "OverviewLog.*\\.csv", full.names = TRUE)
# exclude those with a "-"
filesOverviewLog <- filesOverviewLog[!grepl("/.+-", filesOverviewLog)]
filesOverviewLog

# Exclude those with a "-" and those containing "Tag1"
filesOverviewLog1 <- filesOverviewLog[!grepl("/.+-|Tag1", filesOverviewLog)]
filesOverviewLog1


TO_METER_DIVISOR <- 0.875

### get left lane Data files:
data_leftSiteLanes <- read.csv('laneData/leftSiteLanes.csv', sep = ';')

# Convert commas to periods in the X and Y columns
data_leftSiteLanes$X <- as.numeric(gsub(",", ".", data_leftSiteLanes$X))
data_leftSiteLanes$Y <- as.numeric(gsub(",", ".", data_leftSiteLanes$Y))

# Mutate Coordinates to Meter metric
data_leftSiteLanes <- data_leftSiteLanes %>%
  mutate_at(vars(X, Y), funs(. / TO_METER_DIVISOR))

### get left lane middle Data files
data_leftMiddleLanes <- read.csv('laneData/leftSiteLanesMiddle.csv', sep = ';')

# Convert commas to periods in the X and Y columns
data_leftMiddleLanes$X <- as.numeric(gsub(",", ".", data_leftMiddleLanes$X))
data_leftMiddleLanes$Y <- as.numeric(gsub(",", ".", data_leftMiddleLanes$Y))

# Mutate Coordinates to Meter metric
data_leftMiddleLanes <- data_leftMiddleLanes %>%
  mutate_at(vars(X, Y), funs(. / TO_METER_DIVISOR))

### get left lane POIs
data_leftPOI <- read.csv('laneData/leftSitePOI.csv', sep = ';')

# Convert commas to periods in the X and Y columns
data_leftPOI$X <- as.numeric(gsub(",", ".", data_leftPOI$X))
data_leftPOI$Y <- as.numeric(gsub(",", ".", data_leftPOI$Y))

# Mutate Coordinates to Meter metric
data_leftPOI <- data_leftPOI %>%
  mutate_at(vars(X, Y), funs(. / TO_METER_DIVISOR))



# # Create scatter plot
# plot <- ggplot(data_leftSiteLanes, aes(x = X, y = Y)) +
#   geom_point() #+
#   #theme_void()  # Removes axis and labels
# ggsave("testPlots/rightLanes.pdf", width = pdfwidth, height = pdfheight + 2, device = pdf)
# print(plot)

# Plot as lines
# plot(data_leftSiteLanes$X, data_leftSiteLanes$Y, type = "l", col = "red", main = "Rotated Line vs Original Line")

#####################################################
#####################################################

# Function to rotate points around a specified point
rotate_point <- function(x, y, cx, cy, angle_degrees) {
  angle_radians <- angle_degrees * pi / 180
  x_rotated <- (x - cx) * cos(angle_radians) - (y - cy) * sin(angle_radians) + cx
  y_rotated <- (x - cx) * sin(angle_radians) + (y - cy) * cos(angle_radians) + cy
  return(c(x_rotated, y_rotated))
}

#####################################################
#####################################################
# Specify the center of rotation and the rotation angle <- only need to have a even line <- use origin
center_x <- 0
center_y <- 0

rotation_angle <- -107

# Apply the rotation to each row in the data frame - left site
df_rotatedLeftLanes <- data_leftSiteLanes
df_rotatedLeftLanes[c("X", "Y")] <- t(apply(data_leftSiteLanes[, c("X", "Y")], 1, function(row) rotate_point(row[1], row[2], center_x, center_y, rotation_angle)))

# Apply the rotation to each row in the data frame - left site middle
df_rotatedLeftLanes_middle <- data_leftMiddleLanes
df_rotatedLeftLanes_middle[c("X", "Y")] <- t(apply(df_rotatedLeftLanes_middle[, c("X", "Y")], 1, function(row) rotate_point(row[1], row[2], center_x, center_y, rotation_angle)))


df_rotatedLeftPOI <- data_leftPOI
df_rotatedLeftPOI[c("X", "Y")] <- t(apply(df_rotatedLeftPOI[, c("X", "Y")], 1, function(row) rotate_point(row[1], row[2], center_x, center_y, rotation_angle)))

df_rotatedLeftPOI

####################################################
# Read and combine all log files into a single data frame with a semicolon separator
all_logs <- lapply(filesLog, function(file) {
  read.csv(file, sep = ";", header = TRUE)
}) %>%
  bind_rows()

# Assuming all_logs is your dataframe
position_logs <- all_logs %>%
  mutate(
    coordinates = strsplit(gsub("[()]", "", vehiclePosition), ",\\s*"),
    vehiclePosX = as.numeric(sapply(coordinates, function(coord) coord[1])),
    vehiclePosY = as.numeric(sapply(coordinates, function(coord) coord[3])
    )) %>%
      select(-vehiclePosition, -coordinates)
    
# Mutate Coordinates to Meter metric
position_logs <- position_logs %>%
  mutate_at(vars(vehiclePosX, vehiclePosY), funs(. / TO_METER_DIVISOR))

# Split by sideofConstructionSite and select relevant columns
df_right <- position_logs %>%
  filter(sideOfConstructionSite == 'Right') %>%
  select(sideOfConstructionSite, UserID, ScenarioID, requestID, vehiclePosX, vehiclePosY)
df_left <- position_logs %>%
  filter(sideOfConstructionSite == 'Left') %>%
  select(sideOfConstructionSite, UserID, ScenarioID, requestID, vehiclePosX, vehiclePosY)


# Apply the rotation to each row in the data frame - leftdf (car)
df_left_rotated <- df_left
df_left_rotated[c("vehiclePosX", "vehiclePosY")] <- t(apply(df_left_rotated[, c("vehiclePosX", "vehiclePosY")], 1, function(row) rotate_point(row[1], row[2], center_x, center_y, rotation_angle)))


# Apply the rotation to each row in the data frame - rightdf (car)
df_right_rotated <- df_right
df_right_rotated[c("vehiclePosX", "vehiclePosY")] <- t(apply(df_right_rotated[, c("vehiclePosX", "vehiclePosY")], 1, function(row) rotate_point(row[1], row[2], center_x, center_y, rotation_angle)))

#########################################################
#########################################################
# plot left(global):

combined_plot <- ggplot(df_left_rotated, aes(x = vehiclePosX, y = vehiclePosY, group = paste(UserID, ScenarioID, requestID))) +
  geom_line(size = 0.2) +
  geom_point(data = df_rotatedLeftLanes, aes(x = X, y = Y), color = "red", shape = 1, size = 1, inherit.aes = FALSE) +
  theme_minimal()

# Set equal aspect ratios
combined_plot <- combined_plot + coord_fixed()

# Display the combined plot
print(combined_plot)
ggsave("testPlots/combined.pdf", combined_plot, width = 20, height = 10, device = pdf)


backup <- df_left_rotated
df_left_rotated <- backup
#########################################################
#########################################################
# plot left for each scenario


# cut all vehiclePosX values < 200 in df_left_rotated (not sure if we should do this?)
df_left_rotated <- df_left_rotated %>% filter(vehiclePosX >= 180)
df_rotatedLeftLanes_middle <- df_rotatedLeftLanes_middle %>% filter(X >= 180)
df_rotatedLeftLanes <- df_rotatedLeftLanes %>% filter(X >= 180)

# Find the minimum y-value across all data frames
min_y <- min(c(min(df_left_rotated$vehiclePosY), min(df_rotatedLeftLanes_middle$Y), min(df_rotatedLeftLanes$Y)))
min_y

# Update each data frame by adding the negative of the minimum y-value
df_left_rotated$vehiclePosY <- df_left_rotated$vehiclePosY - min_y
df_rotatedLeftLanes_middle$Y <- df_rotatedLeftLanes_middle$Y - min_y
df_rotatedLeftLanes$Y <- df_rotatedLeftLanes$Y - min_y


# Split data frame by scenarioID into list
list_leftPos_scenarioID <- split(df_left_rotated, df_left_rotated$ScenarioID)


# Function to generate the combined plot for a given scenario [1,12]
generate_combined_plotLeft <- function(num, title=NULL) {
  annoCol = "black"
  
  combined_plot <- ggplot(list_leftPos_scenarioID[[num]], aes(x = vehiclePosX, y = vehiclePosY, group = paste(UserID, ScenarioID, requestID))) +
    geom_path(color = "red", alpha = 0.65, size = 0.2) +
    geom_path(data = df_rotatedLeftLanes, aes(x = X, y = Y, group = LaneName), color = "black", size = 0.6) +
    geom_path(data = df_rotatedLeftLanes_middle, aes(x = X, y = Y, group = LaneName), color = "green", alpha = 1, linetype = 2, size = 0.6) +
    #labs( x = "Longitudinal distance (m)", y = "Lateral distance (m)") +
    labs( x = "", y = "") +
    #geom_vline(xintercept = 0, linetype = "dashed", color = annoCol) + #annotations of POI for left construction site
    geom_vline(xintercept = 200, linetype = "dashed", color = annoCol) +
    geom_vline(xintercept = 215, linetype = "dashed", color = annoCol) +
    geom_vline(xintercept = 600, linetype = "dashed", color = annoCol) +
    #annotate("text", x = 5, y = -8, label = "[1]", vjust = -0.5, color = annoCol) +
    annotate("text", x = 190, y = -2, label = str_wrap("Initial path end", width = 10), vjust = -0.2, color = annoCol) +
    annotate("text", x = 231, y = -2, label = str_wrap("Start of yellow marking", width = 20), vjust = -0.2, color = annoCol) +
    annotate("text", x = 588, y = 12.5, label = "End of \ninteraction", vjust = 0, color = annoCol) +
    theme_minimal() #+
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) #+
    coord_fixed() # fixed x and y axis scalings?
  
  if (!is.null(title)) {
    combined_plot <- combined_plot + ggtitle(title)
  }
  
  #ggsave(paste("testPlots/combinedLeft", num, ".pdf"), combined_plot, width = 20, height = 10, device = pdf)
  return(combined_plot)
}

##############################################################
a <- generate_combined_plotLeft(1, "Trajectory - 1 parallel request")
b <- generate_combined_plotLeft(2, "Path Planning - 1 parallel request")
c <- generate_combined_plotLeft(3, "Waypoints - 1 parallel request")

# Combine and stack plots using patchwork
multiPlot <- a / b / c
print(multiPlot)
ggsave("testPlots/multiPlot_1_2_3.pdf", multiPlot, device = pdf, width = 20, height=10)

##############################################################
a <- generate_combined_plotLeft(4, "Trajectory - 2 parallel requests")
b <- generate_combined_plotLeft(5, "Path Planning - 2 parallel requests")
c <- generate_combined_plotLeft(6, "Waypoints - 2 parallel requests")

# Combine and stack plots using patchwork
multiPlot <- a / b / c
print(multiPlot)
ggsave("testPlots/multiPlot_4_5_6.pdf", multiPlot, device = pdf, width = 20, height=10)

##############################################################
a <- generate_combined_plotLeft(7, "Trajectory - 3 parallel requests")
b <- generate_combined_plotLeft(8, "Path Planning - 3 parallel requests")
c <- generate_combined_plotLeft(9, "Waypoints - 3 parallel requests")

# Combine and stack plots using patchwork
multiPlot <- a / b / c
print(multiPlot)
ggsave("testPlots/multiPlot_7_8_9.pdf", multiPlot, device = pdf, width = 20, height=10)

##############################################################
a <- generate_combined_plotLeft(10, "Trajectory - 4 parallel requests")
b <- generate_combined_plotLeft(11, "Path Planning - 4 parallel requests")
c <- generate_combined_plotLeft(12, "Waypoints - 4 parallel requests")

# Combine and stack plots using patchwork
multiPlot <- a / b / c
print(multiPlot)
ggsave("testPlots/multiPlot_10_11_12.pdf", multiPlot, device = pdf, width = 20, height=10)


