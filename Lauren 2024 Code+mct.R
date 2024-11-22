library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(grid)
library(gridExtra)


##set working directory so this code functions on my side. Alter or remove as needed.
# setwd("C:/Users/llipsius/CSI Pacific Dropbox/Lauren Lipsius/CCBC dashboard")


SnC <- read_xlsx("../MasterSheet.xlsx", sheet = "SnC")
May <- SnC %>% 
  group_by (Date) %>%
  filter(Date>"2024-05-17") %>%
  mutate(Age_Group = case_when(
    `Birth Year` == 2013 ~ "U12",
    `Birth Year` == 2012 ~ "U12",
    `Birth Year` == 2011 ~ "U14",
    `Birth Year` == 2010 ~ "U14",
    `Birth Year` == 2009 ~ "U16",
    `Birth Year` == 2008 ~ "U16",
    `Birth Year` == 2007 ~ "U18",
    `Birth Year` == 2006 ~ "U18",
    `Birth Year` == 2005 ~ "U20",
    `Birth Year` == 2004 ~ "U20",
    `Birth Year` == 2003 ~ "U23",
    `Birth Year` == 2002 ~ "U23",
    `Birth Year` == 2001 ~ "U23",
  ))




##2yAgeGroup Bench Press Scores
May <- May %>%
  group_by(Age_Group, Sex) %>%
  mutate(Age_Group_Press_Avg = mean(`AMRAP TEMPO Bench Press (120BPM, 1-0-1-0, to block on chest)`, na.rm = TRUE)) %>%
  mutate(Age_Group_Press_Sd = sd(`AMRAP TEMPO Bench Press (120BPM, 1-0-1-0, to block on chest)`, na.rm = TRUE)) %>%
  mutate(across(starts_with("Age_Group_Press"), ~replace(., is.na(.), 0))) %>%
  ungroup()

##2yAgeGroup Bench Pull Scores
May <- May %>%
  group_by(Age_Group, Sex) %>%
  mutate(Age_Group_Pull_Avg = mean(`AMRAP TEMPO Bench Pull (120BPM, 1-0-1-0, to block on chest)`, na.rm = TRUE)) %>%
  mutate(Age_Group_Pull_Sd = sd(`AMRAP TEMPO Bench Pull (120BPM, 1-0-1-0, to block on chest)`, na.rm = TRUE)) %>%
  mutate(across(starts_with("Age_Group_Pull"), ~replace(., is.na(.), 0))) %>%
  ungroup()

##2yAgeGroup Pull Up Scores
May <- May %>%
  group_by(Age_Group, Sex) %>%
  mutate(Age_Group_PullUp_Avg = mean(`Continuous AMRAP Pull-Up`, na.rm = TRUE)) %>%
  mutate(Age_Group_PullUp_Sd = sd(`Continuous AMRAP Pull-Up`, na.rm = TRUE))%>%
  mutate(across(starts_with("Age_Group_PullUp"), ~replace(., is.na(.), 0))) %>%
  mutate(PullUpGoal = case_when(
    Sex == 'F' ~ 10,
    Sex == 'M' ~ 15)) %>%
  ungroup()

##2yAgeGroup Jump Height Scores
May <- May %>%
  group_by(Age_Group, Sex) %>%
  mutate(Age_Group_Jump_Height_Avg= mean(`CMJ Height (TOV)`, na.rm = TRUE)) %>%
  mutate(Age_Group_Jump_Height_Sd = sd(`CMJ Height (TOV)`, na.rm = TRUE)) %>%
  mutate(across(starts_with("Age_Group_Jump_Height"), ~replace(., is.na(.), 0))) %>%
  ungroup()

May <- May %>%
  group_by(Age_Group, Sex) %>%
  mutate(Age_Group_CMJRSI_Avg = mean(`CMJ RSI`, na.rm = TRUE)) %>%
  mutate(Age_Group_CMJRSI_Sd = sd(`CMJ RSI`, na.rm = TRUE)) %>%
  mutate(across(starts_with("Age_Group_CMJRSI"), ~replace(., is.na(.), 0))) %>%
  ungroup()

May <- May %>%
  group_by(Age_Group, Sex) %>%
  mutate(Age_Group_CMJPP_Avg = mean(`CMJ PP`, na.rm = TRUE)) %>%
  mutate(Age_Group_CMJPP_Sd = sd(`CMJ PP`, na.rm = TRUE)) %>%
  mutate(across(starts_with("Age_Group_CMJPPkg"), ~replace(., is.na(.), 0))) %>%
  ungroup()

May <- May %>%
  group_by(Age_Group, Sex) %>%
  mutate(Age_Group_IMTP_Avg = mean(`IMTP Peak (N/N)`, na.rm = TRUE)) %>%
  mutate(Age_Group_IMTP_Sd = sd(`IMTP Peak (N/N)`, na.rm = TRUE)) %>%
  mutate(across(starts_with("Age_Group_imtpPF"), ~replace(., is.na(.), 0))) %>%
  mutate(IMTPThreshold = 3) %>%
  ungroup()

May <- May %>%
  group_by(Age_Group, Sex) %>%
  mutate(Age_Group_MedBallThrow_Avg = mean(`Seated Med Ball Throw(m)`, na.rm = TRUE)) %>%
  mutate(Age_Group_MedBallThrow_Sd = sd(`Seated Med Ball Throw(m)`, na.rm = TRUE)) %>%
  mutate(across(starts_with("Age_Group_MedBallThrow"), ~replace(., is.na(.), 0))) %>%
  ungroup()

May <- May %>%
  group_by(Age_Group, Sex) %>%
  mutate(Age_Group_avg.force.Nkg_Avg = mean(`Force Plate Drop and Stick 50cm Peak Force`, na.rm = TRUE)) %>%
  mutate(Age_Group_avg.force.Nkg_Sd = sd(`Force Plate Drop and Stick 50cm Peak Force`, na.rm = TRUE)) %>%
  mutate(across(starts_with("Age_Group_avg.force.Nkg"), ~replace(., is.na(.), 0))) %>%
  mutate(DnSThreshold = 3) %>%
  ungroup()

##This section for creating age group comparisons.  
##I attempted to use a TryCatch to replace empty graphs with an error message but failed.

for (i in 1:nrow(May)) {
  
  ##Plot BenchPress for each athlete's age group
  
  # Extract values for the current row
  mean_value_press <- May$Age_Group_Press_Avg[i]
  std_dev_press <- May$Age_Group_Press_Sd[i]
  
  # Generate data for the normal distribution
  try(x_press <- seq(mean_value_press - 3 * std_dev_press, mean_value_press + 3 * std_dev_press, length.out = 100), silent = TRUE)
  try(y_press <- dnorm(x_press, mean = mean_value_press, sd = std_dev_press), silent = TRUE)
  
  if (!is.na(std_dev_press) && !is.na(std_dev_press && !is.na(May$`AMRAP TEMPO Bench Press (120BPM, 1-0-1-0, to block on chest)`[i]))) {
    # Create a data frame for plotting
    plot_data_press <- data.frame(x = x_press, y = y_press)}
  
  # Plot the bell curve
  plot1 <- ggplot(plot_data_press, aes(x, y)) +
    geom_line(color = "black", linewidth = 1) +
    
    # Plot the athlete score
    geom_vline(xintercept = May$`AMRAP TEMPO Bench Press (120BPM, 1-0-1-0, to block on chest)`[i], linetype = "dashed", color = "black", linewidth = 1) +
    
    # Plot the line 1 standard deviation below the mean
    geom_vline(xintercept = mean_value_press - std_dev_press, linetype = "dashed", color = "red", linewidth = 1) +
    
    # Fill the space below the 1 standard deviation line with red
    geom_ribbon(data = subset(plot_data_press, x <= mean_value_press - std_dev_press),
                aes(ymax = y, ymin = 0), fill = "red", alpha = 0.3) +
    
    # Fill the space above the below 1sd line with green
    geom_ribbon(data = subset(plot_data_press, x >= mean_value_press - std_dev_press),
                aes(ymax = y, ymin = 0), fill = "green", alpha = 0.3) +
    
    # Add labels and theme
    labs(title = "Tempo Bench Press Athlete Score and 1 Std Dev Below 2y Age Group Mean",
         x = "Athlete Score",
         y = "") +
         scale_y_continuous(labels = NULL, breaks = NULL) +
    theme_minimal()  
  
  
  
  
  ## Plot Pull for each athlete's age group
  
  
  
  # Extract values for the current row
  mean_value_pull <- May$Age_Group_Pull_Avg[i]
  std_dev_pull <- May$Age_Group_Pull_Sd[i]
  
  # Generate data for the normal distribution
  try(x_pull <- seq(mean_value_pull - 3 * std_dev_pull, mean_value_pull + 3 * std_dev_pull, length.out = 100), silent = TRUE)
  try(y_pull <- dnorm(x_pull, mean = mean_value_pull, sd = std_dev_pull), silent = TRUE)
  
  # Create a data frame for plotting
  plot_data_pull <- data.frame(x = x_pull, y = y_pull)
  
  # Plot the bell curve
  plot2 <- ggplot(plot_data_pull, aes(x, y)) +
    geom_line(color = "black", linewidth = 1) +
    
    # Plot the athlete score
    geom_vline(xintercept = May$`AMRAP TEMPO Bench Pull (120BPM, 1-0-1-0, to block on chest)`[i], linetype = "dashed", color = "black", linewidth = 1) +
    
    # Plot the line 1 standard deviation below the mean
    geom_vline(xintercept = mean_value_pull - std_dev_pull, linetype = "dashed", color = "red", linewidth = 1) +
    
    # Fill the space below the 1 standard deviation line with red
    geom_ribbon(data = subset(plot_data_pull, x <= mean_value_pull - std_dev_pull),
                aes(ymax = y, ymin = 0), fill = "red", alpha = 0.3) +
    
    # Fill the space above the below 1sd line with green
    geom_ribbon(data = subset(plot_data_pull, x >= mean_value_pull - std_dev_pull),
                aes(ymax = y, ymin = 0), fill = "green", alpha = 0.3) +
    
    # Add labels and theme
    labs(title = "Tempo Bench Pull Reps and 1 Std Dev Below 2y Age Group Mean",
         x = "Athlete Score",
         y = "") +
         scale_y_continuous(labels = NULL, breaks = NULL) +
    
    theme_minimal()  
  
  
  
  ##Plot PullUp for each athlete's age group
  
  
  # Extract values for the current row
  mean_value_PullUp <- May$Age_Group_PullUp_Avg[i]
  std_dev_PullUp <- May$Age_Group_PullUp_Sd[i]
  nat_team_goal <- May$PullUpGoal[i]
  
  # Generate data for the normal distribution
  try (x_PullUp <- seq(mean_value_PullUp - 3 * std_dev_PullUp, mean_value_PullUp + 3 * std_dev_PullUp, length.out = 100), silent = TRUE)
  try (y_PullUp <- dnorm(x_PullUp, mean = mean_value_PullUp, sd = std_dev_PullUp), silent = TRUE)
  
  # Create a data frame for plotting
  plot_data_PullUp <- data.frame(x = x_PullUp, y = y_PullUp)
  
  # Plot the bell curve
  plot3 <- ggplot(plot_data_PullUp, aes(x, y)) +
    geom_line(color = "black", linewidth = 1) +
    
    # Plot the athlete score
    geom_vline(xintercept = May$`Continuous AMRAP Pull-Up`[i], linetype = "dashed", color = "black", linewidth = 1) +
    
    # Plot the line 1 standard deviation below the mean
    geom_vline(xintercept = mean_value_PullUp - std_dev_PullUp, linetype = "dashed", color = "red", linewidth = 1) +
    
    # Plot Nat Team Pull Up Goal
    geom_vline(xintercept = nat_team_goal, linetype = "dashed", color = "yellow", linewidth = 1) +
    
    # Fill the space below the 1 standard deviation line with red
    geom_ribbon(data = subset(plot_data_PullUp, x <= mean_value_PullUp - std_dev_PullUp),
                aes(ymax = y, ymin = 0), fill = "red", alpha = 0.3) +
    
    # Fill the space above the above 1sd line with green
    geom_ribbon(data = subset(plot_data_PullUp, x >= mean_value_PullUp - std_dev_PullUp),
                aes(ymax = y, ymin = 0), fill = "green", alpha = 0.3) +
    
    # Add labels and theme
    labs(title = "AMRAP Pull Up Reps and 1 Std Dev Below 2y Age Group Mean",
         x = "Athlete Score",
         y = "") +
         scale_y_continuous(labels = NULL, breaks = NULL) +
    theme_minimal()  
  
  
  ##Plot MedBallThrow for each athlete's age group
  
  # Extract values for the current row
  mean_value_throw <- May$Age_Group_MedBallThrow_Avg[i]
  std_dev_throw <- May$Age_Group_MedBallThrow_Sd[i]
  
  # Generate data for the normal distribution
  try(x_throw <- seq(mean_value_throw - 3 * std_dev_throw, mean_value_throw + 3 * std_dev_throw, length.out = 100), silent = TRUE)
  try(y_throw <- dnorm(x_throw, mean = mean_value_throw, sd = std_dev_throw), silent = TRUE)
  
  if (!is.na(std_dev_throw) && !is.na(std_dev_throw && !is.na(May$`Seated Med Ball Throw(m)`[i]))) {
    # Create a data frame for plotting
    plot_data_throw <- data.frame(x = x_throw, y = y_throw)}
  
  # Plot the bell curve
  plot4 <- ggplot(plot_data_throw, aes(x, y)) +
    geom_line(color = "black", linewidth = 1) +
    
    # Plot the athlete score
    geom_vline(xintercept = May$`Seated Med Ball Throw(m)`[i], linetype = "dashed", color = "black", linewidth = 1) +
    
    # Plot the line 1 standard deviation below the mean
    geom_vline(xintercept = mean_value_throw - std_dev_throw, linetype = "dashed", color = "red", linewidth = 1) +
    
    # Fill the space below the 1 standard deviation line with red
    geom_ribbon(data = subset(plot_data_throw, x <= mean_value_throw - std_dev_throw),
                aes(ymax = y, ymin = 0), fill = "red", alpha = 0.3) +
    
    # Fill the space above the below 1sd line with green
    geom_ribbon(data = subset(plot_data_throw, x >= mean_value_throw - std_dev_throw),
                aes(ymax = y, ymin = 0), fill = "green", alpha = 0.3) +
    
    # Add labels and theme
    labs(title = "Seated Med Ball Throw (m) Athlete Score and 1 Std Dev Below 2y Age Group Mean",
         x = "Athlete Score",
         y = "") +
         scale_y_continuous(labels = NULL, breaks = NULL) +
    
    theme_minimal()  
  
  
  
  ##Plot CMJPPkg for each athlete's age group
  
  
  # Extract values for the current row
  mean_value_CMJPP <- May$Age_Group_CMJPP_Avg[i]
  std_dev_CMJPP <- May$Age_Group_CMJPP_Sd[i]
  
  # Generate data for the normal distribution
  try(x_CMJPP <- seq(mean_value_CMJPP - 3 * std_dev_CMJPP, mean_value_CMJPP + 3 * std_dev_CMJPP, length.out = 100), silent = TRUE)
  try(y_CMJPP <- dnorm(x_CMJPP, mean = mean_value_CMJPP, sd = std_dev_CMJPP), silent = TRUE)
  
  # Create a data frame for plotting
  plot_data_CMJPP <- data.frame(x = x_CMJPP, y = y_CMJPP)
  
  # Plot the bell curve
  plot5 <- ggplot(plot_data_CMJPP, aes(x, y)) +
    geom_line(color = "black", linewidth = 1) +
    
    # Plot the athlete score
    geom_vline(xintercept = May$`CMJ PP`[i], linetype = "dashed", color = "black", linewidth = 1) +
    
    # Plot the line 1 standard deviation below the mean
    geom_vline(xintercept = mean_value_CMJPP - std_dev_CMJPP, linetype = "dashed", color = "red", linewidth = 1) +
    
    # Fill the space below the 1 standard deviation line with red
    geom_ribbon(data = subset(plot_data_CMJPP, x <= mean_value_CMJPP - std_dev_CMJPP),
                aes(ymax = y, ymin = 0), fill = "red", alpha = 0.3) +
    
    # Fill the space above the below 1sd line with green
    geom_ribbon(data = subset(plot_data_CMJPP, x >= mean_value_CMJPP - std_dev_CMJPP),
                aes(ymax = y, ymin = 0), fill = "green", alpha = 0.3) +
    
    # Add labels and theme
    labs(title = "Jump Peak Power per kg and 1 Std Dev Below 2y Age Group Mean",
         x = "Athlete Score",
         y = "") +
         scale_y_continuous(labels = NULL, breaks = NULL) +
    theme_minimal()  
  
  
  
  ##Plot CMJRSI for each athlete's age group
  
  
  
  # Extract values for the current row
  mean_value_CMJRSI <- May$Age_Group_CMJRSI_Avg[i]
  std_dev_CMJRSI <- May$Age_Group_CMJRSI_Sd[i]
  
  # Generate data for the normal distribution
  try( x_CMJRSI <- seq(mean_value_CMJRSI - 3 * std_dev_CMJRSI, mean_value_CMJRSI + 3 * std_dev_CMJRSI, length.out = 100), silent = TRUE)
  try( y_CMJRSI <- dnorm(x_CMJRSI, mean = mean_value_CMJRSI, sd = std_dev_CMJRSI), silent = TRUE)
  
  # Create a data frame for plotting
  plot_data_CMJRSI <- data.frame(x = x_CMJRSI, y = y_CMJRSI)
  
  # Plot the bell curve
  plot6 <- ggplot(plot_data_CMJRSI, aes(x, y)) +
    geom_line(color = "black", linewidth = 1) +
    
    # Plot the athlete score
    geom_vline(xintercept = May$`CMJ RSI`[i], linetype = "dashed", color = "black", linewidth = 1) +
    
    # Plot the line 1 standard deviation below the mean
    geom_vline(xintercept = mean_value_CMJRSI - std_dev_CMJRSI, linetype = "dashed", color = "red", linewidth = 1) +
    
    # Fill the space below the 1 standard deviation line with red
    geom_ribbon(data = subset(plot_data_CMJRSI, x <= mean_value_CMJRSI - std_dev_CMJRSI),
                aes(ymax = y, ymin = 0), fill = "red", alpha = 0.3) +
    
    # Fill the space above the below 1sd line with green
    geom_ribbon(data = subset(plot_data_CMJRSI, x >= mean_value_CMJRSI - std_dev_CMJRSI),
                aes(ymax = y, ymin = 0), fill = "green", alpha = 0.3) +
    
    # Add labels and theme
    labs(title = "Jump Reactive Strength Index (RSImod) and 1 Std Dev Below 2y Age Group Mean",
         x = "Athlete Score",
         y = "") +
         scale_y_continuous(labels = NULL, breaks = NULL) +
    
    theme_minimal()  
  
  
  ##Plot IMTP for each athlete's age group
  
  
  # Extract values for the current row
  mean_value_IMTP <- May$Age_Group_IMTP_Avg[i]
  std_dev_IMTP <- May$Age_Group_IMTP_Sd[i]
  
  # Generate data for the normal distribution
  try(x_IMTP <- seq(mean_value_IMTP - 3 * std_dev_IMTP, mean_value_IMTP + 3 * std_dev_IMTP, length.out = 100), silent = TRUE)
  try(y_IMTP <- dnorm(x_IMTP, mean = mean_value_IMTP, sd = std_dev_IMTP), silent = TRUE)
  
  # Create a data frame for plotting
  plot_data_IMTP <- data.frame(x = x_IMTP, y = y_IMTP)
  
  # Plot the bell curve
  plot7 <- ggplot(plot_data_IMTP, aes(x, y)) +
    geom_line(color = "black", linewidth = 1) +
    
    # Plot the athlete score
    geom_vline(xintercept = May$`IMTP Peak (N/N)`[i], linetype = "dashed", color = "black", linewidth = 1) +
    
    # Plot the line 1 standard deviation below the mean
    geom_vline(xintercept = mean_value_IMTP - std_dev_IMTP, linetype = "dashed", color = "red", linewidth = 1) +
    
    # Fill the space below the 1 standard deviation line with red
    geom_ribbon(data = subset(plot_data_IMTP, x <= mean_value_IMTP - std_dev_IMTP),
                aes(ymax = y, ymin = 0), fill = "red", alpha = 0.3) +
    
    # Fill the space above the below 1sd line with green
    geom_ribbon(data = subset(plot_data_IMTP, x >= mean_value_IMTP - std_dev_IMTP),
                aes(ymax = y, ymin = 0), fill = "green", alpha = 0.3) +
    
    # Add labels and theme
    labs(title = "IMTP (N/N) and 1 Std Dev Below 2y Age Group Mean",
         x = "Athlete Score",
         y = "") +
         scale_y_continuous(labels = NULL, breaks = NULL) +
    theme_minimal() 
  
  ##this adds current score label
  ## + geom_text(aes(x = May$`IMTP Peak (N/N)`[i], y = 0, label = paste("Current Score:", May$`IMTP Peak (N/N)`[i])), vjust = -3, hjust = 2, size = 3)  
  
  
  
  ##Adds a text box with info
  ##textbox <- textGrob("
  ##  The black line refers to the athlete's score.
  ##  The Red line refers to the lower threshold", x = 0.5, y = 0.5, just = "center", gp = gpar(col = "black"))
  ##Then you would add this to the grid.arrange with the plots. 
  
  athletename <- textGrob(May$`Full Name`[i], x = .5, y = .5, just = "center", gp = gpar(col = "black", fontsize = 30))
  general <- str_wrap("These graphs show your score (the black line) against the rest of your age group. The scores below the red line are lower than the average for your age group. The yellow line marks the national team standard. 
                      If the black line (your score) is to the left of the red line, this means your score was significantly below the average for your age group.", width = 100)
  general <- textGrob(general, x = .5, y = .5, just = "center", gp = gpar(col = "black", fontsize = 10))
  nameandinfo <- gridExtra::grid.arrange(athletename, general, ncol = 1)
  notes <- str_wrap(May$Notes[i], width = 100)
  notes <- (textGrob(notes, x = .5, y = .5, just = "center", gp = gpar(col = "black", fontsize = 10)))
  
  
  
  finaldoc <-  gridExtra::grid.arrange(nameandinfo, notes, plot1, plot2, plot3, plot4, plot5, plot6, plot7, nrow = 5)
 ##Tagging these out so you can amalgamate the doc as needed on your side
 # filename <- paste0("ComparisonDoc", May$`Full Name`[i], ".pdf")
 # ggsave(filename, finaldoc, width = 15, height = 8, dpi = 300)  
  
}


##This section to provide visual representation of scores over time

SnC$Date <- as.Date(SnC$Date, format = "%Y-%m-%d")
AthleteData <- SnC[SnC$`Full Name` %in% May$`Full Name`, ]
AthleteData <- split(AthleteData, AthleteData$`Full Name`)
current_date <- Sys.Date()

for (i in names(AthleteData)) {
  
  
  Athlete <- AthleteData[[i]]
  nameandinfo <- textGrob(Athlete$`Full Name`, x = .5, y = .5, just = "center", gp = gpar(col = "black", fontsize = 30))
  
  ##Clean this data
  Athlete_clean <- na.omit(Athlete[, c("Date", "CMJ PP")])
  ##  CMJ PP Comparison Plot
  plot8 <- ggplot(Athlete_clean,  aes(x = Date, y = `CMJ PP`)) +
    geom_point() +
    geom_line() +
    labs(title = "Peak Power in Countermovement Jump (W/kg)",
         x = "Date",
         y = "Peak Power (W/kg)") +
    scale_x_date(limits = c(min(Athlete_clean$Date), current_date), date_labels = "%b %Y")+
    scale_y_continuous(labels = function(x) round(x,2))
  
  ##Clean this data
  Athlete_clean <- na.omit(Athlete[, c("Date", "CMJ RSI")])
  ##  CMJ RSI Comparison Plot
  plot9 <- ggplot(Athlete_clean,  aes(x = Date, y = `CMJ RSI`)) +
    geom_point() +
    geom_line() +
    labs(title = "Modified Reactive Strength Index in Countermovement Jump",
         x = "Date",
         y = "RSI - modified") +
    scale_x_date(limits = c(min(Athlete_clean$Date), current_date), date_labels = "%b %Y")+
    scale_y_continuous(labels = function(x) round(x,2))
  
  ##Clean this data
  Athlete_clean <- na.omit(Athlete[, c("Date", "IMTP Peak (N/N)")])
  ##  IMTP Comparison Plot
  plot10 <- ggplot(Athlete_clean,  aes(x = Date, y = `IMTP Peak (N/N)`)) +
    geom_point() +
    geom_line() +
    labs(title = "Isometric Mid-Thigh Pull Force (N/N)",
         x = "Date",
         y = "IMTP (N/N)") +
    scale_x_date(limits = c(min(Athlete_clean$Date), current_date), date_labels = "%b %Y")+
    scale_y_continuous(labels = function(x) round(x,2))
  
  
  ##Clean this data
  Athlete_clean <- na.omit(Athlete[, c("Date", "Force Plate Drop and Stick 50cm Peak Force")])
  ##  DnS Comparison Plot
  plot11 <- ggplot(Athlete_clean,  aes(x = Date, y = `Force Plate Drop and Stick 50cm Peak Force`)) +
    geom_point() +
    geom_line() +
    labs(title = "Drop and Stick Peak Force",
         x = "Date",
         y = "Drop and Stick PF (N/N)") +
    scale_x_date(limits = c(min(Athlete_clean$Date), current_date), date_labels = "%b %Y")+
    scale_y_continuous(labels = function(x) round(x,2))
  
  ##Clean this data
  Athlete_clean <- na.omit(Athlete[, c("Date", "Continuous AMRAP Pull-Up")])
  ## Pull Up Comparison Plot
  plot12 <- ggplot(Athlete_clean,  aes(x = Date, y = `Continuous AMRAP Pull-Up`)) +
    geom_point() +
    geom_line() +
    labs(title = "Pull Up Reps",
         x = "Date",
         y = "Pull Ups Reps") +
    scale_x_date(limits = c(min(Athlete_clean$Date), current_date), date_labels = "%b %Y")+
    scale_y_continuous(labels = function(x) round(x,2))
  
  ##Clean this data
  Athlete_clean <- na.omit(Athlete[, c("Date", "AMRAP TEMPO Bench Pull (120BPM, 1-0-1-0, to block on chest)")])
  ## Bench Pull Comparison Plot
  plot13 <- ggplot(Athlete_clean,  aes(x = Date, y =  `AMRAP TEMPO Bench Pull (120BPM, 1-0-1-0, to block on chest)`)) +
    geom_point() +
    geom_line() +
    labs(title = "Bench Pull Reps",
         x = "Date",
         y = "Bench Pull Reps") +
    scale_x_date(limits = c(min(Athlete_clean$Date), current_date), date_labels = "%b %Y")+
    scale_y_continuous(labels = function(x) round(x,2))
  
  ##Clean this data
  Athlete_clean <- na.omit(Athlete[, c("Date", "AMRAP TEMPO Bench Press (120BPM, 1-0-1-0, to block on chest)")])
  ## Bench Press Comparison Plot
  plot14 <- ggplot(Athlete_clean,  aes(x = Date, y =  `AMRAP TEMPO Bench Press (120BPM, 1-0-1-0, to block on chest)`)) +
    geom_point() +
    geom_line() +
    labs(title = "Bench Press Reps",
         x = "Date",
         y = "Bench Press Reps") +
    scale_x_date(limits = c(min(Athlete_clean$Date), current_date), date_labels = "%b %Y")+
    scale_y_continuous(labels = function(x) round(x,2))
  
  
  ##Clean this data
  Athlete_clean <- na.omit(Athlete[, c("Date", "Seated Med Ball Throw(m)")])
  ## Med Ball Throw Comparison Plot
  plot15 <- ggplot(Athlete_clean,  aes(x = Date, y = "Seated Med Ball Throw(m)")) +
    geom_point() +
    geom_line() +
    labs(title = "Med Ball Throw Distance",
         x = "Date",
         y = "Distance (m)") +
    scale_x_date(limits = c(min(Athlete_clean$Date), current_date), date_labels = "%b %Y")+
    scale_y_continuous(labels = function(x) round(x,2))
  
  
  
  
  comparison <-  gridExtra::grid.arrange(nameandinfo, plot8, plot9, plot10, plot11, plot12, plot13, plot14, nrow = 4)
 ##tagging these out so you can amalgamate the documents as needed on your end.
 # filename <- paste0("comparison", i, ".pdf")
 # ggsave(filename, comparison, width = 15, height = 6, dpi = 300)
  
  
}



#### Add legend 
#### Add written description with test details and interpretation - you can combine the plot with the text grob to make one grob**
#### Add lines around SWC for Athlete results over time.
#### combine both sets of graphs into one doc.
