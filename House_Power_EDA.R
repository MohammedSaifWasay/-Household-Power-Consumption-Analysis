# Load necessary libraries
install.packages("GGally")

library(tidyverse)
library(ggplot2)
library(lubridate)
library(reshape2)
library(GGally)
library(table1)
options(scipen = 999)

# Load the dataset
data <- read_csv("household_power_consumption.csv")

# Overview of the dataset
str(data)
summary(data)

# Convert necessary columns to numeric
numeric_cols <- c("Global_active_power", "Global_reactive_power", 
                  "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
for (col in numeric_cols) {
  data[[col]] <- as.numeric(data[[col]])
}
# Convert Date and Time columns to a single datetime column
data$DateTime <- dmy(data$Date) + hms(data$Time)

summary(data)

# Check for missing values and visualize them
missing_values <- colSums(is.na(data))
print(missing_values)

# Visualization of Missing Values
install.packages("naniar")

library(naniar)
gg_miss_var(data) +
  labs(title = "Missing Values in Each Column", y = "Number of Missing Values", x = "Columns")

# Impute missing values with the median of each column
for (col in numeric_cols) {
  data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
}

#Descriptive Statistics
render.median.IQR <- function(x, ...) {
  c('', 
    'Mean (SD)' = sprintf("%s (%s)", round(mean(x), 2), round(sd(x), 2)),
    'Median [IQR]' = sprintf("%s [%s, %s]", median(x), 
                             quantile(x, 0.25), quantile(x, 0.75)))
}
table1(~ Global_active_power + Global_reactive_power + Voltage + Global_intensity + 
         Sub_metering_1 + Sub_metering_2 + Sub_metering_3,data = data, render = render.median.IQR)

# Boxplot to identify outliers
data_before <- data %>%
  pivot_longer(cols = c(Voltage, Global_active_power, Global_reactive_power, Global_intensity),
               names_to = "Variable", values_to = "Value")
ggplot(data_before, aes(y = Value, fill = Variable)) +
  geom_boxplot(color = "black") +
  facet_wrap(~ Variable, scales = "free") + 
  labs(title = "Boxplots of Various Variables before handling outliers", y = "Value") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA)   
  )

# Capping outliers at 95th percentile
cap_outliers <- function(x) {
  q <- quantile(x, probs = c(0.05, 0.95))
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}

data$Voltage <- cap_outliers(data$Voltage)
data$Global_active_power <- cap_outliers(data$Global_active_power)
data$Global_reactive_power <- cap_outliers(data$Global_reactive_power)
data$Global_intensity <- cap_outliers(data$Global_intensity)

#Boxplot after capping
data_after <- data %>%
  pivot_longer(cols = c(Voltage, Global_active_power, Global_reactive_power, Global_intensity),
               names_to = "Variable", values_to = "Value")
ggplot(data_after, aes(y = Value, fill = Variable)) +
  geom_boxplot(color = "black") +
  facet_wrap(~ Variable, scales = "free") +  
  labs(title = "Boxplots of Various Variables after handling outliers", y = "Value") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA)   
  )

# Visualizations

data$Hour <- hour(data$DateTime)

# Aggregate Global Active Power by Hour
peak_usage <- data %>%
  group_by(Hour) %>%
  summarise(Global_active_power = mean(Global_active_power, na.rm = TRUE))

# Plotting peak and off-peak electricity usage
ggplot(peak_usage, aes(x = Hour, y = Global_active_power)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Patterns of Peak and Off-Peak Electricity Usage",
       x = "Hour of the Day",
       y = "Average Global Active Power (kW)") +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA)   
  )

# Scatter plot of Global Active Power vs Voltage
ggplot(data, aes(x = Voltage, y = Global_active_power)) +
  geom_point(alpha = 0.2, color = "darkgreen") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Relationship Between Global Active Power and Voltage",
       x = "Voltage (V)",
       y = "Global Active Power (kW)") +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA)   
  )

# Scatter plot of Global Intensity vs Global Active Power
ggplot(data, aes(x = Global_intensity, y = Global_active_power)) +
  geom_point(alpha = 0.2, color = "darkred") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Relationship Between Global Intensity and Global Active Power",
       x = "Global Intensity (A)",
       y = "Global Active Power (kW)") +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA)   
  )

# Scatter plot of Global Intensity vs Voltage
ggplot(data, aes(x = Global_intensity, y = Voltage)) +
  geom_point(alpha = 0.2, color = "pink") +
  geom_smooth(method = "lm", color = "purple", se = FALSE) +
  labs(title = "Relationship Between Global Intensity and Voltage",
       x = "Global Intensity (A)",
       y = "Voltage (V)") +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA)   
  )

submeter_usage <- data %>%
  group_by(Hour) %>%
  summarise(Sub_metering_1 = mean(Sub_metering_1, na.rm = TRUE),
            Sub_metering_2 = mean(Sub_metering_2, na.rm = TRUE),
            Sub_metering_3 = mean(Sub_metering_3, na.rm = TRUE))


submeter_usage_melt <- melt(submeter_usage, id.vars = "Hour", 
                            variable.name = "Sub_metering", 
                            value.name = "Average_Consumption")

# Plot Sub-Metering Data
ggplot(submeter_usage_melt, aes(x = Hour, y = Average_Consumption, color = Sub_metering)) +
  geom_line(size = 1) +
  labs(title = "Average Electricity Consumption by Sub-Metering",
       x = "Hour of the Day",
       y = "Average Consumption (Watt-hours)") +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA)   
  )
