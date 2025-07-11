library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)
library(GGally)
library(table1)

data <- read.csv("household_power_consumption.csv", stringsAsFactors = FALSE)
str(data)
summary(data)

data$Date <- dmy(data$Date)
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")

# Sampling a week from each month
sampled_data <- data %>%
  group_by(Year = year(Date), Month = month(Date)) %>%
  filter(row_number() <= 7) %>%
  ungroup()

#Numeric columns
numeric_cols <- c("Global_active_power", "Global_reactive_power", 
                  "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

for (col in numeric_cols) {
  sampled_data[[col]] <- as.numeric(sampled_data[[col]])
}

# Handling missing values
for (col in numeric_cols) {
  sampled_data[[col]][is.na(sampled_data[[col]])] <- median(sampled_data[[col]], na.rm = TRUE)
}

# Scaling and Normalizing the data for clustering
clustering_data <- sampled_data %>%
  select(Global_active_power, Global_intensity, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3)
clustering_data_scaled <- scale(clustering_data)

# Elbow method to determine optimal k
wss <- function(k) {
  kmeans(clustering_data_scaled, centers = k, nstart = 10)$tot.withinss
}
k_values <- 1:10
wss_values <- sapply(k_values, wss)

# Elbow method
ggplot() +
  geom_line(aes(x = k_values, y = wss_values)) +
  geom_point(aes(x = k_values, y = wss_values)) +
  labs(title = "Elbow Method for Optimal k", x = "Number of Clusters (k)", y = "Total Within-Cluster Sum of Squares") +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA)   
  )

# K-means clustering
set.seed(123)
kmeans_result1 <- kmeans(clustering_data_scaled, centers = 5, nstart = 10)
kmeans_result2 <- kmeans(clustering_data_scaled, centers = 6, nstart = 10)

# Add cluster labels to the original sampled data
sampled_data$Cluster1 <- as.factor(kmeans_result1$cluster)
sampled_data$Cluster2 <- as.factor(kmeans_result2$cluster)

#Result 1
(BSS <- kmeans_result1$betweenss)
(TSS <- kmeans_result1$totss)

# Quality of the partition
BSS / TSS * 100

#Result 2
(BSS <- kmeans_result2$betweenss)
(TSS <- kmeans_result2$totss)

# Quality of the partition
BSS / TSS * 100

# Scatter plot of cluster1 using Global_active_power and Voltage
ggplot(sampled_data, aes(x = Global_active_power, y = Voltage, color = Cluster1)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(title = "Scatter Plot of Clusters", x = "Global Active Power", y = "Voltage") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA)   
  ) +
  scale_color_brewer(palette = "Set1")

# Scatter plot of cluster2 using Global_active_power and Voltage
ggplot(sampled_data, aes(x = Global_active_power, y = Voltage, color = Cluster2)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(title = "Scatter Plot of Clusters", x = "Global Active Power", y = "Voltage") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA)   
  ) +
  scale_color_brewer(palette = "Set1")

# Boxplot of variables by Cluster1
data_long <- sampled_data %>%
  pivot_longer(cols = c(Global_active_power, Global_intensity, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3), 
               names_to = "Variable", values_to = "Value")

ggplot(data_long, aes(x = Cluster1, y = Value, fill = Cluster1)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Boxplots of Variables by Cluster1", y = "Value", x = "Cluster") +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA)   
  )

# Boxplot of variables by Cluster2
ggplot(data_long, aes(x = Cluster2, y = Value, fill = Cluster2)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Boxplots of Variables by Cluster2", y = "Value", x = "Cluster") +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background = element_rect(fill = "white", colour = NA)   
  )
