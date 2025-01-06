# Research Question
# Is there a correlation between temperature and the air quality (PM2.5 levels) 
# in different global locations?

# Hypotheses
# Null Hypothesis (H0): As there is no statistical significance between 
# temperature and air quality (PM2.5 levels), these variables are not correlated.
# Alternative Hypothesis (H1): As there exists a statistical significance between 
# temperature and air quality (PM2.5 levels), these variables are correlated.

# Selected Hypothesis Tests
# Test 1: Pearson correlation
# Test 2: Chi-Square Test of Independence

# Loading necessary libraries
library(ggplot2)
library(dplyr)
library(forecast)
library(zoo)
library(lubridate)
library(tseries)
library(GGally)
library(pheatmap)

# Loading the data
file_path <- "~/Desktop/TEAM_RESEARCH/TM/air-quality/GlobalWeatherRepository.csv"
data <- read.csv(file_path)

# Previewing the dataset
head(data)
str(data)
summary(data)


# Checking for missing values
sum(is.na(data))

# Handling missing values (e.g., remove or impute)
data <- na.omit(data) # Removing rows with missing values

# Checking for duplicates
duplicates <- data[duplicated(data), ]
print(duplicates)

# Removing duplicates
data <- data[!duplicated(data), ]

# Checking data types and convert if necessary
str(data)

data$humidity <- as.numeric(data$humidity)
data$temperature_celsius <- as.numeric(data$temperature_celsius)

# Histogram for temperature in Celsius
ggplot(data, aes(x = temperature_celsius)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Temperature Distribution (Celsius)", x = "Temperature (°C)", y = "Frequency")

# Histogram for temperature in Fahrenheit
ggplot(data, aes(x = temperature_fahrenheit)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Temperature Distribution (Fahrenheit)", x = "Temperature (°F)", y = "Frequency")

# Boxplot for temperature by country
ggplot(data, aes(x = country, y = temperature_celsius, fill = country)) +
  geom_boxplot() +
  labs(title = "Temperature by Country", x = "Country", y = "Temperature (°C)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot for temperature vs. humidity
ggplot(data, aes(x = humidity, y = temperature_celsius)) +
  geom_point(color = "red") +
  labs(title = "Temperature vs. Humidity", x = "Humidity (%)", y = "Temperature (°C)")

# Performing visual inspection as well as hypothesis testing (Shapiro-Wilk test) 
# to answer the research question
## Visual inspection
ggplot(data, aes(x = temperature_celsius, y = air_quality_us.epa.index)) +
  geom_point() +
  labs(title = "Scatter Plot of Temperature vs air_quality_us-epa-index",
       x = "Temperature (°C)", y = "air_quality_us-epa-index") +
  theme_minimal()

## Test 1: Pearson correlation
correlation_test <- cor.test(
  data$temperature_celsius, 
  data$air_quality_us.epa.index, 
  method = "pearson"
  )

cat("Pearson Correlation Coefficient:", correlation_test$estimate, "\n")
cat("p-value:", correlation_test$p.value, "\n")

if (correlation_test$p.value < 0.05) {
  cat("As there exists a statistical significance between temperature and air_quality_us-epa-index,\nthese variables are correlated.\n")
} else {
  cat("As there is no statistical significance between temperature and air_quality_us-epa-index,\nthese variables are not correlated.\n")
}

## Test 2: Chi-Square Test of Independence
### Data Preparation
data$temperature_category <- cut(
  data$temperature_celsius, 
  breaks = 3, 
  labels = c("Low", "Medium", "High")
) #categorizing the "temperature_celsius" column

data$pm_category <- cut(
  data$air_quality_us.epa.index, 
  breaks = 3, 
  labels = c("Low", "Medium", "High")
) #categorizing the "air_quality_us-epa-index" column

contingency_table <- table(data$temperature_category, data$pm_category)
print(contingency_table)

### Performing the Chi-Square Test
chi_square_test <- chisq.test(contingency_table)

cat("Chi-Square Test Statistic:", chi_square_test$statistic, "\n")
cat("p-value:", chi_square_test$p.value, "\n")

if (chi_square_test$p.value < 0.05) {
  cat("As there exists a statistical significance between temperature categories and air_quality_us-epa-index,\nthese variables are dependent (i.e., correlated).\n")
} else {
  cat("As there is no statistical significance between temperature categories and air_quality_us-epa-index,\nthese variables are independent (i.e., not-correlated).\n")
}

# Performing ADF test on temperature_celsius (to check for stationarity in the time series data)
adf_result_temp <- adf.test(data$temperature_celsius, alternative = "stationary")
print(adf_result_temp)

# Performing ADF test on humidity (for stationarity)
adf_result_humidity <- adf.test(data$humidity, alternative = "stationary")
print(adf_result_humidity)

# Selecting variables for pair plot
weather_vars <- data %>% select(temperature_celsius, humidity, wind_mph, pressure_mb, visibility_km)
ggpairs(weather_vars)

# Analyzing summary statistics for key weather variables
summary(data$temperature_celsius)
summary(data$humidity)
summary(data$wind_mph)
summary(data$visibility_km)

# Creating a heatmap for air quality parameters
air_quality_data <- data %>% select(air_quality_Carbon_Monoxide, air_quality_Ozone, air_quality_Nitrogen_dioxide, air_quality_Sulphur_dioxide, air_quality_PM2.5, air_quality_us.epa.index)
correlation_air_quality <- cor(air_quality_data, use = "complete.obs")

# Creating heatmap for air quality correlations
pheatmap(correlation_air_quality, cluster_rows = TRUE, cluster_cols = TRUE, main = "Air Quality Correlation Heatmap")

# Converting 'last_updated' to a POSIXct date-time format
data$last_updated <- mdy_hms(data$last_updated)  # Adjust format if necessary
data$last_updated <- as.POSIXct(data$last_updated, format="%m/%d/%Y %H:%M", tz="UTC")

# Converting 'temperature_celsius' to a time series (daily data assumption)
temperature_ts <- ts(data$temperature_celsius, frequency=1, start=c(2024, 1))
print(data$temperature_celsius)

# Fitting ARIMA model to the time series data
model <- auto.arima(temperature_ts)

# Forecasting the next 7 days
forecasted_values <- forecast(model, h=365)

# Plotting the forecasted values
autoplot(forecasted_values) + 
  theme_minimal() + 
  theme(plot.margin = margin(10, 10, 10, 10)) +
  ggtitle("Temperature Forecast") + 
  theme(plot.title = element_text(size = 10))

