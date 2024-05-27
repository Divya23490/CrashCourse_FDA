# Load necessary libraries
library(tidyr)
library(dplyr)
library(fda)
library(fda.usc)
library(readxl)
library(e1071)
library(caret)

# Load and clean temperature data
data_base <- read.csv("/Users/divyakhurana/Downloads/city_temperature.csv")
data <- data_base %>%
  mutate(Month = month.abb[Month], 
         date = paste(Month, Day, sep = "/"),
         AvgTemperature = (AvgTemperature - 32) * 5/9) %>%
  filter(AvgTemperature > -99, Day != 0, date != "Feb/29", Year > 1994) %>%
  select(-Year)

# Load and prepare climate zone data
climateZones <- read_excel("/Users/divyakhurana/Desktop/fda_crashcourse/Koppen Climate Classification.xlsx", 
                           col_types = c("text", "text", "text", "text"))
climateZones <- climateZones %>%
  select(Country = 1, ClimateZone = 2)

# Join temperature data with climate zones
data <- data %>%
  group_by(Country, date) %>%
  summarize(Temperature = mean(AvgTemperature, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = date, values_from = Temperature) %>%
  left_join(climateZones, by = "Country")

# Ensure numeric conversion for temperature data (excluding country and climate zone)
data_numeric <- data %>%
  select(-Country, -ClimateZone) %>%
  mutate(across(everything(), as.numeric))

# Convert to matrix and remove any NA/Inf/NaN values
temp_matrix <- as.matrix(data_numeric)
temp_matrix[!is.finite(temp_matrix)] <- NA  # Replace non-finite values with NA

# Create the fdata object
argvals <- 1:ncol(temp_matrix)
temp_fdata <- fdata(temp_matrix, argvals, rangeval = c(1, 365))

# Plotting the fdata object to check
plot(temp_fdata, main = "Temperature Data", xlab = "Day of the year", ylab = "Temperature (C)")

# Smoothing the data
nbasis <- 20  # Increasing basis functions for better smoothing
basis <- create.fourier.basis(rangeval = c(1, 365), nbasis = nbasis)
temp_fd <- smooth.basis(argvals, t(temp_matrix), basis)$fd

# Plot the smoothed data
plot(temp_fd, main = "Smoothed Temperature Data", xlab = "Day", ylab = "Temperature (C)")

