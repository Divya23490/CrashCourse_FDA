# Load necessary libraries
library(tidyr)
library(dplyr)
library(fda)
library(fda.usc)
library(readxl)
library(e1071)
library(caret)

# Load the temperature data
data_base <- read.csv("/Users/divyakhurana/Downloads/city_temperature.csv")

# Load the Koppen Climate Classification data
climateZones <- read_excel("/Users/divyakhurana/Desktop/fda_crashcourse/Koppen Climate Classification.xlsx", 
                           col_types = c("text", "text", "text", "text"))  # Adjust as needed

# Select only Country and ClimateZone from climate data
climateZones <- climateZones %>% 
  select(Country = 1, Main = 2)  # Adjust these names according to actual Excel columns

# Data cleanup for temperature data
data <- data_base %>%
  mutate(Month = month.abb[Month],  # Convert month numbers to abbreviations
         date = paste(Month, Day, sep = "/")) %>%
  filter(AvgTemperature != -99, Day != 0, date != "Feb/29", Year > 1994) %>%
  select(-Year)  # Remove Year if it's not used for further analysis

# Convert temperatures to Celsius
data <- data %>%
  mutate(AvgTemperature = (AvgTemperature - 32) * 5/9)

# Calculate mean daily temperature by Country and date
data_with_mean <- data %>%
  group_by(Country, date) %>%
  summarize(Temperature = mean(AvgTemperature, na.rm = TRUE), .groups = 'drop')

# Spread the data to wide format with dates as columns
data_wide <- data_with_mean %>%
  pivot_wider(names_from = date, values_from = Temperature)

# Join ClimateZone data
data_wide <- data_wide %>%
  left_join(climateZones, by = "Country")  # Ensure the key column 'Country' matches in both data frames

# Convert data to matrix and ensure numeric type
temp_matrix <- as.matrix(data_wide[,-c(1,367)])

##################################################################################

# Create the fdata object
temp_fdata <- fdata(temp_matrix, argvals = 1:365, rangeval=c(1,365), list(main = "Temperature Data", xlab = "Day", ylab = "Temperature"))
rownames(temp_matrix) <- data_wide$Country
plot(temp_fdata,labels= TRUE)

# Create functional data object
nbasis <- 4
basis <- create.fourier.basis(c(1, 365), nbasis = nbasis)

# Smooth the data to create functional data object
temp_fd <- smooth.basis(1:365, t(temp_matrix), basis)$fd
plot(temp_fd)


