# Load necessary libraries
library(tidyr)
library(dplyr)
library(fda)
library(fda.usc)
library(fds)
library(e1071)
library(caret)
library(readxl)

# Load the data
data_base <- read.csv("/Users/divyakhurana/Downloads/city_temperature.csv")
climateZones <- read_excel("/Users/divyakhurana/Desktop/fda_crashcourse/Koppen Climate Classification.xlsx")
climateZones <- climateZones[climateZones$Country != "Iceland", ] # Remove Iceland

#if we want to remove the countries that are in the Southern Hemisphere

southern_hemisphere_countries <- 
  c("Argentina", "Australia", "Bolivia", "Brazil", "Ecuador", "Madagascar",
    "Malawi", "Mozambique", "Namibia", "New Zealand", "Peru","South Africa",
    "Tanzania", "Uruguay", "Zambia")

climateZones <- climateZones[!climateZones$Country %in% southern_hemisphere_countries, ]

# Data cleanup
data <- data_base
data$Month <- month.abb[data$Month]
data$date <- paste(data$Month, data$Day, sep = "/")
data <- data[data$AvgTemperature != -99, ] # Remove missing values
data <- data[data$Day != 0, ] # Remove invalid days
data <- data[data$date != "Feb/29", ] # Remove February 29
data <- data[data$Year > 1994, ] # Remove outlier years
data <- data[!data$Country %in% southern_hemisphere_countries, ]


## I think we need to drop the country "Burundi" because it is missing 12 days 
data <- data[data$Country != "Burundi", ] # Remove Burundi
data <- data[data$Country != "Iceland", ] # Remove Iceland since it is alone in polar group

# Convert temperatures to Celsius
data$AvgTemperature <- (data$AvgTemperature - 32) * 5/9

# Calculate mean daily temperature over the country over the years 
data_with_mean <- data %>%
  group_by(Country, Region, Month, Day, date) %>%
  summarize(Temperature = mean(AvgTemperature, na.rm = TRUE), .groups = 'drop')

# to sort the data based on date
data_with_mean$date_sort <- as.Date(data_with_mean$date, format="%b/%d")
data_with_mean <- data_with_mean[order(data_with_mean$date_sort), ]
######
data_long <- data_with_mean[, !names(data_with_mean) %in% c("AvgTemperature","AvgTemperature_C","City", "State", "Day","Year","Month","date_sort")]


# Spread the data to wide format
data_wide <- data_long %>%
  pivot_wider(names_from = date, values_from = Temperature)


######
data_wide <- cbind(climateZones$`Climate zone`, data_wide)
names(data_wide)[1] <- "ClimateZones"

# Convert data to matrix and ensure numeric type
temp_matrix <- as.matrix(data_wide[,-c(1, 2 , 3)])
rownames(temp_matrix) <- data_wide$Country
##################################################################################
days = 1:365

# Create the fdata object
temp_fdata <- fdata(temp_matrix, argvals = days, rangeval=range(days), list(main = "Temperature Data", xlab = "Day", ylab = "Temperature"))
plot(temp_fdata,labels= TRUE)

# Create functional data object
nbasis <- 8
basis <- create.fourier.basis(c(1, 365), nbasis = nbasis)

# Smooth the data to create functional data object
temp_fd <- smooth.basis(days, t(temp_matrix), basis)$fd
plot(temp_fd)

temp_fd$ClimateZone = data_wide$ClimateZones
#### plotting unsmoothed vs smoothed data

par(mfrow = c(1, 2), oma = c(0, 0, 2, 0)) 
# plotting the unsmoothed data 

# Define a set of colors
climate_colors <- c("Continental" = "red", "Dry" = "blue", "Temperate" = "green", "Tropical" = "orange")

# Create a vector of colors for each country based on its climate zone
country_colors <- climate_colors[data_wide$ClimateZones]

# Plot the temperature curves using matplot
matplot(days, t(temp_matrix), type = "l", col = country_colors, lty = 1, lwd = 1,
        main = "Unsmoothed",
        xlab = "Day", ylab = "Temperature (°C)")

legend("bottom", legend = names(climate_colors), col = climate_colors, lty = 1, lwd = 2, cex = 0.8)
# plotting smoothed data

temp_fd_eval <- eval.fd(days, temp_fd)
matplot(days, temp_fd_eval, type = "l", col = country_colors, lty = 1, lwd = 1,
        main = "Smoothed",
        xlab = "Day", ylab = "Temperature (°C)")
legend("bottom", legend = names(climate_colors), col = climate_colors, lty = 1, lwd = 2, cex = 0.8)
mtext("Temperature Data: Unsmoothed and Smoothed", outer = TRUE, cex = 1.5)

par(mfrow = c(1, 1))

#######################################################################################
# from here we start the analysis.
# First Method:
#########
# Functional Principle Component #
# Calculate the mean curve
temp_eval <- eval.fd(days, temp_fd)
meanCurve <- apply(temp_eval, 1, mean)
# plotting the data + the mean curve
plot(days, meanCurve, type = "l", lwd=5,ylim=c(-30, 45),
     main = "Mean Temperature Curve", xlab = "Days", ylab = "Mean Temperature")
for(i in 1:109) {
  lines(days, eval.fd(days, temp_fd[i,]), col=i)
}

# calculating the deviations from the mean and the centered data
dev= eval.fd(days, temp_fd) - meanCurve

centeredFd <- smooth.basis(days, dev, basis)
# plotting the deviation from the mean 
plot(days, rep(0, length(days)), type='n', ylim=c(-30, 45),
     xlab="Days", ylab="Temperature", main="Deviations from the mean")
for(i in 1:109) {
  lines(days, matrix(dev[, i], nrow = 365, ncol = 1), col= i)
}
abline(h=0, lwd = 3,lty=2)

# Perform Functional Principal Component Analysis

nharm <- 6  # Number of harmonics to compute
fpcaRes <- pca.fd(centeredFd$fd, nharm)

# Extract the harmonics (eigenfunctions)
eigenfunction_fd_list <- fpcaRes$harmonics

# Plot each eigenfunction in a separate frame
par(mfrow = c(2, 3))
for(i in 1:6) {
  # Construct the functional data object for each principal component using its coefficients
  eigen_fd  <- fd(eigenfunction_fd_list$coefs[, i], basis)
  
  
  # Plot the eigenfunction
  plot(eigen_fd[i], xlab="Day of the Year", ylab=sprintf("PC%d", i), xlim=c(0, 365), ylim=c(-0.1, 0.1), lwd=2)
  lines(eigen_fd, col=2, lwd=2, main=plot_title)
  # add this arguments to the plot function becasue it caused and error (main=sprintf("Eigenfunction %d (%.3f%% of variance)", i), type='l',)
  # Add a horizontal dotted line at y=0, in black
  abline(h=0, lty=2)  # Horizontal line, 'lty=2' for dotted
}
par(mfrow = c(1, 1))
# print the first four eigenvalues and the proportion of variance explained
print(fpcaRes$values[1:6])

#print the proportion of variance explained by the first four eigenfunctions
print(fpcaRes$varprop[1:6])

############################################
# Ensure the response variable for climate zones is correctly factorized

response <- factor(data_wide$ClimateZones)

# Create a data list as required by the classification functions
data_list <- list("df" = data.frame(response), "temp_fd" = temp_fd)
############################################
#### Second Method
#### Linear Regression 
##### FANOVA model

FANOVA <- fRegress(temp_fd ~ response, temp_fd)
par(mfrow = c(2, 2))
plot(FANOVA$betaestlist$const, xlab = "Day", ylab = "Temperature (°C)",main = "Continental",lwd = 3 ) # the constant for the continental
plot(FANOVA$betaestlist$response.Dry, xlab = "Day", ylab = "Temperature (°C)",main = "Dry" ,lwd = 3 )
plot(FANOVA$betaestlist$response.Temperate, xlab = "Day", ylab = "Temperature (°C)",main = "Temperate" ,lwd = 3 )
plot(FANOVA$betaestlist$response.Tropical, xlab = "Day", ylab = "Temperature (°C)",main = "Tropical" ,lwd = 3 )
par(mfrow = c(1, 1))

# estimated mean of the four regions:
Continental_mean = eval.fd(evalarg = days,fdobj=FANOVA$betaestlist$const$fd)
Dry_mean = Continental_mean + eval.fd(evalarg = days,fdobj=FANOVA$betaestlist$response.Dry$fd)
Temperate_mean = Continental_mean + eval.fd(evalarg = days,fdobj=FANOVA$betaestlist$response.Temperate$fd)
Tropical_mean = Continental_mean + eval.fd(evalarg = days,fdobj=FANOVA$betaestlist$response.Tropical$fd)

response_means = rbind(t(Continental_mean),t(Dry_mean),t(Temperate_mean),t(Tropical_mean))
temp_eval = eval.fd(days,temp_fd)
regions = response

matplot((temp_eval),col=factor(regions),type='l',lty=1,lwd=0.5)
matlines(t(response_means),type='l',lwd=2,lty=1)
############################
# Third Method 
# Support Vector Machine classification

# SVM Classification using functional data
svm_model <- classif.svm(formula = response ~ temp_fd, data = data_list, fdataobj = temp_fd, type = "C-classification", kernel = "linear")
summary(svm_model)

##########################
# Forth Method 
# Random Forest Classification
rf_model <- classif.randomForest(formula = response ~ temp_fd, data = data_list, fdataobj = temp_fd)
summary(rf_model)
##########################
# Fifth Method

# K-Nearest Neighbors Classification
knn_result <- classif.knn(fdataobj = temp_fdata, group = response, kmax = 15)
summary(knn_result)

################################
# Sixth Method
# Artificial Neural Network (ANN) classification using functional data

ann_model <- classif.nnet(response ~ temp_fd, data = data_list, size = 5, rang = 0.1, decay = 5e-4, maxit = 200)
summary(ann_model)

#######################

# Cross-validation and model comparison using caret
train_control <- trainControl(method = "cv", number = 10)

# For caret, we need to use the coefficient matrix
svm_cv <- train(x = as.data.frame(t(temp_fd$coefs)), y = response, method = "svmLinear", trControl = train_control)
rf_cv <- train(x = as.data.frame(t(temp_fd$coefs)), y = response, method = "rf", trControl = train_control)
knn_cv <- train(x = as.data.frame(t(temp_fd$coefs)), y = response, method = "knn", trControl = train_control)

# Collecting results for comparison
results <- resamples(list(SVM = svm_cv, RF = rf_cv, KNN = knn_cv))

# Summarize the results
summary(results)

# Visualization of model performance
bwplot(results)

############################

# Check dimensions
print(dim(temp_fd$coefs))
print(length(response))

# Align the dimensions of the predictors and response
n_samples <- ncol(temp_fd$coefs)
response <- response[1:n_samples]

# Ensure the number of rows in temp_fd$coefs matches the length of response
if (length(response) != n_samples) {
  stop("Number of rows in predictors does not match the length of the response variable")
}

# Prepare data for caret
predictors <- as.data.frame(t(temp_fd$coefs))

# Cross-validation and model comparison using caret
train_control <- trainControl(method = "cv", number = 10)

# SVM Cross-validation using caret
svm_cv <- train(x = predictors, y = response, method = "svmLinear", trControl = train_control)

# Random Forest Cross-validation using caret
rf_cv <- train(x = predictors, y = response, method = "rf", trControl = train_control)

# ANN Cross-validation using caret
ann_cv <- train(x = predictors, y = response, method = "nnet", trControl = train_control, linout = FALSE, trace = FALSE)

# Collecting results for comparison
results <- resamples(list(SVM = svm_cv, RF = rf_cv, ANN = ann_cv))

# Summarize the results
summary(results)

# Visualization of model performance
bwplot(results)
