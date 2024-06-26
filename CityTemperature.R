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
data_base <- read.csv("C:\\Users\\jzd767\\OneDrive - University of Copenhagen\\Heba Basha\\Courses\\FDA and ML of Complex Data\\project\\city_temperature.csv")
climateZones <- read_excel("C:\\Users\\jzd767\\OneDrive - University of Copenhagen\\Heba Basha\\Courses\\FDA and ML of Complex Data\\project\\Koppen Climate Classification.xlsx")
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

# print the first four eigenvalues and the proportion of variance explained
print(fpcaRes$values[1:6])


#print the proportion of variance explained by the first four eigenfunctions
print(fpcaRes$varprop[1:6])

############################################
#Second Method
# Support Vector Machine classification

# Ensure the response variable for climate zones is correctly factorized

response <- factor(data_wide$ClimateZones)

# Create a data list as required by the classification functions
data_list <- list("df" = data.frame(response), "temp_fd" = temp_fd)

# SVM Classification using functional data
svm_model <- classif.svm(formula = response ~ temp_fd, data = data_list, fdataobj = temp_fd, type = "C-classification", kernel = "linear")
summary(svm_model)


############################
# Third Method 
# Random Forest Classification
rf_model <- classif.randomForest(formula = response ~ temp_fd, data = data_list, fdataobj = temp_fd)
summary(rf_model)

##########################
# Forth Method 
#### Linear Regression 
##### FANOVA model

FANOVA <- fRegress(temp_fd ~ response, temp_fd)
plot(FANOVA$betaestlist$const) # the constant for the continental
plot(FANOVA$betaestlist$response.Dry)
plot(FANOVA$betaestlist$response.Temperate)
plot(FANOVA$betaestlist$response.Tropical)

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

##########################
# Fifth Method









####### The following is for you Divya ######################
#### I didn't do anything here

# K-Nearest Neighbors Classification
knn_model <- classif.knn(response ~ temp_fd, data = data_list, fdataobj = temp_fd, group = response)

# Cross-validation and model comparison using caret
train_control <- trainControl(method = "cv", number = 10)

# For caret, we need to use the coefficient matrix
svm_cv <- train(x = as.data.frame(temp_fd$coefs), y = response, method = "svmLinear", trControl = train_control)
rf_cv <- train(x = as.data.frame(temp_fd$coefs), y = response, method = "rf", trControl = train_control)
#knn_cv <- train(x = as.data.frame(temp_fd$coefs), y = response, method = "knn", trControl = train_control)

# Collecting results for comparison
results <- resamples(list(SVM = svm_cv, RF = rf_cv, KNN = knn_cv))

# Summarize the results
summary(results)

# Visualization of model performance
bwplot(results)