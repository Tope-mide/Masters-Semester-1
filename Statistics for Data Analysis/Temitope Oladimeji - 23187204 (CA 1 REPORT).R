#Relevant packages
install.packages(c("ggplot2", "ggthemes", "GGally", "scales")) #For EDAs
install.packages("dplyr") #For data manipulation and wrangling
install.packages(c("lmtest", "car")) #For performing diagnostics and hypothesis tests on linear models
install.packages("data.table") #Handles large datasets with high performance
install.packages(c("Amelia", "mice")) #Handles missing data
install.packages("corrplot") #For visualizing correlation matrices.
install.packages(c("Metrics", "MLmetrics")) #Offers collection of evaluation metrics used in statistical analysis
install.packages("ISLR")
install.packages("performance")

#Libraries
library(Amelia)
library(Metrics)
library(ggplot2)
library(car)
library(ISLR)
library(corrplot)
library(lmtest)
library(performance)
library(caret) #For cross-validation


#DATA PRE-PROCESSING
#Import the dataset
setwd("C:\\Users\\topeo\\OneDrive\\Desktop\\Masters\\NCI\\Stats for Data Analytics") #Set the working directory

#Load the dataset
housing_data <- read.csv("housing.csv", header = TRUE, na.string=c(""), stringsAsFactors = T)

#Visualization of distributions in the variables (EDA)
#Histogram to visualize numerical variables
#FIGURE 1 IN REPORT
par(mfrow = c(2, 2)) #2x2 grid 
hist(housing_data$Lot_Frontage, main = "Histogram of Lot Frontage", xlab = "Lot Frontage")
hist(housing_data$Lot_Area, main = "Histogram of Lot Area", xlab = "Lot Area")
hist(housing_data$Year_Built, main = "Histogram of Year Built", xlab = "Year Built")
hist(housing_data$Total_Bsmt_SF, main = "Histogram of Basement Area", xlab = "Basement Area")
par(mfrow = c(1,1)) #Reset layout

par(mfrow = c(2, 2))  
hist(housing_data$First_Flr_SF, main = "Histogram of Ground Floor Area", xlab = "Ground Floor Area")
hist(housing_data$Second_Flr_SF, main = "Histogram of First Floor Area", xlab = "First Floor Area")
hist(housing_data$Full_Bath, main = "Histogram of Full Bathrooms", xlab = "Full Bathrooms")
hist(housing_data$Half_Bath, main = "Histogram of Half Bathrooms", xlab = "Half Bathrooms")
par(mfrow = c(1,1))

par(mfrow = c(2, 2))  
hist(housing_data$Bedroom_AbvGr, main = "Histogram of Bedrooms on or above Ground Floor", xlab = "Bedrooms on or above Ground Floor")
hist(housing_data$Kitchen_AbvGr, main = "Histogram of Kitchens on or above Ground Floor", xlab = "Kitchens on or above Ground Floor")
hist(housing_data$Fireplaces, main = "Histogram of Fireplaces", xlab = "Fireplaces")
hist(housing_data$Longitude, main = "Histogram of Longitude", xlab = "Longitude")
par(mfrow = c(1,1))

hist(housing_data$Latitude, main = "Histogram of Lattitude", xlab = "Lattitude")

#Barplot to visualize categorical variables
#FIGURE 2 IN REPORT
par(mfrow = c(2, 2))  
bldg_type <- table(housing_data$Bldg_Type)
barplot(bldg_type, xlab = "Dwelling Type", ylab = "Count", main = "Bar Plot: Dwelling Type")

house_style <- table(housing_data$House_Style)
barplot(house_style, xlab = "Dwelling Style", ylab = "Count", main = "Bar Plot: Dwelling Style")

overall_cond <- table(housing_data$Overall_Cond)
barplot(overall_cond, xlab = "Overall Condition of House", ylab = "Count", main = "Bar Plot: Overall Condition of House")

exter_cond <- table(housing_data$Exter_Cond)
barplot(exter_cond, xlab = "Condition of Material on Exterior of House", ylab = "Count", main = "Bar Plot: Condition of Material on Exterior of House")
par(mfrow = c(1, 1))


#Data Cleaning
#Inspect the dataset
summary(housing_data)

#Encode categorical variables
#Use OH encoding for nominal variables
nominal_variables <- c("Bldg_Type", "House_Style")
encoded_variables <- data.frame(model.matrix(~ . -1, data = housing_data[nominal_variables]))
housing_data <- cbind(housing_data, encoded_variables) #add newly created columns to dataset

#Assign numeric values to the ordinal variables
housing_data$Overall_Cond_numeric <- as.numeric(factor(housing_data$Overall_Cond, levels = c("Below_Average", "Average", "Above_Average", "Good", "Very_Good", "Excellent", "Other")))
housing_data$Exter_Cond_numeric <- as.numeric(factor(housing_data$Exter_Cond, levels = c("Poor", "Fair", "Typical", "Good", "Excellent")))

#Drop the original categorical variables
housing_data <- subset(housing_data, select = -c(Bldg_Type, House_Style, Overall_Cond, Exter_Cond))

summary(housing_data)

#Inspect missing values
missing_values <- is.na(housing_data)
col_missing_counts <- colSums(missing_values) #count number of missing values in each column, if any
col_missing_counts #display result

#Handle relevant columns with missing values
#Imputation with mean for column with missing values (preserve the data)
mean_Overall_Cond_numeric <- mean(housing_data$Overall_Cond_numeric, na.rm = TRUE) #imputation w/mean
housing_data$Overall_Cond_numeric[is.na(housing_data$Overall_Cond_numeric)] <- mean_Overall_Cond_numeric

summary(housing_data)


#Visualize distribution of numerical variables via Box Plots [Before IQR test]
#FIGURE 3 IN REPORT
par(mfrow = c(2, 2)) 
boxplot(housing_data$Lot_Frontage, main = "Box Plot of Lot Frontage")
boxplot(housing_data$Lot_Area, main = "Box Plot of Lot Area")
boxplot(housing_data$Year_Built, main = "Box Plot of Year Built")
boxplot(housing_data$Total_Bsmt_SF, main = "Box Plot of Basement Area")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
boxplot(housing_data$First_Flr_SF, main = "Box Plot of Ground floor area")
boxplot(housing_data$Second_Flr_SF, main = "Box Plot of First floor Area")
boxplot(housing_data$Full_Bath, main = "Box Plot of Full bathroom")
boxplot(housing_data$Half_Bath, main = "Box Plot of Half bathroom")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
boxplot(housing_data$Bedroom_AbvGr, main = "Box Plot of Bedroom above ground")
boxplot(housing_data$Kitchen_AbvGr, main = "Box Plot of Kitchen above ground")
boxplot(housing_data$Fireplaces, main = "Box Plot of Fireplaces")
boxplot(housing_data$Longitude, main = "Box Plot of Longitude")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
boxplot(housing_data$Latitude, main = "Box Plot of Latitude")
boxplot(housing_data$Bldg_TypeDuplex, main = "Box Plot of Building Type (Duplex)")
boxplot(housing_data$Bldg_TypeOneFam, main = "Box Plot of Building Type (One-Fam)")
boxplot(housing_data$Bldg_TypeTwnhs, main = "Box Plot of Building Type (Twin House)")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
boxplot(housing_data$Bldg_TypeTwnhsE, main = "Box Plot of Building Type (Twin HSE)")
boxplot(housing_data$Bldg_TypeTwoFmCon, main = "Box Plot of Building Type (TwoFmCon)")
boxplot(housing_data$House_StyleOne_and_Half_Unf, main = "Box Plot of House Style (1 & 1/2)")
boxplot(housing_data$House_StyleOne_Story, main = "Box Plot of House Style (One Story)")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
boxplot(housing_data$House_StyleSFoyer, main = "Box Plot of House Style (Foyer)")
boxplot(housing_data$House_StyleSLvl, main = "Box Plot of House Style (1-level)")
boxplot(housing_data$House_StyleTwo_and_Half_Fin, main = "Box Plot of House Style (2 & 1/2) Finished")
boxplot(housing_data$House_StyleTwo_and_Half_Unf, main = "Box Plot of House Style (2 & 1/2) Unfinished")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
boxplot(housing_data$House_StyleTwo_Story, main = "Box Plot of House Style (Two Story)")
boxplot(housing_data$Overall_Cond_numeric, main = "Box Plot of Overall Condition")
boxplot(housing_data$Exter_Cond_numeric, main = "Box Plot of Exterior Condition")
par(mfrow = c(1, 1))

#Identifying and removing outliers
cols <- c("Lot_Frontage", "Lot_Area", "Year_Built", 
          "Total_Bsmt_SF", "First_Flr_SF", "Second_Flr_SF", 
          "Full_Bath", "Half_Bath", "Bedroom_AbvGr", 
          "Kitchen_AbvGr", "Fireplaces", "Longitude", "Latitude",
          "Bldg_TypeDuplex", "Bldg_TypeOneFam", "Bldg_TypeTwnhs",
          "Bldg_TypeTwnhsE", "Bldg_TypeTwoFmCon",
          "House_StyleOne_and_Half_Unf", "House_StyleOne_Story",
          "House_StyleSFoyer", "House_StyleSLvl", "House_StyleTwo_and_Half_Fin",
          "House_StyleTwo_and_Half_Unf", "House_StyleTwo_Story",
          "Overall_Cond_numeric", "Exter_Cond_numeric")

#Outlier detection and handling for every numeric variable using IQR
for (col in cols) {
  Q1 <- quantile(housing_data[[col]], 0.25)
  Q3 <- quantile(housing_data[[col]], 0.75)
  IQR <- Q3 - Q1
  
  k <- 1.5 #IQR multiplier 
  
  lower_bound <- Q1 - k * IQR
  upper_bound <- Q3 + k * IQR
  
  #Remove outliers
  housing_data <- housing_data[!(housing_data[[col]] < lower_bound | housing_data[[col]] > upper_bound), ]
}

summary(housing_data)

#Drop columns with no range as it has no importance
housing_data <- housing_data[, !colnames(housing_data) %in% c("Kitchen_AbvGr", "Bldg_TypeDuplex",
                                                              "Bldg_TypeTwnhs", "Bldg_TypeTwnhsE",
                                                              "Bldg_TypeTwoFmCon", "Bldg_TypeOneFam", "House_StyleOne_and_Half_Unf",
                                                              "House_StyleSFoyer", "House_StyleSLvl", "House_StyleTwo_and_Half_Fin",
                                                              "House_StyleTwo_and_Half_Unf", "Exter_Cond_numeric")]

summary(housing_data)

#Visualize distribution of numerical variables via Box Plots [After IQR test]
#FIGURE 4 IN REPORT
par(mfrow = c(2, 2)) 
boxplot(housing_data$Lot_Frontage, main = "Box Plot of Lot Frontage")
boxplot(housing_data$Lot_Area, main = "Box Plot of Lot Area")
boxplot(housing_data$Year_Built, main = "Box Plot of Year Built")
boxplot(housing_data$Total_Bsmt_SF, main = "Box Plot of Basement Area")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
boxplot(housing_data$First_Flr_SF, main = "Box Plot of Ground floor area")
boxplot(housing_data$Second_Flr_SF, main = "Box Plot of First floor Area")
boxplot(housing_data$Full_Bath, main = "Box Plot of Full bathroom")
boxplot(housing_data$Half_Bath, main = "Box Plot of Half bathroom")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
boxplot(housing_data$Bedroom_AbvGr, main = "Box Plot of Bedroom Average")
boxplot(housing_data$Fireplaces, main = "Box Plot of Fireplaces")
boxplot(housing_data$Longitude, main = "Box Plot of Longitude")
boxplot(housing_data$Latitude, main = "Box Plot of Latitude")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
boxplot(housing_data$House_StyleOne_Story, main = "Box Plot of House Style (One Story)")
boxplot(housing_data$House_StyleTwo_Story, main = "Box Plot of House Style (Two Story)")
boxplot(housing_data$Overall_Cond_numeric, main = "Box Plot of Overall Condition")
par(mfrow = c(1, 1))


#Data Transformation
#Standardize features for consistency
#Every feature is of numeric type
features <- c("Lot_Frontage","Lot_Area", "Year_Built", "Total_Bsmt_SF",
                      "First_Flr_SF", "Second_Flr_SF", "Full_Bath", "Half_Bath",
                      "Bedroom_AbvGr", "Fireplaces", "Longitude", "Latitude", 
                      "House_StyleOne_Story", "House_StyleTwo_Story", "Overall_Cond_numeric")

#Normalize the data using min-max normalization
min_max_transform <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#Apply min-max scaling function to relevant columns
housing_data[features] <- lapply(housing_data[features], min_max_transform)

summary(housing_data)


#Model Building & Evaluation
#Use seed = student number
set.seed(23187204) #Create reproducible results

#Split data into training and testing data
split_data <- sample(nrow(housing_data), 0.7 * nrow(housing_data)) #70:30 split
train_data <- housing_data[split_data,] #training set used to train the model
test_data <- housing_data[-split_data,] #testing set used to evaluate its performance

#Features chosen are based on their interpretation and relevance from analysis in pre-processing stage
#Model 1
first_model <- Sale_Price ~ Lot_Frontage + Lot_Area + Year_Built + Total_Bsmt_SF +
  First_Flr_SF + Second_Flr_SF + Full_Bath + Half_Bath + Bedroom_AbvGr +
  Fireplaces + Longitude + Latitude + House_StyleOne_Story +
  House_StyleTwo_Story + Overall_Cond_numeric

#Model 1: Summary 
model1 <- lm(first_model, data = train_data)
summary(model1) #Adjusted R-squared:0.8497, Residual standard error: 24880 on 840 df

#Model 1: Tests
#FIGURE 5 IN REPORT
check_model(model1) #Model shows absence of linearity
#Homoscedasticity (Constant Variance):
ncvTest(model1) 
#Chisquare = 224.7722, Df = 1, p = < 2.22e-16
check_heteroscedasticity(model1) #Model violates Homoscedasticity assumption

#Multicollinearity Test:
vif(model1) 
check_collinearity(model1) #Model violates assumption

#Residual Plot
hist(residuals(model1)) #Model meets assumption

#Testing for autocorrelation
durbinWatsonTest(model1)
check_autocorrelation(model1) #Model meets assumption 

#Solution: Log transform the target variable to address heteroscedasticity
train_data$Sale_Price <- log(train_data$Sale_Price)
summary(train_data$Sale_Price)

#Model 2
second_model <- Sale_Price ~ Lot_Frontage + Lot_Area + Year_Built + Total_Bsmt_SF +
  First_Flr_SF + Second_Flr_SF + Full_Bath + Half_Bath + Bedroom_AbvGr +
  Fireplaces + Longitude + Latitude + House_StyleOne_Story +
  House_StyleTwo_Story + Overall_Cond_numeric

#Model 2: Summary
model2 <- lm(second_model, train_data)
summary(model2) #Adjusted R-squared:0.89, Residual standard error: 0.1167 on 890 df

#Model 2: Tests
#FIGURE 7 IN REPORT
check_model(model2) #Model shows signs of linearity
#Homoscedasticity (Constant Variance):
ncvTest(model2) 
#Chisquare = 5.257754, Df = 1, p = 0.021849
check_heteroscedasticity(model2) #Model violates assumption

#Multicollinearity Test:
vif(model2) 
check_collinearity(model2) #Model violates assumption

#Residual Plot
hist(residuals(model2)) #Model meets assumption

#Testing for autocorrelation
durbinWatsonTest(model2) 
check_autocorrelation(model2) #Model meets assumption

#Solution: Drop variables with high VIF (>5) to address multicollinearity
#Model 3
third_model <- Sale_Price ~ Lot_Frontage + Lot_Area + Year_Built + Full_Bath +
  Half_Bath + Bedroom_AbvGr + Fireplaces + Longitude + Latitude + Overall_Cond_numeric

#Model 3: Summary
model3 <- lm(third_model, train_data)
summary(model3) #Adjusted R-squared:  0.7645, Residual standard error: 0.1708 on 895 df

#Model 3: Tests
#FIGURE 8 IN REPORT
check_model(model3) #Model meets assumption
#Homoscedasticity (Constant Variance)
ncvTest(model3) 
#Chisquare = 0.2482488, Df = 1, p = 0.61831
check_heteroscedasticity(model3) #Model meets assumption

#Multicollinearity Test:
vif(model3) 
check_collinearity(model3) #Model meets assumption

#Residual Plot
#FIGURE 10 IN REPORT
#hist(residuals(model3)) #Model meets assumption
#Plot histogram with density curve 
residual_model3 <- residuals(model3)
ggplot(train_data, aes(x = residual_model3)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "red", color = "black") +
  geom_density(color = "black", lwd=2.5) +
  labs(title = "Histogram with Density Curve for Model 3", x = "Residuals", y = "Density") +
  theme_minimal()

#Testing for autocorrelation
durbinWatsonTest(model3) 
check_autocorrelation(model3) #Model meets assumption 

#Final model (model 3): Evaluation
#Evaluate predictive capability using a test dataset
#Predict on the test set using the trained model
test_data$Sale_Price <- log(test_data$Sale_Price)
test_predictions <- predict(model3, newdata = test_data)

#Create a data frame with actual and predicted values
plot_data <- data.frame(Actual = test_data$Sale_Price, Predicted = test_predictions)

#Create a scatter plot to show relationship
#FIGURE 11 IN REPORT
plot(test_data$Sale_Price, test_predictions, 
     xlab = "Actual Sale_Price (log scale)", 
     ylab = "Predicted Sale_Price (log scale)",
     main = "Actual vs. Predicted Sale Price",
     col = "red", pch = 16)

#Assess the R-squared value
rsquared <- summary(model3)$adj.r.squared
print(rsquared) #0.764507

#Cross-validation
#Define parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 10)  #10-fold cross-validation

#Define model
cvmodel_results <- train(Sale_Price ~ ., data = train_data, method = "lm", trControl = ctrl)

#Print the results
print(cvmodel_results)
#RMSE       Rsquared   MAE       
#0.1172726  0.8914042  0.08929857


