install.packages("leaps")
library(leaps)
library(caret)
library(car)

install.packages("glmnet")
library(glmnet)
library(tidyverse)
library(dplyr)



# Read the CSV file
data <- read.csv("/Users/karthikradhakrishnan/Library/CloudStorage/OneDrive-Personal/SMU MS Data Science/Courses/DS 6371 Statistical Foundations for Data Science/Final Project/Data/train.csv")

# Function to count NA values in each column
count_na <- function(x) sum(is.na(x))

# Count NA values in each column
na_counts <- sapply(data, count_na)

# Filter columns with NA values and sort in descending order
columns_with_na <- na_counts[na_counts > 0]
columns_with_na <- sort(columns_with_na, decreasing = TRUE)

# Print the results
cat("Columns with NA values and their counts:\n")
print(columns_with_na)




# Handling Missing Values

# LotFrontage: Median imputation grouped by neighborhood
data <- data %>%
  group_by(Neighborhood) %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage), median(LotFrontage, na.rm = TRUE), LotFrontage)) %>%
  ungroup()

# MasVnrArea: Impute missing values with 0
data$MasVnrArea[is.na(data$MasVnrArea)] <- 0

# Alley, FireplaceQu, Fence, PoolQC: Create "None" category for missing values
data$Alley[is.na(data$Alley)] <- "None"
data$FireplaceQu[is.na(data$FireplaceQu)] <- "None"
data$Fence[is.na(data$Fence)] <- "None"
data$PoolQC[is.na(data$PoolQC)] <- "None"

# Garage-related variables
garage_vars <- c("GarageType", "GarageYrBlt", "GarageFinish", "GarageQual", "GarageCond")

data <- data %>%
  mutate(
    GarageType = ifelse(is.na(GarageType) & apply(is.na(select(., all_of(garage_vars))), 1, all), "NoGarage", GarageType),
    GarageYrBlt = ifelse(GarageType == "NoGarage", 0, GarageYrBlt),
    GarageFinish = ifelse(GarageType == "NoGarage", "NoGarage", GarageFinish),
    GarageQual = ifelse(GarageType == "NoGarage", "NoGarage", GarageQual),
    GarageCond = ifelse(GarageType == "NoGarage", "NoGarage", GarageCond)
  )

# Basement-related variables
bsmt_vars <- c("BsmtExposure", "BsmtFinType2", "BsmtQual", "BsmtCond", "BsmtFinType1")
data <- data %>%
  mutate(
    BsmtExposure = ifelse(is.na(BsmtExposure) & apply(is.na(select(., all_of(bsmt_vars))), 1, all), "NoBsmt", BsmtExposure),
    BsmtFinType2 = ifelse(BsmtExposure == "NoBsmt", "NoBsmt", BsmtFinType2),
    BsmtQual = ifelse(BsmtExposure == "NoBsmt", "NoBsmt", BsmtQual),
    BsmtCond = ifelse(BsmtExposure == "NoBsmt", "NoBsmt", BsmtCond),
    BsmtFinType1 = ifelse(BsmtExposure == "NoBsmt", "NoBsmt", BsmtFinType1)
  )

# MasVnrType: Create "None" category for missing values
data$MasVnrType[is.na(data$MasVnrType)] <- "None"

# Check for any remaining NA values
remaining_na <- colSums(is.na(data))
print(remaining_na[remaining_na > 0])


# Convert categorical variables to factors
categorical_vars <- c("MSZoning", "Street", "Alley", "LotShape", "LandContour", "Utilities", 
                      "LotConfig", "LandSlope", "Neighborhood", "Condition1", "Condition2", 
                      "BldgType", "HouseStyle", "RoofStyle", "RoofMatl", "Exterior1st", 
                      "Exterior2nd", "MasVnrType", "ExterQual", "ExterCond", "Foundation", 
                      "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", 
                      "Heating", "HeatingQC", "CentralAir", "Electrical", "KitchenQual", 
                      "Functional", "FireplaceQu", "GarageType", "GarageFinish", "GarageQual", 
                      "GarageCond", "PavedDrive", "PoolQC", "Fence", "MiscFeature", "SaleType", 
                      "SaleCondition")

data[categorical_vars] <- lapply(data[categorical_vars], as.factor)


# Select numeric variables and SalePrice for the model
numeric_vars <- sapply(data, is.numeric)

# Get names of numeric columns
numeric_col_names <- names(data)[numeric_vars]

# Create model_data with selected columns
model_data <- data[, numeric_col_names]

# Print the names of selected columns to verify
print(names(model_data))

# Remove Id column and any other non-predictive variables
model_data$Id <- NULL


# Check for multicollinearity


predictors <- model_data[, names(model_data) != "SalePrice"]
cor_matrix <- cor(predictors)
high_cor <- which(abs(cor_matrix) > 0.6 & abs(cor_matrix) < 1, arr.ind = TRUE)
high_cor <- high_cor[high_cor[,1] < high_cor[,2], ]
                  
predictors <- model_data[, names(model_data) != "SalePrice"]
cor_matrix <- cor(predictors)        
                  
# Convert correlation matrix to a long format dataframe
cor_df <- as.data.frame(as.table(cor_matrix))
names(cor_df) <- c("Var1", "Var2", "Correlation")

# Calculate absolute correlation and remove self-correlations
cor_df$AbsCorrelation <- abs(cor_df$Correlation)
cor_df <- cor_df[cor_df$Var1 != cor_df$Var2, ]

# Sort by absolute correlation in descending order
cor_df <- cor_df[order(-cor_df$AbsCorrelation), ]

# Print the top correlations (optional)
print(head(cor_df, 20))

# Plot the correlations
library(ggplot2)

ggplot(cor_df, aes(x = Var1, y = Var2, fill = AbsCorrelation)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Absolute Correlation Heatmap",
       x = "Variables", y = "Variables",
       fill = "Absolute\nCorrelation")                  




# Filter for absolute correlations greater than 0.7 and remove self-correlations
high_cor_df <- cor_df[cor_df$AbsCorrelation > 0.7 & cor_df$Var1 != cor_df$Var2, ]

# Create a unique identifier for each pair
high_cor_df$Pair <- apply(high_cor_df[, c("Var1", "Var2")], 1, function(x) paste(sort(x), collapse="_"))

# Remove duplicates based on the unique pair identifier
high_cor_df <- high_cor_df[!duplicated(high_cor_df$Pair), ]

# Sort by absolute correlation in descending order
high_cor_df <- high_cor_df[order(-high_cor_df$AbsCorrelation), ]

# Round the correlation values to 3 decimal places for readability
high_cor_df$Correlation <- round(high_cor_df$Correlation, 3)
high_cor_df$AbsCorrelation <- round(high_cor_df$AbsCorrelation, 3)

# Remove the Pair column as it's no longer needed
high_cor_df$Pair <- NULL

# Print the results
print(high_cor_df)



# Check for multicollinearity using VIF

# Fit the model
model <- lm(SalePrice ~ ., data = model_data)

# Check for aliased coefficients
aliased_vars <- alias(model)$Complete
print(aliased_vars)


# Remove variables with perfect multicollinearity
# Remove the component variables
variables_to_remove <- c("TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF")
model_data_reduced <- model_data[, !names(model_data) %in% variables_to_remove]

# Refit the model
model_reduced <- lm(SalePrice ~ ., data = model_data_reduced)

# Check for remaining aliased coefficients
aliased_vars <- alias(model_reduced)$Complete
print(aliased_vars)

vif_values <- vif(model_reduced)
# Print VIF values
print(vif_values)

