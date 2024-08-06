# Importing Libraries and Data
library(caret)
library(BART)
library(dplyr)
library(forcats)

# Please change as needed! 
austinhouses <- read.csv("C:/Users/ethan/OneDrive/Desktop/MSBA Summer Classes/STA S380/austinhouses.csv")

# Drop unwanted columns
austinhouses <- austinhouses %>%
  select(-streetAddress, -description)

# Function to aggregate zipcode into regions
aggregate_zipcode <- function(zipcode) {
  if (zipcode >= 78701 && zipcode <= 78705) {
    return('Central Austin')
  } else if (zipcode >= 78721 && zipcode <= 78725) {
    return('East Austin')
  } else if (zipcode >= 78731 && zipcode <= 78735) {
    return('West Austin')
  } else if (zipcode >= 78741 && zipcode <= 78747) {
    return('South Austin')
  } else if (zipcode >= 78750 && zipcode <= 78759) {
    return('North Austin')
  } else {
    return('Other')
  }
}

# Apply the zipcode aggregation
austinhouses <- austinhouses %>%
  mutate(
    zipcode_aggregated = sapply(zipcode, aggregate_zipcode)
  ) %>%
  select(-zipcode, -homeType, -latest_saledate)

# Convert specific columns to factors
austinhouses <- austinhouses %>%
  mutate(
    hasAssociation = as.factor(hasAssociation),
    hasGarage = as.factor(hasGarage),
    hasSpa = as.factor(hasSpa),
    hasView = as.factor(hasView),
    latest_salemonth = as.factor(latest_salemonth),
    zipcode_aggregated = as.factor(zipcode_aggregated)
  )

# Creating dummy variables
# Remove the intercept to avoid multicollinearity
dummy_vars <- model.matrix(~ . - 1, data = austinhouses)

# Convert the dummy variables into a data frame
austinhouses <- as.data.frame(dummy_vars)

# Drop extra column
austinhouses <- austinhouses %>%
  select(-hasAssociationFALSE)

# Train Test Split

set.seed(22)

train_ix = createDataPartition(austinhouses$latestPrice,
                               p = 0.8)
austin_train = austinhouses[train_ix$Resample1,]
austin_test  = austinhouses[-train_ix$Resample1,]

y_train <- log(austin_train$latestPrice)

predictor_columns <- setdiff(names(austinhouses), "latestPrice")
X_train <- austin_train[, predictor_columns]
X_test <- austin_test[, predictor_columns]

# Consider reading in X_train, x_test, y_train, y_test from Python?

# Fitting the BART model

set.seed(22)
bart.austin <- gbart(X_train, y_train, X_test)

yhat.bart <- bart.austin$yhat.test.mean

yhat.bart <- exp(yhat.bart)

test_mse_bart <- mean((yhat.bart - austin_test$latestPrice)^2)
print(test_mse_bart)