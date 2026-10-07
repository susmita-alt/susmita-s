# Overfitting a data
# Load the built-in airquality dataset
data(airquality)

# Remove rows with missing values
air <- na.omit(airquality)

# Set seed so the same train/test split is produced
set.seed(123)

# Split data into training (70%) and test (30%) sets
train_id <- sample(1:nrow(air), size = 0.7 * nrow(air))

train <- air[train_id, ]
test <- air[-train_id, ]

# Create a data frame to store errors
results <- data.frame(
  degree = 1:10,
  training_error = NA,
  test_error = NA
)

# Fit polynomial models from degree 1 to 10
for (d in 1:10) {
  
  model <- lm(
    Ozone ~ poly(Temp, degree = d, raw = TRUE),
    data = train
  )
  
  # Predict on training data
  train_pred <- predict(model, newdata = train)
  
  # Predict on test data
  test_pred <- predict(model, newdata = test)
  
  # Calculate Mean Squared Error
  results$training_error[d] <- mean(
    (train$Ozone - train_pred)^2
  )
  
  results$test_error[d] <- mean(
    (test$Ozone - test_pred)^2
  )
}

# View the results
results

# CREATE THE CHART
library(ggplot2)

ggplot(results, aes(x = degree)) +
  geom_line(aes(y = training_error, color = "Training error"), linewidth = 1) +
  geom_point(aes(y = training_error, color = "Training error")) +
  geom_line(aes(y = test_error, color = "Test error"), linewidth = 1) +
  geom_point(aes(y = test_error, color = "Test error")) +
  labs(
    title = "Training vs. Test Error",
    x = "Polynomial Degree",
    y = "Mean Squared Error",
    color = "Error"
  ) +
  theme_minimal()