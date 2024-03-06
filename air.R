# Read the dataset from CSV file
aircraft_engines <- read.csv("~/Documents/R Programs/aircraft_engines.csv")


# Display the structure of the dataset
str(aircraft_engines)

# Summary statistics
summary(aircraft_engines)



# Load the necessary libraries
library(ggplot2)
library(dplyr)
# Load necessary library for Gradient Boosting
library(gbm)

# Create a binary outcome variable
aircraft_engines$engine_failure <- ifelse(aircraft_engines$engine_model == "MP14-PXDK", 1, 0)

# Convert categorical variables to factors
aircraft_engines$engine_model_grouped <- as.factor(ifelse(aircraft_engines$engine_model %in% c("MP14-PXDK", "O-470", "O-360-A2A"), aircraft_engines$engine_model, "OtherModels"))

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_indices <- sample(seq_len(nrow(aircraft_engines)), 0.8 * nrow(aircraft_engines))
train_data <- aircraft_engines[train_indices, ]
test_data <- aircraft_engines[-train_indices, ]

# Define the Gradient Boosting model
gb_formula <- as.formula("engine_failure ~ engine_model_grouped")
gb_model <- gbm(
  formula = gb_formula,
  data = train_data,
  distribution = "bernoulli",  # for binary classification
  n.trees = 100,  # you can adjust the number of trees
  interaction.depth = 3,  # you can adjust the depth of interaction
  shrinkage = 0.1  # you can adjust the shrinkage parameter
)

# Make predictions on the test set
predictions <- predict(gb_model, newdata = test_data, type = "response")

# Convert predicted probabilities to labels
predicted_status <- ifelse(predictions > 0.5, "Engine has failure", "Engine does not have failure")

# Display the final prediction
cat("Final Prediction:", predicted_status, "\n")




# Bar plot: Engine Manufacture vs. Engine Model
ggplot(aircraft_engines, aes(x = engine_manufacture, fill = engine_model)) +
  geom_bar(position = "stack", stat = "count") +
  labs(title = "Engine Manufacture vs. Engine Model",
       x = "Engine Manufacture",
       y = "Count",
       fill = "Engine Model") +
  theme_minimal()

# Plot 2: Fuel Injection Type distribution
ggplot(aircraft_engines, aes(x = fuel_injection_type)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Fuel Injection Types",
       x = "Fuel Injection Type",
       y = "Count") +
  theme_minimal()

# Plot 3: Power vs. Last Changed Date
ggplot(aircraft_engines, aes(x = last_changed_date, y = power)) +
  geom_point() +
  labs(title = "Power vs. Last Changed Date",
       x = "Last Changed Date",
       y = "Power") +
  theme_minimal()
# Bar plot: Propeller Type vs. Engine Model (Percentage)
ggplot(aircraft_engines, aes(x = engine_model, fill = propeller_type)) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Propeller Type vs. Engine Model (Percentage)",
       x = "Engine Model",
       y = "Percentage",
       fill = "Propeller Type") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Convert y-axis to percentage
  theme_minimal()

# Bar plot: Propeller Type vs. Engine Model
ggplot(aircraft_engines, aes(x = engine_model, fill = propeller_type)) +
  geom_bar(position = "stack", stat = "count") +
  labs(title = "Propeller Type vs. Engine Model",
       x = "Engine Model",
       y = "Count",
       fill = "Propeller Type") +
  theme_minimal()


