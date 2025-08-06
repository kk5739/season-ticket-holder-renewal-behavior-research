# Load packages
suppressMessages({
  library(dplyr)
  library(readxl)
  library(caret)
  library(e1071)
  library(ggplot2)
})

# Load cleaned data
merch_data <- readRDS("./data/cleaned_merchandise_data.rds")
ticket_data <- readRDS("./data/cleaned_ticketing_data.rds")

# Merge datasets
combined_df <- merge(merch_data, ticket_data, by = "SSB_CRMSYSTEM_CONTACT_ID")

# Convert target variable
combined_df$renewed <- ifelse(combined_df$renewed == "Yes", 1, 0)

# Train-test split
set.seed(2025)
sample_idx <- sample(c(TRUE, FALSE), nrow(combined_df), replace = TRUE, prob = c(0.8, 0.2))
train_df <- combined_df[sample_idx, ]
test_df  <- combined_df[!sample_idx, ]

# Logistic regression model
model <- glm(renewed ~ TotalSpending + TotalQytSold, data = train_df, family = "binomial")
summary(model)

# Predictions
pred_probs <- predict(model, newdata = test_df, type = "response")
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Confusion matrix
conf_mat <- confusionMatrix(factor(pred_labels), factor(test_df$renewed))
print(conf_mat)

# Save model
saveRDS(model, file = "./model/renewal_logistic_model.rds")

# Optional: Plot predicted probabilities
ggplot(data.frame(prob = pred_probs, truth = factor(test_df$renewed)), aes(x = prob, fill = truth)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
  labs(title = "Predicted Probabilities of Renewal", x = "Probability", fill = "Actual Renewal") +
  theme_minimal()

# -------------------------
# Cross-Validation Section
# -------------------------

set.seed(2025)
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
crossval_model <- train(
  renewed ~ TotalSpending + TotalQytSold,
  data = combined_df,
  method = "glm",
  family = "binomial",
  trControl = train_control
)

print(crossval_model)
