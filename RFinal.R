# Install once (if not already installed)
install.packages("readxl")

# Load the library
library(readxl)





# Step 2: Load your Excel dataset
#  Replace the file name below with your actual Excel file name
df <- read_excel("D:\\Dhavalrp\\Desktop\\R\\R\\world-happiness-report.xlsx")




#  Step 3: Check the data
cat("\n===== FIRST FEW ROWS =====\n")
print(head(df))

cat("\n===== DATA STRUCTURE =====\n")
str(df)

cat("\n===== SUMMARY OF DATA =====\n")
print(summary(df))





# Clean column
names(df) <- gsub(" ", "_", tolower(trimws(names(df))), fixed = TRUE)
print(names(df))

colSums(is.na(df))  #to see no of missing value




# -------------------------------
#  Fill Missing Values in R
# -------------------------------

#  Fill missing numeric columns with their median
num_cols <- names(df)[sapply(df, is.numeric)]        #checks every column to see if it’s numeric.

for (col in num_cols) {
  median_val <- median(df[[col]], na.rm = TRUE)  #na.rm = ignore NA values
  df[[col]][is.na(df[[col]])] <- median_val
}

#  Fill missing categorical (character or factor) columns with mode
cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]

get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

for (col in cat_cols) {
  mode_val <- get_mode(df[[col]])
  df[[col]][is.na(df[[col]])] <- mode_val
}







# Check if all NAs handled
cat("\n===== MISSING VALUES AFTER FILLING =====\n")
print(colSums(is.na(df)))









# ==============================
# Step 7: Remove Duplicates
# ==============================
df <- unique(df)
cat("\n Duplicates removed. Remaining rows:", nrow(df), "\n")





# ==============================
#  Step 8: Save Cleaned Data
# ==============================
write.csv(df, "D:\\Dhavalrp\\Desktop\\R\\R\\world-happiness-report-clean.csv", row.names = FALSE)
cat("\n Ceaned dataset saved as 'world-happiness-report-clean.csv'\n")


sum(duplicated(df))







library(tidyr)

# Example: convert wide to long format
df_long <- pivot_longer(df, cols = starts_with("score"), 
                        names_to = "year", values_to = "happiness_score")

# Then your code will work
cat("\nTOTAL ROWS:", nrow(df_long), "\n")
cat("TOTAL MISSING VALUES:", sum(is.na(df_long)), "\n")
cat("ANY DUPLICATES? ", any(duplicated(df_long[, c("country_or_region", "year")])), "\n")

names(df)








library(tidyr)
library(dplyr)

#convert score_year to year
df_long <- df %>%
  pivot_longer(cols = starts_with("score_"),
               names_to = "year",
               values_to = "happiness_score") %>%
  mutate(year = as.integer(sub("score_", "", year)))







# Now you can do:
#avg happiness score
selected_year <- 2021
df_long %>%
  filter(year == selected_year) %>%
  summarise(avg_happiness = mean(happiness_score, na.rm = TRUE))

#all year
cat("\n===== AVERAGE HAPPINESS BY YEAR =====\n")

avg_happiness <- df_long %>%
  group_by(year) %>%
  summarise(average_happiness = mean(happiness_score, na.rm = TRUE))

print(avg_happiness)








# Manually choose your desired year 
selected_year <- 2021

cat("\n===== TOP 10 HAPPIEST COUNTRIES IN", selected_year, "=====\n")

top10 <- df_long %>%
  filter(year == selected_year) %>%
  arrange(desc(happiness_score)) %>%
  select(country_or_region, happiness_score) %>%
  head(10)

print(top10)






#plot graph of avg happiness of year
library(ggplot2)

cat("\n===== SHOWING GLOBAL HAPPINESS TREND =====\n")

ggplot(avg_happiness, aes(x = year, y = average_happiness)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(
    title = " Global Happiness Trend (2019–2022)",
    x = "Year",
    y = "Average Happiness"
  ) +
  theme_minimal()






top_countries <- c("Finland", "Denmark", "India", "United States")

cat("\n===== SHOWING HAPPINESS COMPARISON PLOT =====\n")

ggplot(df_long %>% filter(country_or_region %in% top_countries),
       aes(x = year, y = happiness_score, color = country_or_region)) +
  geom_line(size = 1.2) +  #“geom” means geometric object
  geom_point(size = 2) +
  labs(
    title = " Happiness Over Time by Country (2019–2022)",
    x = "Year",
    y = "Happiness Score",
    color = "Country"
  ) +
  theme_minimal()





# Load required libraries
library(dplyr)
library(ggplot2)

# Step 1: Choose your desired year
selected_year <- 2021   # You can change this to 2019, 2020, or 2021

# Step 2: Filter data and get Top 10 happiest countries  
top10_year <- df_long %>%
  filter(year == selected_year) %>%
  arrange(desc(happiness_score)) %>%
  select(country_or_region, happiness_score) %>%
  head(10)

# Step 3: Print top 10 (optional)
cat("\n===== TOP 10 HAPPIEST COUNTRIES IN", selected_year, "=====\n")
print(top10_year)

# Step 4: Create the plot
p <- ggplot(top10_year, aes(x = reorder(country_or_region, -happiness_score), #The minus sign (-) sorts them in descending order
                            y = happiness_score,
                            fill = country_or_region)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = paste("Top 10 Happiest Countries in", selected_year),
    x = "Country",
    y = "Happiness Score"
  ) +
  theme_minimal() +
  geom_text(aes(label = round(happiness_score, 2)), 
            vjust = -0.5, size = 3.5) +  # show labels above bars
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 5: Display the plot
print(p)









# -------------------------------------------------------------
#  STEP 6: BUILD A PREDICTION MODEL (LINEAR REGRESSION)
# -------------------------------------------------------------

# Load necessary libraries
library(dplyr)
library(caret)   # for training/testing split and model evaluation

# -------------------------------------------------------------
# STEP 1: Prepare Data for Modeling
# -------------------------------------------------------------

# We'll use 2022 data (you can change year if you want)
selected_year <- 2022

data_model <- df_long %>%
  filter(year == selected_year) %>%
  select(happiness_score,
         gdp_per_capita,
         social_support,
         healthy_life_expectancy,
         freedom_to_make_life_choices,
         generosity,
         perceptions_of_corruption)

# Drop missing values just in case
data_model <- na.omit(data_model)

cat("\n===== SAMPLE OF MODEL DATA =====\n")
print(head(data_model))

# -------------------------------------------------------------
# STEP 2: Split Data into Training and Testing Sets
# -------------------------------------------------------------
set.seed(42)  # for reproducibility
train_index <- createDataPartition(data_model$happiness_score, p = 0.8, list = FALSE)

train_data <- data_model[train_index, ]
test_data  <- data_model[-train_index, ]

cat("\nTraining set rows:", nrow(train_data), "\n")
cat("Testing set rows:", nrow(test_data), "\n")

# -------------------------------------------------------------
# STEP 3: Train Linear Regression Model
# -------------------------------------------------------------
model <- lm(happiness_score ~ ., data = train_data)

cat("\n===== MODEL SUMMARY =====\n")
print(summary(model))

# -------------------------------------------------------------
# STEP 4: Make Predictions
# -------------------------------------------------------------
pred <- predict(model, newdata = test_data)

# Evaluate R² (how good the model fits)
r2 <- cor(test_data$happiness_score, pred)^2

cat("\n===== MODEL PERFORMANCE =====\n")
cat("R² Score:", round(r2, 3), "\n")

# Show actual vs predicted (first 10)
comparison <- data.frame(
  Actual = round(test_data$happiness_score, 2),
  Predicted = round(pred, 2)
)
cat("\n===== ACTUAL VS PREDICTED (FIRST 10) =====\n")
print(head(comparison, 10))


# -------------------------------------------------------------
# 📈 STEP 7: VISUALIZE MODEL PERFORMANCE
# -------------------------------------------------------------

library(ggplot2)

#  Scatter Plot — Actual vs Predicted
ggplot(data = data.frame(Actual = test_data$happiness_score, Predicted = pred),
       aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Actual vs Predicted Happiness Scores",
    x = "Actual Happiness Score",
    y = "Predicted Happiness Score"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Residual Plot — Model Errors
residuals <- test_data$happiness_score - pred

ggplot(data.frame(Predicted = pred, Residuals = residuals),
       aes(x = Predicted, y = Residuals)) +
  geom_point(color = "purple", size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = " Residual Plot (Model Errors)",
    x = "Predicted Happiness Score",
    y = "Residuals (Actual - Predicted)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))






# -------------------------------------------------------------
# STEP 5: Linear Model Accuracy
# -------------------------------------------------------------
rmse_linear <- sqrt(mean((test_data$happiness_score - pred)^2))
mae_linear <- mean(abs(test_data$happiness_score - pred))
accuracy_linear <- r2 * 100

cat("\n===== LINEAR MODEL ACCURACY =====\n")
cat("R² Accuracy:", round(accuracy_linear, 2), "%\n")
cat("RMSE:", round(rmse_linear, 3), "\n")
cat("MAE:", round(mae_linear, 3), "\n")










#make more accurate

# -------------------------------------------------------------
# STEP 6 (Improved): BUILD A BETTER PREDICTION MODEL (NON-LINEAR)
# -------------------------------------------------------------

# Load necessary libraries
library(dplyr)
library(caret)
library(ggplot2)

# -------------------------------------------------------------
# STEP 1: Prepare Data for Modeling
# -------------------------------------------------------------
selected_year <- 2022   # you can change this to 2021, 2020, etc.

data_model <- df_long %>%
  filter(year == selected_year) %>%
  select(happiness_score,
         gdp_per_capita,
         social_support,
         healthy_life_expectancy,
         freedom_to_make_life_choices,
         generosity,
         perceptions_of_corruption)



cat("\n===== SAMPLE OF MODEL DATA =====\n")
print(head(data_model))

# -------------------------------------------------------------
# STEP 2: Split Data into Training and Testing Sets
# -------------------------------------------------------------
set.seed(42)
train_index <- createDataPartition(data_model$happiness_score, p = 0.8, list = FALSE)
train_data <- data_model[train_index, ]
test_data  <- data_model[-train_index, ]

cat("\nTraining set rows:", nrow(train_data), "\n")
cat("Testing set rows:", nrow(test_data), "\n")

# -------------------------------------------------------------
# STEP 3: Build a Non-Linear (Polynomial) Regression Model
# -------------------------------------------------------------
# Add polynomial (squared) terms for key variables
model_poly <- lm(
  happiness_score ~ poly(gdp_per_capita, 2, raw = TRUE) +
    poly(social_support, 2, raw = TRUE) +
    poly(healthy_life_expectancy, 2, raw = TRUE) +
    freedom_to_make_life_choices +
    generosity +
    perceptions_of_corruption,
  data = train_data
)

cat("\n===== MODEL SUMMARY (Non-linear) =====\n")
print(summary(model_poly))

# -------------------------------------------------------------
# STEP 4: Make Predictions and Evaluate
# -------------------------------------------------------------
pred_poly <- predict(model_poly, newdata = test_data)
r2_poly <- cor(test_data$happiness_score, pred_poly)^2

cat("\n===== MODEL PERFORMANCE (Non-linear) =====\n")
cat("R² Score:", round(r2_poly, 3), "\n")

comparison_poly <- data.frame(
  Actual = round(test_data$happiness_score, 2),
  Predicted = round(pred_poly, 2)
)
cat("\n===== ACTUAL VS PREDICTED (FIRST 10) =====\n")
print(head(comparison_poly, 10))

# -------------------------------------------------------------
#  STEP 5: Visualize Results
# -------------------------------------------------------------

#  Actual vs Predicted Plot
ggplot(data = data.frame(Actual = test_data$happiness_score, Predicted = pred_poly),
       aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = " Actual vs Predicted Happiness Scores (Non-linear Model)",
    x = "Actual Happiness Score",
    y = "Predicted Happiness Score"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#  Residual Plot
residuals_poly <- test_data$happiness_score - pred_poly
ggplot(data.frame(Predicted = pred_poly, Residuals = residuals_poly),
       aes(x = Predicted, y = Residuals)) +
  geom_point(color = "purple", size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = " Residual Plot (Non-linear Model Errors)",
    x = "Predicted Happiness Score",
    y = "Residuals (Actual - Predicted)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))







# -------------------------------------------------------------
# STEP 6: Non-linear Model Accuracy
# -------------------------------------------------------------
rmse_poly <- sqrt(mean((test_data$happiness_score - pred_poly)^2))
mae_poly <- mean(abs(test_data$happiness_score - pred_poly))
accuracy_poly <- r2_poly * 100

cat("\n===== NON-LINEAR MODEL ACCURACY =====\n")
cat("R² Accuracy:", round(accuracy_poly, 2), "%\n")
cat(" RMSE:", round(rmse_poly, 3), "\n")
cat(" MAE:", round(mae_poly, 3), "\n")







#Compare Both Models Side by Side

comparison_results <- data.frame(
  Model = c("Linear Regression", "Non-linear Regression"),
  Accuracy_Percent = c(round(accuracy_linear, 2), round(accuracy_poly, 2)),
  RMSE = c(round(rmse_linear, 3), round(rmse_poly, 3)),
  MAE = c(round(mae_linear, 3), round(mae_poly, 3))
)

cat("\n=====  MODEL COMPARISON =====\n")
print(comparison_results)









