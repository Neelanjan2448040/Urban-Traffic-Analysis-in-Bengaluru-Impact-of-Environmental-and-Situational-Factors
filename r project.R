# Load necessary libraries
library(ggplot2)
library(dplyr)
library(MASS)
library(BSDA)

# Load data
data <- read.csv("C:/Users/Aaditya/Desktop/R/synthetic_normal_data.csv")

# Question 1: Investigating the Effect of Area and Weather on Congestion Levels


# Objective 1: Test if there is a significant difference in congestion levels between two areas (e.g., Indiranagar and Koramangala)

# Step 1: Subset data for the two areas (Indiranagar and Koramangala)
area1 <- "Indiranagar"
area2 <- "Koramangala"
data_subset <- data %>% filter(Area_Name %in% c(area1, area2))

# Step 2: Randomly sample 25 observations from each area
set.seed(123) # Set seed for reproducibility
sampled_data <- data_subset %>%
  group_by(Area_Name) %>%
  slice_sample(n = 25) %>%
  ungroup()

# Step 3: Check for normality using Shapiro-Wilk test
shapiro1 <- shapiro.test(sampled_data$Congestion_Level[sampled_data$Area_Name == area1])
shapiro2 <- shapiro.test(sampled_data$Congestion_Level[sampled_data$Area_Name == area2])

cat("Shapiro-Wilk Test for Indiranagar: W =", shapiro1$statistic, "p-value =", shapiro1$p.value, "\n")
cat("Shapiro-Wilk Test for Koramangala: W =", shapiro2$statistic, "p-value =", shapiro2$p.value, "\n")

# Step 4: Test for equality of variances using F-test
variance_test <- var.test(Congestion_Level ~ Area_Name, data = sampled_data)
cat("F-test for equality of variances: F =", variance_test$statistic, "p-value =", variance_test$p.value, "\n")

# Step 5: Independent t-test (assuming normality and equal variances)
t_test_result <- t.test(Congestion_Level ~ Area_Name, data = sampled_data, var.equal = TRUE)
cat("T-test result: t =", t_test_result$statistic, "p-value =", t_test_result$p.value, "\n")

# Step 6: Interpretation based on p-value
if (t_test_result$p.value < 0.05) {
  cat("Result: There is a significant difference in congestion levels between Indiranagar and Koramangala.\n")
} else {
  cat("Result: No significant difference in congestion levels between Indiranagar and Koramangala.\n")
}

# Step 7: Boxplot visualization
library(ggplot2)
ggplot(sampled_data, aes(x = Area_Name, y = Congestion_Level, fill = Area_Name)) +
  geom_boxplot() +
  labs(title = "Congestion Levels by Area (Sampled Data)", x = "Area", y = "Congestion Level") +
  theme_minimal()



# Objective 2: Investigate if weather conditions impact congestion levels


# Step 1: Assumption Check for Normality (Shapiro-Wilk Test)
shapiro_test <- shapiro.test(data$Congestion_Level)
cat("Shapiro-Wilk Test for Normality: W =", shapiro_test$statistic, "p-value =", shapiro_test$p.value, "\n")

# Step 2: Assumption Check for Homogeneity of Variances (Bartlett's Test)
bartlett_test <- bartlett.test(Congestion_Level ~ Weather_Conditions, data = data)
cat("Bartlett's Test for Homogeneity of Variances: K-squared =", bartlett_test$statistic, 
    "p-value =", bartlett_test$p.value, "\n")

# Step 3: One-way ANOVA (if assumptions are met)
anova_result <- aov(Congestion_Level ~ Weather_Conditions, data = data)
anova_summary <- summary(anova_result)
cat("ANOVA Summary: F =", anova_summary[[1]]["Weather_Conditions", "F value"], 
    "p-value =", anova_summary[[1]]["Weather_Conditions", "Pr(>F)"], "\n")

# Step 4: Extract F-value and p-value for comparison with critical F-value
f_value <- anova_summary[[1]]["Weather_Conditions", "F value"]
p_value <- anova_summary[[1]]["Weather_Conditions", "Pr(>F)"]
cat("F-value:", f_value, "p-value:", p_value, "\n")

# Step 5: Compare F-value with critical F-value and decide on Tukey's Test
f_critical <- qf(0.95, df1 = length(unique(data$Weather_Conditions)) - 1, 
                 df2 = nrow(data) - length(unique(data$Weather_Conditions)))
cat("Critical F-value:", f_critical, "\n")

if (f_value >= f_critical) {
  cat("F value is greater than or equal to the tabulated value. Proceeding with Tukey's test.\n")
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
} else {
  cat("F value is less than the tabulated value. No Tukey test required.\n")
}

# Step 6: Visualization: Boxplot for Congestion Level by Weather Conditions
library(ggplot2)
ggplot(data, aes(x = Weather_Conditions, y = Congestion_Level, fill = Weather_Conditions)) +
  geom_boxplot() +
  labs(title = "Congestion Levels by Weather Condition", x = "Weather Condition", y = "Congestion Level") +
  theme_minimal()




# Objective 3: Test if the observed distribution of weather conditions deviates from the expected distribution

# Step 1: Calculate observed frequencies
observed <- table(data$Weather_Conditions)  # Frequencies of each condition
cat("Observed Frequencies:\n")
print(observed)

# Step 2: Define custom expected proportions
expected_proportions <- c(0.25, 0.15, 0.35, 0.15, 0.10)  # Adjust proportions to match your hypothesis
cat("Expected Proportions:\n")
print(expected_proportions)

# Step 3: Ensure the length of observed and expected match
if (length(observed) == length(expected_proportions)) {
  # Step 4: Perform Chi-squared test
  chi_sq_result <- chisq.test(x = observed, p = expected_proportions)
  cat("Chi-squared Test Result:\n")
  print(chi_sq_result)
  
  # Step 5: Visualization: Observed vs Expected Frequencies
  observed_df <- as.data.frame(observed)
  expected_df <- data.frame(Weather_Conditions = names(observed), 
                            Frequency = sum(observed) * expected_proportions)
  
  library(ggplot2)
  ggplot() +
    geom_bar(data = observed_df, aes(x = Var1, y = Freq, fill = "Observed"), 
             stat = "identity", position = "dodge") +
    geom_bar(data = expected_df, aes(x = Weather_Conditions, y = Frequency, fill = "Expected"), 
             stat = "identity", position = "dodge") +
    labs(title = "Observed vs Expected Weather Condition Distribution", 
         x = "Weather Condition", y = "Frequency") +
    scale_fill_manual(values = c("Observed" = "blue", "Expected" = "red")) +
    theme_minimal()
} else {
  cat("Error: Length of observed categories does not match length of expected proportions.\n")
}



# Question 2: Investigating the Effect of Weather Conditions on Traffic Volume


# Objective 1: Compare Traffic Volumes Across Different Weather Conditions Using Kruskal-Wallis Test

# 1. Test for Normality (Optional, for understanding; not required for Kruskal-Wallis)
shapiro_test <- shapiro.test(data$Traffic_Volume)
print(shapiro_test)

# 2. Homogeneity of Variances: Bartlett test (Optional check, not required for Kruskal-Wallis)
bartlett_test <- bartlett.test(Traffic_Volume ~ Weather_Conditions, data = data)
print(bartlett_test)

# Since Bartlett test shows significant p-value, Kruskal-Wallis is the better choice.
# Main Test: Kruskal-Wallis Test
kruskal_result <- kruskal.test(Traffic_Volume ~ Weather_Conditions, data = data)

# Display results
cat("Kruskal-Wallis Test Statistic:", kruskal_result$statistic, "\nP-value:", kruskal_result$p.value, "\n")

# Check if Kruskal-Wallis test result is significant
if (kruskal_result$p.value < 0.05) {
  cat("There is a significant difference in traffic volumes across weather conditions.\n")
} else {
  cat("There is no significant difference in traffic volumes across weather conditions.\n")
}

# Visualization: Boxplot for Traffic Volumes Across Weather Conditions
library(ggplot2)

ggplot(data, aes(x = Weather_Conditions, y = Traffic_Volume, fill = Weather_Conditions)) +
  geom_boxplot() +
  labs(title = "Traffic Volume by Weather Condition", x = "Weather Condition", y = "Traffic Volume") +
  theme_minimal()



# Objective 2: Compare Road Capacity Utilization in Different Weather Conditions Using Mann-Whitney U Test

# Subset data for two weather conditions
weather_condition1 <- "Clear"
weather_condition2 <- "Rain"
data_subset <- data %>% filter(Weather_Conditions %in% c(weather_condition1, weather_condition2))

# Separate data for each weather condition
clear_weather_data <- data_subset$Road_Capacity_Utilization[data_subset$Weather_Conditions == weather_condition1]
rain_weather_data <- data_subset$Road_Capacity_Utilization[data_subset$Weather_Conditions == weather_condition2]

# 1. Mann-Whitney U Test (Wilcoxon rank-sum test)
mann_whitney_result <- wilcox.test(clear_weather_data, rain_weather_data, alternative = "two.sided", conf.level = 0.95)

# Print Mann-Whitney U test results
cat("\nMann-Whitney U Test Results:\n")
print(mann_whitney_result)

# Visualization: Boxplot for road capacity utilization in the two weather conditions
library(ggplot2)

ggplot(data_subset, aes(x = Weather_Conditions, y = Road_Capacity_Utilization, fill = Weather_Conditions)) +
  geom_boxplot() +
  labs(title = "Road Capacity Utilization by Weather Condition", x = "Weather Condition", y = "Road Capacity Utilization") +
  theme_minimal()




# Objective 3: Investigate whether there's a relationship between congestion level and traffic signal compliance using a correlation test.

# Assumption: Normality of data for both variables
shapiro_congestion <- shapiro.test(data$Congestion_Level)
shapiro_signal_compliance <- shapiro.test(data$Traffic_Signal_Compliance)

# Print Shapiro-Wilk test results
print(shapiro_congestion)
print(shapiro_signal_compliance)

# Assumption check: Normality
if (shapiro_congestion$p.value > 0.05 && shapiro_signal_compliance$p.value > 0.05) {
  cat("Both variables are normally distributed. Proceeding with Pearson correlation.\n")
  
  # Main Test: Pearson Correlation
  correlation_test <- cor.test(data$Congestion_Level, data$Traffic_Signal_Compliance, method = "pearson")
} else {
  cat("Normality violated for one or both variables. Proceeding with Spearman correlation.\n")
  
  # Main Test: Spearman Correlation
  correlation_test <- cor.test(data$Congestion_Level, data$Traffic_Signal_Compliance, method = "spearman")
}

# Print correlation test results
print(correlation_test)

# Visualization: Scatter plot for congestion level and traffic signal compliance
library(ggplot2)
ggplot(data, aes(x = Congestion_Level, y = Traffic_Signal_Compliance)) +
  geom_point(aes(color = Weather_Conditions)) +
  labs(title = "Congestion Level vs Traffic Signal Compliance", 
       x = "Congestion Level", y = "Traffic Signal Compliance") +
  theme_minimal()




# Question 3: Impact of Roadwork and Construction Activity on Traffic Volume


# Objective 1: Mann-Whitney U test for difference in Traffic Volume between roads with and without Roadwork/Construction Activity


# Subset data based on Roadwork and Construction Activity ("Yes" vs "No")
data_subset <- data %>% filter(Roadwork_and_Construction_Activity %in% c("Yes", "No"))

# Assumptions for Mann-Whitney U Test:
# 1. The two groups are independent.
# 2. The data does not need to be normally distributed.

# Main Test: Mann-Whitney U Test (Wilcoxon rank-sum test)
mann_whitney_result <- wilcox.test(Traffic_Volume ~ Roadwork_and_Construction_Activity, data = data_subset)
cat("\nMann-Whitney U Test Result:\n")
print(mann_whitney_result)

# Results Interpretation:
if (mann_whitney_result$p.value < 0.05) {
  cat("There is a significant difference in Traffic Volume between the two groups.\n")
} else {
  cat("No significant difference in Traffic Volume between the two groups.\n")
}

# Visualization: Boxplot for Traffic Volume based on Roadwork and Construction Activity
library(ggplot2)
ggplot(data_subset, aes(x = Roadwork_and_Construction_Activity, y = Traffic_Volume, fill = Roadwork_and_Construction_Activity)) +
  geom_boxplot() +
  labs(title = "Traffic Volume by Roadwork and Construction Activity", 
       x = "Roadwork and Construction Activity", 
       y = "Traffic Volume") +
  theme_minimal()




# Objective 2: Chi-Squared Test for Independence (Roadwork and Construction Activity vs Traffic Volume Category)


# Categorize Traffic Volume into categories (Low, Medium, High, Very High)
data$Traffic_Volume_Category <- cut(
  data$Traffic_Volume, 
  breaks = c(0, 10000, 30000, 50000, Inf), 
  labels = c("Low", "Medium", "High", "Very High")
)

# Remove rows with NA values in Traffic_Volume_Category or Roadwork_and_Construction_Activity
data_clean <- na.omit(data[, c("Traffic_Volume_Category", "Roadwork_and_Construction_Activity")])

# Create a contingency table
contingency_table <- table(data_clean$Roadwork_and_Construction_Activity, data_clean$Traffic_Volume_Category)

# Assumptions for Chi-squared test:
# Check if all expected frequencies are > 5
expected_frequencies <- chisq.test(contingency_table)$expected
if (all(expected_frequencies > 5)) {
  cat("All expected frequencies are greater than 5. Proceeding with the Chi-squared test.\n")
} else {
  cat("Warning: Some expected frequencies are less than 5. Chi-squared test may not be valid.\n")
}

# Perform Chi-squared test
chi_sq_test <- chisq.test(contingency_table)
cat("\nChi-squared Test for Independence:\n")
print(chi_sq_test)

# Results Interpretation
if (chi_sq_test$p.value < 0.05) {
  cat("There is a significant relationship between Roadwork/Construction Activity and Traffic Volume Category.\n")
} else {
  cat("No significant relationship between Roadwork/Construction Activity and Traffic Volume Category.\n")
}

# Visualization: Bar plot for contingency table
library(ggplot2)
ggplot(data_clean, aes(x = Traffic_Volume_Category, fill = Roadwork_and_Construction_Activity)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Traffic Volume Category vs Roadwork/Construction Activity",
    x = "Traffic Volume Category",
    y = "Count"
  ) +
  theme_minimal()




# Objective 3: Two-Way ANOVA to Compare Traffic Volume across Area Names when Roadwork and Construction Activity is Present vs Absent


# Assumptions for Two-Way ANOVA:

# 1. Normality of residuals for each group (Shapiro-Wilk test)
cat("\nChecking Assumptions for Two-Way ANOVA:\n")
cat("1. Normality of residuals (Shapiro-Wilk Test):\n")

shapiro_test_anova <- shapiro.test(residuals(lm(Traffic_Volume ~ Area_Name * Roadwork_and_Construction_Activity, data = data)))
print(shapiro_test_anova)

if (shapiro_test_anova$p.value > 0.05) {
  cat("Residuals are normally distributed (p-value =", shapiro_test_anova$p.value, ").\n")
} else {
  cat("Residuals are NOT normally distributed (p-value =", shapiro_test_anova$p.value, ").\n")
}

# 2. Homogeneity of variances (Bartlett test)
cat("\n2. Homogeneity of variances (Bartlett Test):\n")
bartlett_test_anova <- bartlett.test(Traffic_Volume ~ interaction(Area_Name, Roadwork_and_Construction_Activity), data = data)
print(bartlett_test_anova)

if (bartlett_test_anova$p.value > 0.05) {
  cat("Variances are homogeneous (p-value =", bartlett_test_anova$p.value, ").\n")
} else {
  cat("Variances are NOT homogeneous (p-value =", bartlett_test_anova$p.value, ").\n")
}

# Main Test: Two-Way ANOVA
cat("\nConducting Two-Way ANOVA:\n")
anova_result <- aov(Traffic_Volume ~ Area_Name * Roadwork_and_Construction_Activity, data = data)
anova_summary <- summary(anova_result)
print(anova_summary)

# Extract the F-value and tabulated F-value
f_value <- anova_summary[[1]]$`F value`[3]  # Interaction term F value
f_tabulated <- qf(0.95, df1 = anova_summary[[1]]$Df[3], df2 = anova_summary[[1]]$Df[4])

cat("\nF-value (Interaction Term):", f_value, "\nF-tabulated (Critical):", f_tabulated, "\n")

# Check if Tukey's test is needed
if (f_value >= f_tabulated) {
  cat("F value is greater than or equal to the tabulated value. Proceeding with Tukey's test.\n")
  
  # Tukey's Test
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
} else {
  cat("F value is less than the tabulated value. No Tukey test required.\n")
}


# Assumptions for Two-Way ANOVA:

# 1. Normality of residuals for each group (Shapiro-Wilk test)
cat("\nChecking Assumptions for Two-Way ANOVA:\n")
cat("1. Normality of residuals (Shapiro-Wilk Test):\n")

# Calculate residuals from the linear model
lm_model <- lm(Traffic_Volume ~ Area_Name * Roadwork_and_Construction_Activity, data = data)

# Perform Shapiro-Wilk test on residuals
shapiro_test_anova <- shapiro.test(residuals(lm_model))
print(shapiro_test_anova)

if (shapiro_test_anova$p.value > 0.05) {
  cat("Residuals are normally distributed (p-value =", shapiro_test_anova$p.value, ").\n")
} else {
  cat("Residuals are NOT normally distributed (p-value =", shapiro_test_anova$p.value, ").\n")
}

# 2. Homogeneity of variances (Bartlett test)
cat("\n2. Homogeneity of variances (Bartlett Test):\n")
bartlett_test_anova <- bartlett.test(Traffic_Volume ~ interaction(Area_Name, Roadwork_and_Construction_Activity), data = data)
print(bartlett_test_anova)

if (bartlett_test_anova$p.value > 0.05) {
  cat("Variances are homogeneous (p-value =", bartlett_test_anova$p.value, ").\n")
} else {
  cat("Variances are NOT homogeneous (p-value =", bartlett_test_anova$p.value, ").\n")
}

# Main Test: Two-Way ANOVA
cat("\nConducting Two-Way ANOVA:\n")
anova_result <- aov(Traffic_Volume ~ Area_Name * Roadwork_and_Construction_Activity, data = data)
anova_summary <- summary(anova_result)
print(anova_summary)

# Extract the F-value for both factors and interaction term
f_value_area_name <- anova_summary[[1]]$`F value`[1]  # F-value for Area_Name
f_value_roadwork <- anova_summary[[1]]$`F value`[2]  # F-value for Roadwork_and_Construction_Activity
f_value_interaction <- anova_summary[[1]]$`F value`[3]  # F-value for Interaction term

# Extract degrees of freedom for each term
df_area_name <- anova_summary[[1]]$Df[1]
df_roadwork <- anova_summary[[1]]$Df[2]
df_interaction <- anova_summary[[1]]$Df[3]
df_residuals <- anova_summary[[1]]$Df[4]

# Compute the critical F-value (tabulated) for the interaction term
f_tabulated_interaction <- qf(0.95, df1 = df_interaction, df2 = df_residuals)

cat("\nF-value (Area_Name):", f_value_area_name, "\n")
cat("F-value (Roadwork_and_Construction_Activity):", f_value_roadwork, "\n")
cat("F-value (Interaction Term):", f_value_interaction, "\n")
cat("\nF-tabulated (Critical) for Interaction Term:", f_tabulated_interaction, "\n")

# Check if Tukey's test is needed
if (f_value_interaction >= f_tabulated_interaction) {
  cat("F value (Interaction Term) is greater than or equal to the tabulated value. Proceeding with Tukey's test.\n")
  
  # Tukey's Test
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
} else {
  cat("F value (Interaction Term) is less than the tabulated value. No Tukey test required.\n")
}

# Enhanced Interaction Plot


ggplot(data, aes(x = Roadwork_and_Construction_Activity, 
                 y = Traffic_Volume, 
                 color = Area_Name, 
                 group = Area_Name)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5, size = 2) + 
  geom_line(aes(linetype = Area_Name), size = 1) + 
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", size = 0.5, alpha = 0.3) +
  labs(title = "Interaction Plot: Traffic Volume by Area and Roadwork/Construction Activity",
       x = "Roadwork and Construction Activity (Yes/No)",
       y = "Traffic Volume",
       color = "Area Name",
       linetype = "Area Name") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12))






# Question 4: Exploring the Effect of Weather Conditions on Public Transport Usage



# Objective 1: One-Way ANOVA for Public Transport Usage by Weather Conditions

# Assumptions for One-Way ANOVA:
# 1. Normality of residuals for each group (Shapiro-Wilk test).
cat("\nPerforming Shapiro-Wilk Test for Normality of Residuals:\n")
shapiro_tests <- by(data$Public_Transport_Usage, data$Weather_Conditions, function(x) shapiro.test(x)$p.value)
cat("\nShapiro-Wilk Test Results (p-values):\n")
print(shapiro_tests)

# 2. Homogeneity of variances (Bartlett test).
cat("\nPerforming Bartlett Test for Homogeneity of Variances:\n")
bartlett_test <- bartlett.test(Public_Transport_Usage ~ Weather_Conditions, data = data)
cat("\nBartlett Test Results:\n")
print(bartlett_test)

# Main Test: One-Way ANOVA
cat("\nPerforming One-Way ANOVA:\n")
anova_result <- aov(Public_Transport_Usage ~ Weather_Conditions, data = data)
anova_summary <- summary(anova_result)
cat("\nOne-Way ANOVA Summary:\n")
print(anova_summary)

# Check F-value and determine if Tukey's post hoc test is needed
f_value <- anova_summary[[1]]$`F value`[1]  # Extract the first F-value
f_tabulated <- qf(0.95, df1 = anova_summary[[1]]$Df[1], df2 = anova_summary[[1]]$Df[2])

cat("\nF-value from ANOVA:", f_value, "\nF-tabulated:", f_tabulated, "\n")

if (f_value >= f_tabulated) {
  cat("\nF value is greater than or equal to the tabulated value. Proceeding with Tukey's test.\n")
  tukey_result <- TukeyHSD(anova_result)
  cat("\nTukey's Post Hoc Test Results:\n")
  print(tukey_result)
} else {
  cat("\nF value is less than the tabulated value. No Tukey test required.\n")
}

# Visualization: Boxplot for Public Transport Usage across Weather Conditions

ggplot(data, aes(x = Weather_Conditions, y = Public_Transport_Usage, fill = Weather_Conditions)) +
  geom_boxplot() +
  labs(title = "Public Transport Usage by Weather Condition", x = "Weather Condition", y = "Public Transport Usage") +
  theme_minimal()



# Objective 2: Spearman Rank Correlation between Environmental Impact and Public Transport Usage

# Assumption: Monotonic relationship between variables.
cat("\nAssumption Check: Monotonic relationship between Environmental Impact and Public Transport Usage.\n")
cat("Proceeding with Spearman Rank Correlation calculation.\n")

# Calculate Spearman rank correlation
spearman_corr <- cor.test(data$Environmental_Impact, data$Public_Transport_Usage, method = "spearman")

# Print the Spearman correlation result
cat("\nSpearman Rank Correlation Test Results:\n")
print(spearman_corr)

# Check the p-value and correlation coefficient
cat("\nSpearman's rho (Correlation Coefficient):", spearman_corr$estimate, "\n")
cat("p-value:", spearman_corr$p.value, "\n")
cat("Null hypothesis: rho = 0 (no monotonic relationship)\n")
if (spearman_corr$p.value < 0.05) {
  cat("The correlation is statistically significant. We reject the null hypothesis.\n")
} else {
  cat("The correlation is not statistically significant. We fail to reject the null hypothesis.\n")
}

# Visualization: Scatter plot for Environmental Impact vs Public Transport Usage

ggplot(data, aes(x = Environmental_Impact, y = Public_Transport_Usage)) +
  geom_point(aes(color = Weather_Conditions), size = 2) +
  labs(title = "Environmental Impact vs Public Transport Usage", x = "Environmental Impact", y = "Public Transport Usage") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "red")  # Add trend line


# Objective 3: Poisson Regression to predict Public Transport Usage by Weather Conditions

# Step 1: Remove rows with negative or extreme Public_Transport_Usage values
cat("\nStep 1: Removing rows with negative or extreme Public_Transport_Usage values...\n")
data <- data[data$Public_Transport_Usage >= 0, ]
cat("Removed rows with negative Public Transport Usage values.\n")

# Remove extreme outliers (values > 3 SD from the mean)
outlier_threshold <- mean(data$Public_Transport_Usage) + 3 * sd(data$Public_Transport_Usage)
cat("\nOutlier threshold for Public_Transport_Usage (mean + 3 SD):", outlier_threshold, "\n")
data <- data[data$Public_Transport_Usage <= outlier_threshold, ]
cat("Removed rows with extreme outliers in Public Transport Usage.\n")

# Step 2: Visualizations and Assumption Checks

# 2.1: Check linearity assumption using a scatter plot with smooth line
cat("\nStep 2.1: Checking linearity assumption...\n")
ggplot(data, aes(x = Weather_Conditions, y = Public_Transport_Usage)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linearity Check", x = "Weather Conditions", y = "Public Transport Usage") +
  theme_minimal()

# 2.2: Homoscedasticity check using residuals vs fitted values
cat("\nStep 2.2: Checking homoscedasticity...\n")
linear_model <- lm(Public_Transport_Usage ~ Weather_Conditions, data = data)
par(mfrow = c(1, 2))  # Set up 1 row, 2 columns for plots
plot(linear_model$fitted.values, rstandard(linear_model),
     xlab = "Fitted Values", ylab = "Standardized Residuals",
     main = "Residuals vs Fitted", pch = 19)
abline(h = 0, col = "red")

# 2.3: Normality check for residuals using QQ plot and Shapiro-Wilk test
cat("\nStep 2.3: Checking normality of residuals...\n")
qqnorm(residuals(linear_model))
qqline(residuals(linear_model), col = "red")
cat("\nShapiro-Wilk Test for Normality of Residuals:\n")
shapiro_test <- shapiro.test(residuals(linear_model))
print(shapiro_test)

# Step 3: Fit Poisson regression model
cat("\nStep 3: Fitting Poisson regression model...\n")
poisson_model <- glm(Public_Transport_Usage ~ Weather_Conditions, family = poisson(), data = data)
cat("\nPoisson Model Summary:\n")
summary(poisson_model)

# Check for overdispersion (Residual deviance / df should be close to 1)
dispersion_ratio <- summary(poisson_model)$deviance / summary(poisson_model)$df.residual
cat("\nDispersion Ratio for Poisson Model:", dispersion_ratio, "\n")
if (dispersion_ratio > 1.5) {
  cat("\nWarning: Potential overdispersion. Consider using Negative Binomial regression.\n")
}

# Step 4: Fit Negative Binomial regression model to address overdispersion
cat("\nStep 4: Fitting Negative Binomial regression model...\n")
negbinom_model <- glm.nb(Public_Transport_Usage ~ Weather_Conditions, data = data)
cat("\nNegative Binomial Model Summary:\n")
summary(negbinom_model)

# Step 5: Visualize the Negative Binomial regression results

ggplot(data, aes(x = Weather_Conditions, y = Public_Transport_Usage)) +
  geom_point(aes(color = Weather_Conditions), size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE, color = "blue") +
  labs(title = "Negative Binomial Regression: Public Transport Usage vs. Weather Conditions",
       x = "Weather Conditions", y = "Public Transport Usage") +
  theme_minimal()




# Question 5: Analyzing the Relationship Between Congestion Level and Travel Time Index


# Objective 1: Determine if the median Travel Time Index differs significantly from a hypothesized value using a Wilcoxon Signed-Rank Test.

# Step 1: Assumption - Normality Check
cat("\nChecking the assumption of normality for Travel Time Index...\n")
shapiro_test <- shapiro.test(data$Travel_Time_Index)
print(shapiro_test)

if (shapiro_test$p.value < 0.05) {
  cat("Result: The data significantly deviates from normality (p < 0.05).\n")
  cat("Proceeding with Wilcoxon Signed-Rank Test (non-parametric).\n")
} else {
  cat("Result: The data does not significantly deviate from normality (p ≥ 0.05).\n")
  cat("Consider using a parametric test if assumptions are met.\n")
}

# Step 2: Main Test - Wilcoxon Signed-Rank Test for a Single Median
cat("\nConducting Wilcoxon Signed-Rank Test...\n")
mu <- 1.5  # Hypothesized median

# Perform Wilcoxon Signed-Rank Test
wilcox_test <- wilcox.test(data$Travel_Time_Index, mu = mu, alternative = "two.sided")
print(wilcox_test)

# Step 3: Visualization - Histogram with Hypothesized Median

hist(data$Travel_Time_Index, 
     main = "Travel Time Index Distribution", 
     xlab = "Travel Time Index", 
     col = "lightblue", 
     border = "black")
abline(v = mu, col = "red", lwd = 2, lty = 2)  # Add line for hypothesized median
legend("topright", legend = c("Hypothesized Median"), col = c("red"), lty = 2, lwd = 2)



# Objective 2: Investigate the monotonic relationship between Congestion Level and Travel Time Index using Spearman's Rank Correlation.

# Main Test: Spearman's Rank Correlation

# Step 1: Perform Spearman's correlation test
correlation_result <- cor.test(data$Congestion_Level, data$Travel_Time_Index, 
                               method = "spearman")

# Step 2: Display the results
cat("Spearman's Rank Correlation Coefficient:", correlation_result$estimate, "\n")
cat("P-value:", correlation_result$p.value, "\n")

# Step 3: Plot - Scatterplot with Lowess Trend Line
plot(data$Travel_Time_Index, data$Congestion_Level, 
     main = "Congestion Level vs Travel Time Index",
     xlab = "Travel Time Index", ylab = "Congestion Level", 
     pch = 19, col = "blue")
lines(lowess(data$Travel_Time_Index, data$Congestion_Level), col = "red", lwd = 2)
legend("topright", legend = c("Trend Line"), col = c("red"), lwd = 2)

# Handle warning: If ties are present, it's normal, no need to worry
if (correlation_result$p.value >= 0.05) {
  cat("Result: No significant monotonic relationship (p-value >= 0.05).\n")
} else {
  cat("Result: Significant monotonic relationship found (p-value < 0.05).\n")
}


# Objective 3: Check for equality of medians of Congestion Level across different ranges of Travel Time Index using the Kruskal-Wallis Test.

# Step 1: Create groups based on Travel Time Index (3 groups: Low, Medium, High)
data$Travel_Time_Groups <- cut(data$Travel_Time_Index, breaks = 3, 
                               labels = c("Low", "Medium", "High"))

# Step 2: Perform the Kruskal-Wallis test
kruskal_result <- kruskal.test(Congestion_Level ~ Travel_Time_Groups, data = data)

# Step 3: Display the results of the Kruskal-Wallis test
cat("Kruskal-Wallis Test Statistic:", kruskal_result$statistic, "\n")
cat("P-value:", kruskal_result$p.value, "\n")

# Step 4: Interpretation based on p-value
if (kruskal_result$p.value < 0.05) {
  cat("Result: There is a significant difference in the medians of Congestion Level across Travel Time Index groups.\n")
} else {
  cat("Result: No significant difference in the medians of Congestion Level across Travel Time Index groups.\n")
}

# Step 5: Plot - Boxplot to visualize Congestion Level across groups
boxplot(data$Congestion_Level ~ data$Travel_Time_Groups, 
        main = "Congestion Level Across Travel Time Index Groups", 
        xlab = "Travel Time Groups", ylab = "Congestion Level", 
        col = c("lightblue", "lightgreen", "lightpink"), border = "black")

# Add a legend to the boxplot
legend("topright", legend = c("Low", "Medium", "High"), 
       fill = c("lightblue", "lightgreen", "lightpink"), border = "black")





# Objective 2: Chi-Squared Test for Independence (Roadwork and Construction Activity vs Congestion Level Category)

# Categorize Congestion Level into categories (Low, Moderate, High, Severe)
data$Congestion_Level_Category <- cut(
  data$Congestion_Level, 
  breaks = c(0, 50, 100, 150, Inf), 
  labels = c("Low", "Moderate", "High", "Severe")
)

# Remove rows with NA values in Congestion_Level_Category or Roadwork_and_Construction_Activity
data_clean <- na.omit(data[, c("Congestion_Level_Category", "Roadwork_and_Construction_Activity")])

# Create a contingency table
contingency_table <- table(data_clean$Roadwork_and_Construction_Activity, data_clean$Congestion_Level_Category)

# Assumptions for Chi-squared test:
# Check if all expected frequencies are > 5
expected_frequencies <- chisq.test(contingency_table)$expected
if (all(expected_frequencies > 5)) {
  cat("All expected frequencies are greater than 5. Proceeding with the Chi-squared test.\n")
} else {
  cat("Warning: Some expected frequencies are less than 5. Chi-squared test may not be valid.\n")
}

# Perform Chi-squared test
chi_sq_test <- chisq.test(contingency_table)
print(chi_sq_test)

# Results Interpretation
if (chi_sq_test$p.value < 0.05) {
  cat("There is a significant relationship between Roadwork/Construction Activity and Congestion Level Category.\n")
} else {
  cat("No significant relationship between Roadwork/Construction Activity and Congestion Level Category.\n")
}

# Visualization: Bar plot for contingency table
library(ggplot2)
ggplot(data_clean, aes(x = Congestion_Level_Category, fill = Roadwork_and_Construction_Activity)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Congestion Level Category vs Roadwork/Construction Activity",
    x = "Congestion Level Category",
    y = "Count"
  )



# Main Test: Two-Way ANOVA
cat("\nConducting Two-Way ANOVA:\n")

# Perform Two-Way ANOVA
anova_result <- aov(Traffic_Volume ~ Area_Name * Roadwork_and_Construction_Activity, data = data)
anova_summary <- summary(anova_result)
print(anova_summary)

# Extract F-values for each factor and interaction term
f_value_area_name <- anova_summary[[1]]$`F value`[1]  # F-value for Area_Name
f_value_roadwork <- anova_summary[[1]]$`F value`[2]  # F-value for Roadwork_and_Construction_Activity
f_value_interaction <- anova_summary[[1]]$`F value`[3]  # F-value for Interaction term

