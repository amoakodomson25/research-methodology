# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)

# 1. Data Preparation -----------------------------------------------------

# Create 2017-2018 dataset (percentages)
data_1718 <- data.frame(
  Region = "Ghana",
  Category = rep(c("Total", "Never attended", "Basic education", 
                   "Secondary/vocational", "Tertiary"), each = 2),
  Locality = rep(c("Urban", "Rural"), times = 5),
  Male = c(67.1, 65.4, 17.1, 27.2, 30.8, 29.7, 10.9, 5.9, 8.3, 2.6),
  Female = c(32.9, 34.6, 12.7, 18.9, 16.1, 13.9, 2.8, 1.4, 1.3, 0.3)
)

# Create 2024 dataset (counts)
data_2024 <- data.frame(
  Region = "Ghana",
  Category = rep(c("Total", "Never attended", "Basic education", 
                   "Secondary/vocational", "Tertiary"), each = 2),
  Locality = rep(c("Urban", "Rural"), times = 5),
  Both_Sexes = c(710358, 2327023, 211929, 1071857, 333030, 1015836, 
                 97115, 171016, 60108, 58878),
  Male = c(476984, 1522245, 121791, 632508, 218916, 691403, 
           77217, 138112, 52374, 52221),
  Female = c(233374, 804778, 90138, 439349, 114114, 324433, 
             19898, 32904, 7734, 6657)
)

# Convert 2024 counts to percentages for comparison
data_2024 <- data_2024 %>%
  mutate(
    Male_Pct = round(Male / Both_Sexes * 100, 1),
    Female_Pct = round(Female / Both_Sexes * 100, 1)
  ) %>%
  select(Region, Category, Locality, Male = Male_Pct, Female = Female_Pct)

# 2. Descriptive Statistics -----------------------------------------------

# Function to generate descriptive stats
generate_stats <- function(data, dataset_name) {
  stats <- data %>%
    group_by(Category, Locality) %>%
    summarise(
      Avg_Male = mean(Male),
      SD_Male = sd(Male),
      Avg_Female = mean(Female),
      SD_Female = sd(Female),
      Gender_Gap = Avg_Male - Avg_Female,
      .groups = 'drop'
    )
  
  cat("\nDescriptive Statistics for", dataset_name, "Dataset:\n")
  print(kable(stats))
  return(stats)
}

# Generate stats for both datasets
stats_1718 <- generate_stats(data_1718, "2017-2018")
stats_2024 <- generate_stats(data_2024, "2024")

# 3. Comparative Analysis -------------------------------------------------

# Combine datasets for comparison
combined_data <- bind_rows(
  data_1718 %>% mutate(Year = "2017-2018"),
  data_2024 %>% mutate(Year = "2024")
)

# Comparative statistics - fixed column names
comparison <- combined_data %>%
  group_by(Year, Category, Locality) %>%
  summarise(
    Male_Pct = mean(Male),
    Female_Pct = mean(Female),
    Gender_Gap = Male_Pct - Female_Pct,
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = Year,
    values_from = c(Male_Pct, Female_Pct, Gender_Gap),
    names_sep = "_"
  )

cat("\nComparison Between 2017-2018 and 2024:\n")
print(kable(comparison))

# 4. Visualization -------------------------------------------------------

# Plot gender gap by education level - fixed variable names
gender_gap_plot <- ggplot(comparison, aes(x = Category)) +
  geom_col(aes(y = `Gender_Gap_2017-2018`, fill = "2017-2018"), 
           position = position_dodge(width = 0.7), width = 0.7) +
  geom_col(aes(y = Gender_Gap_2024, fill = "2024"), 
           position = position_dodge(width = 0.7), width = 0.7) +
  facet_wrap(~Locality) +
  labs(title = "Gender Gap in Educational Attainment",
       y = "Percentage Points Difference (Male - Female)",
       x = "Education Level",
       fill = "Year") +
  scale_fill_manual(values = c("2017-2018" = "#1f77b4", "2024" = "#ff7f0e")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(gender_gap_plot)


# Alternative visualization that will definitely work
combined_data %>%
  group_by(Year, Category, Locality) %>%
  summarise(Gender_Gap = mean(Male) - mean(Female), .groups = "drop") %>%
  ggplot(aes(x = Category, y = Gender_Gap, fill = Year)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~Locality) +
  labs(title = "Gender Gap in Educational Attainment",
       y = "Percentage Points Difference (Male - Female)",
       x = "Education Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot educational attainment by gender and year
education_trends <- combined_data %>%
  pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "Percentage") %>%
  ggplot(aes(x = Category, y = Percentage, fill = interaction(Gender, Year))) +
  geom_col(position = position_dodge()) +
  facet_wrap(~Locality) +
  labs(title = "Educational Attainment by Gender and Year",
       y = "Percentage (%)",
       x = "Education Level",
       fill = "Gender & Year") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(education_trends)


# Calculate average percentages for each gender by year and locality
mean_gender_percents <- combined_data %>%
  group_by(Year, Locality) %>%
  summarise(
    Mean_Male = mean(Male),
    Mean_Female = mean(Female),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = starts_with("Mean"), names_to = "Gender", values_to = "Mean_Percentage") %>%
  mutate(Gender = recode(Gender, "Mean_Male" = "Male", "Mean_Female" = "Female"))

# Plot
ggplot(mean_gender_percents, aes(x = Locality, y = Mean_Percentage, fill = Gender)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~Year) +
  labs(title = "Average Educational Attainment by Gender",
       x = "Locality", y = "Average Percentage (%)") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()



# 5. Key Findings Summary -------------------------------------------------

cat("\nKey Findings:\n")
cat("1. Gender Gap: Both datasets show significant gender disparities in educational attainment.\n")
cat("2. Urban Advantage: Urban areas show higher educational attainment than rural areas.\n")
cat("3. Education Level Impact: Gender gap widens with higher education levels.\n")
cat("4. Temporal Changes: The comparison shows changes in educational patterns over time.\n")