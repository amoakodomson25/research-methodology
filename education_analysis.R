# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(knitr)

# Import 2017-2018 dataset (percentages)
data_1718 <- data.frame(
  Region = "Ghana",
  Category = rep(c("Total", "Never attended", "Basic education", 
                   "Secondary/vocational", "Tertiary"), each = 2),
  Locality = rep(c("Urban", "Rural"), times = 5),
  Male = c(67.1, 65.4, 17.1, 27.2, 30.8, 29.7, 10.9, 5.9, 8.3, 2.6),
  Female = c(32.9, 34.6, 12.7, 18.9, 16.1, 13.9, 2.8, 1.4, 1.3, 0.3)
)

# Import 2024 dataset (counts)
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
    Male_Pct = Male / Both_Sexes * 100,
    Female_Pct = Female / Both_Sexes * 100
  ) %>%
  select(-Both_Sexes, -Male, -Female) %>%
  rename(Male = Male_Pct, Female = Female_Pct)