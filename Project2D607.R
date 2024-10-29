---
title: "Project 2"
author: "Jose Fuentes"
date: "2024-10-27"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Project 2 DATA 607: Preparing Dataset for Analysis

```{r libraries}
# Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)
library(viridis)
```


#Defining file path and steps to tidy the first dataset

#First Dataset: HIV Dataset in NYC
```{r tidying-data1}
# Define file path
file_path <- "C:/Users/Dell/Downloads/HIV_AIDS_Diagnoses_20241014.csv"

# Step 1: Read the CSV file
data <- read_csv(file_path)

# Step 2: Rename columns for clarity
data <- data %>%
  rename(
    Year = `YEAR`,
    Borough = `Borough`,
    Neighborhood = `Neighborhood (U.H.F)`,
    Sex = `SEX`,
    Race_Ethnicity = `RACE/ETHNICITY`,
    Total_HIV_Diagnoses = `TOTAL NUMBER OF HIV DIAGNOSES`,
    HIV_Diagnoses_Per_100k = `HIV DIAGNOSES PER 100,000 POPULATION`,
    Total_Concurrent_HIV_AIDS_Diagnoses = `TOTAL NUMBER OF CONCURRENT HIV/AIDS DIAGNOSES`,
    Proportion_Concurrent_HIV_AIDS_Diagnoses = `PROPORTION OF CONCURRENT HIV/AIDS DIAGNOSES AMONG ALL HIV DIAGNOSES`,
    Total_AIDS_Diagnoses = `TOTAL NUMBER OF AIDS DIAGNOSES`,
    AIDS_Diagnoses_Per_100k = `AIDS DIAGNOSES PER 100,000 POPULATION`
  )

# Step 3: Split Neighborhood column where necessary
# Some neighborhoods might represent more than one area, split them into separate rows
data <- data %>%
  separate_rows(Neighborhood, sep = " - ")

# Step 4: Fill missing values in Borough column based on Neighborhood if known
data <- data %>%
  mutate(
    Borough = case_when(
      Neighborhood == "Greenpoint" ~ "Brooklyn",
      Neighborhood == "Stapleton" ~ "Staten Island",
      Neighborhood == "Southeast Queens" ~ "Queens",
      Neighborhood == "Upper Westside" ~ "Manhattan",
      Neighborhood == "Willowbrook" ~ "Staten Island",
      Neighborhood == "East Flatbush" ~ "Brooklyn",
      Neighborhood == "Southwest Queens" ~ "Queens",
      Neighborhood == "Fordham" ~ "Bronx",
      Neighborhood == "Flushing" ~ "Queens",
      TRUE ~ Borough  # Keep existing Borough values
    )
  )

# Step 5: Treat "All" entries in Sex and Race/Ethnicity columns
data <- data %>%
  mutate(
    Sex = ifelse(Sex == "All", NA, Sex),  # Replace "All" with NA
    Race_Ethnicity = ifelse(Race_Ethnicity == "All", NA, Race_Ethnicity)  # Replace "All" with NA
  )

# Step 6: Replace missing values with NA
data <- data %>%
  replace_na(list(
    Borough = "Unknown",
    Sex = "Unknown",
    Race_Ethnicity = "Unknown"
  ))

# Step 7: Ensure all numeric columns are numeric and replace NA values
data <- data %>%
  mutate(
    Total_HIV_Diagnoses = as.numeric(Total_HIV_Diagnoses),
    HIV_Diagnoses_Per_100k = as.numeric(HIV_Diagnoses_Per_100k),
    Total_Concurrent_HIV_AIDS_Diagnoses = as.numeric(Total_Concurrent_HIV_AIDS_Diagnoses),
    Proportion_Concurrent_HIV_AIDS_Diagnoses = as.numeric(Proportion_Concurrent_HIV_AIDS_Diagnoses),
    Total_AIDS_Diagnoses = as.numeric(Total_AIDS_Diagnoses),
    AIDS_Diagnoses_Per_100k = as.numeric(AIDS_Diagnoses_Per_100k)
  )

# Step 8: View the cleaned dataset
print(data)

# Optional: Write the cleaned data to a new CSV file
write_csv(data, "C:/Users/Dell/Downloads/Tidy_HIV_AIDS_Diagnoses_20241014.csv")
```

## Including Plots
**1)HIV Diagnosis Rates by Gender and Race/Ethnicity **
```{r hiv-diagnosis}
# Load necessary libraries
library(dplyr)
library(ggplot2)

#Setting new dataset: 
dataset <- read.csv("C:/Users/Dell/Downloads/Tidy_HIV_AIDS_Diagnoses_20241014.csv")

# Summarize HIV Diagnosis Rates by Gender and Race/Ethnicity
hiv_by_gender_race <- dataset %>%
  group_by(Sex, Race_Ethnicity) %>%
  summarise(average_rate = mean(HIV_Diagnoses_Per_100k, na.rm = TRUE))

# Plot the results
ggplot(hiv_by_gender_race, aes(x = Sex, y = average_rate, fill = Race_Ethnicity)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "HIV Diagnosis Rates by Gender and Race/Ethnicity",
       y = "Average HIV Diagnosis Rate per 100,000",
       x = "Gender") +
  theme_minimal()
```

**Temporal Trends: Number of HIV and AIDS Diagnoses Over the Years**

```{r temporal-trends}
hiv_aids_trends <- dataset %>%
  group_by(Year) %>%
  summarise(total_hiv_diagnoses = sum(`Total_HIV_Diagnoses`, na.rm = TRUE),
            total_aids_diagnoses = sum(`Total_AIDS_Diagnoses`, na.rm = TRUE))

# Plot the results
ggplot(hiv_aids_trends, aes(x = Year)) +
  geom_line(aes(y = total_hiv_diagnoses, color = "HIV Diagnoses")) +
  geom_line(aes(y = total_aids_diagnoses, color = "AIDS Diagnoses")) +
  labs(title = "Temporal Trends in HIV and AIDS Diagnoses",
       y = "Number of Diagnoses",
       x = "Year") +
  scale_color_manual(name = "Diagnosis Type", values = c("HIV Diagnoses" = "blue", "AIDS Diagnoses" = "red")) +
  theme_minimal()
```

**Year with most HIV diagnoses**
```{r top-year}
# Calculate the total HIV diagnoses per year
hiv_by_year <- dataset %>%
  group_by(Year) %>%
  summarise(total_hiv_diagnoses = sum(Total_HIV_Diagnoses, na.rm = TRUE))

# Find the year with the most HIV diagnoses
year_most_hiv <- hiv_by_year %>%
  filter(total_hiv_diagnoses == max(total_hiv_diagnoses))

year_most_hiv
```

**HIV Diagnoses by Neighborhood (Percentage)**
```{r diagnosis-neighborhood}
# Create a summary of HIV diagnoses by neighborhood
hiv_by_neighborhood <- dataset %>%
  group_by(Neighborhood) %>%
  summarise(total_hiv_diagnoses = sum(Total_HIV_Diagnoses, na.rm = TRUE))

# Calculate percentage of total diagnoses per neighborhood
hiv_by_neighborhood <- hiv_by_neighborhood %>%
  mutate(percentage = (total_hiv_diagnoses / sum(total_hiv_diagnoses)) * 100)

# Plot the histogram
ggplot(hiv_by_neighborhood, aes(x = reorder(Neighborhood, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Percentage of HIV Diagnoses by Neighborhood",
       x = "Neighborhood",
       y = "Percentage of Total HIV Diagnoses") +
  theme_minimal()
```

**Geographical Patterns: HIV/AIDS Diagnoses by Neighborhood**
```{r geo-patterns}
# Summarize HIV and AIDS Diagnoses by Neighborhood
hiv_by_neighborhood <- dataset %>%
  group_by(Neighborhood) %>%
  summarise(
    total_hiv_diagnoses = sum(Total_HIV_Diagnoses, na.rm = TRUE),
    total_aids_diagnoses = sum(Total_AIDS_Diagnoses, na.rm = TRUE)
  )

# Plot the results for HIV Diagnoses by Neighborhood
ggplot(hiv_by_neighborhood, aes(x = reorder(Neighborhood, total_hiv_diagnoses), y = total_hiv_diagnoses)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "HIV Diagnoses by Neighborhood",
       y = "Total HIV Diagnoses",
       x = "Neighborhood") +
  theme_minimal()
```

**Intersectional Analysis: HIV Diagnoses by Gender and Race/Ethnicity**
```{r intersectional}
# Intersectional Analysis by Gender and Race/Ethnicity
intersectional_analysis <- dataset %>%
  group_by(Sex, `Race_Ethnicity`) %>%
  summarise(total_hiv_diagnoses = sum(`Total_HIV_Diagnoses`, na.rm = TRUE))

# Plot the results
ggplot(intersectional_analysis, aes(x = Sex, y = total_hiv_diagnoses, fill = `Race_Ethnicity`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Intersectional Analysis of HIV Diagnoses by Gender and Race/Ethnicity",
       y = "Total HIV Diagnoses",
       x = "Gender") +
  theme_minimal()
```
**Relation newly HIV and Aids diagnosis**
```{r relation}
# Summarize the total number of HIV and AIDS diagnoses for the pie chart
diagnosis_summary <- dataset %>%
  summarise(
    total_hiv_diagnoses = sum(Total_HIV_Diagnoses, na.rm = TRUE),
    total_aids_diagnoses = sum(Total_AIDS_Diagnoses, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(total_hiv_diagnoses, total_aids_diagnoses), names_to = "Diagnosis", values_to = "Count")

# Create pie chart
ggplot(diagnosis_summary, aes(x = "", y = Count, fill = Diagnosis)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Proportion of Newly Diagnosed HIV vs AIDS Cases") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

```

# Statistics Summary:
```{r statistics}
# Get a statistical summary of all numeric columns in the dataset
summary(dataset)

```

##Loading and preparing second dataset: Drinking water quality

```{r data2}
# Load necessary libraries
library(tidyverse)

# Define the URL of the CSV file
url <- "https://github.com/Jomifum/Project2D607/blob/main/Drinking_Water_Quality_Distribution_Monitoring_Data_20241016.csv?raw=true"

# Read the data from the URL
data <- read_csv(url)

# View the column names and first few rows to check for any issues
print(colnames(data))
print(head(data))

# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

```

#Tyding the dataset 2:
```{r tyding-data2}
# Load necessary libraries
library(tidyverse)

# Define the URL of the CSV file
url <- "https://github.com/Jomifum/Project2D607/blob/main/Drinking_Water_Quality_Distribution_Monitoring_Data_20241016.csv?raw=true"

# Read the data from the URL
data <- read_csv(url, col_types = cols(
  `Sample Number` = col_double(),
  `Sample Date` = col_character(),
  `Sample Time` = col_character(),
  `Sample Site` = col_character(),
  `Sample class` = col_character(),
  `Residual Free Chlorine (mg/L)` = col_double(),
  `Turbidity (NTU)` = col_character(),
  `Fluoride (mg/L)` = col_double(),
  `Coliform (Quanti-Tray) (MPN /100mL)` = col_character(),
  `E.coli(Quanti-Tray) (MPN/100mL)` = col_character()
))

# Handling missing values
data <- data %>%
  mutate(
    `Sample Site` = ifelse(is.na(`Sample Site`), "Unknown", `Sample Site`),
    `Residual Free Chlorine (mg/L)` = ifelse(is.na(`Residual Free Chlorine (mg/L)`), median(data$`Residual Free Chlorine (mg/L)`, na.rm = TRUE), `Residual Free Chlorine (mg/L)`),
    `Turbidity (NTU)` = ifelse(is.na(`Turbidity (NTU)`), "Unknown", `Turbidity (NTU)`),
    `Fluoride (mg/L)` = ifelse(is.na(`Fluoride (mg/L)`), median(data$`Fluoride (mg/L)`, na.rm = TRUE), `Fluoride (mg/L)`),
    `Coliform (Quanti-Tray) (MPN /100mL)` = ifelse(is.na(`Coliform (Quanti-Tray) (MPN /100mL)`), "Unknown", `Coliform (Quanti-Tray) (MPN /100mL)`),
    `E.coli(Quanti-Tray) (MPN/100mL)` = ifelse(is.na(`E.coli(Quanti-Tray) (MPN/100mL)`), "Unknown", `E.coli(Quanti-Tray) (MPN/100mL)`)
  )

# Check the tidied data
print(head(data))

# Check for missing values again
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

# Save the tidied data to a CSV file
write_csv(data, "tidied_data2.csv")


```

#Statistics: 
```{r statistics-data2}
library(tidyverse)

# Read the data from the URL
url <- "https://github.com/Jomifum/Project2D607/blob/main/Drinking_Water_Quality_Distribution_Monitoring_Data_20241016.csv?raw=true"
data <- read_csv(url)

# Handle missing values as previously described
data <- data %>%
  mutate(
    `Sample Site` = ifelse(is.na(`Sample Site`), "Unknown", `Sample Site`),
    `Residual Free Chlorine (mg/L)` = ifelse(is.na(`Residual Free Chlorine (mg/L)`), median(data$`Residual Free Chlorine (mg/L)`, na.rm = TRUE), `Residual Free Chlorine (mg/L)`),
    `Turbidity (NTU)` = ifelse(is.na(`Turbidity (NTU)`), "Unknown", `Turbidity (NTU)`),
    `Fluoride (mg/L)` = ifelse(is.na(`Fluoride (mg/L)`), median(data$`Fluoride (mg/L)`, na.rm = TRUE), `Fluoride (mg/L)`),
    `Coliform (Quanti-Tray) (MPN /100mL)` = ifelse(is.na(`Coliform (Quanti-Tray) (MPN /100mL)`), "Unknown", `Coliform (Quanti-Tray) (MPN /100mL)`),
    `E.coli(Quanti-Tray) (MPN/100mL)` = ifelse(is.na(`E.coli(Quanti-Tray) (MPN/100mL)`), "Unknown", `E.coli(Quanti-Tray) (MPN/100mL)`)
  )

# Summary statistics
summary_stats <- data %>% 
  summarise(
    min_chlorine = min(`Residual Free Chlorine (mg/L)`, na.rm = TRUE),
    max_chlorine = max(`Residual Free Chlorine (mg/L)`, na.rm = TRUE),
    mean_chlorine = mean(`Residual Free Chlorine (mg/L)`, na.rm = TRUE),
    min_turbidity = min(as.numeric(`Turbidity (NTU)`), na.rm = TRUE),
    max_turbidity = max(as.numeric(`Turbidity (NTU)`), na.rm = TRUE),
    mean_turbidity = mean(as.numeric(`Turbidity (NTU)`), na.rm = TRUE)
  )

print(summary_stats)

```


## Including Plots
```{r visuals-data2}
#Residual free chlorine:
ggplot(data, aes(x = `Residual Free Chlorine (mg/L)`)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Residual Free Chlorine", x = "Residual Free Chlorine (mg/L)", y = "Frequency")

#Turbidity distribution:
ggplot(data, aes(x = as.numeric(`Turbidity (NTU)`))) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Turbidity", x = "Turbidity (NTU)", y = "Frequency")

#Residual free chlorine versus Turbidity
ggplot(data, aes(x = `Residual Free Chlorine (mg/L)`, y = as.numeric(`Turbidity (NTU)`))) +
  geom_point(alpha = 0.5) +
  labs(title = "Residual Free Chlorine vs. Turbidity", x = "Residual Free Chlorine (mg/L)", y = "Turbidity (NTU)")

```

#Analysis: The chlorine levels in water samples show a typical range with some higher or lower concentrations, while turbidity levels, revealing water clarity, exhibit a similar common range but with some outliers. The scatter plot comparing chlorine to turbidity hints at possible correlations, like higher chlorine corresponding with lower turbidity. Chlorine levels' mean, minimum, and maximum illustrate average concentration and range, and turbidity statistics provide parallel insights into water clarity and quality. This data offers a comprehensive understanding of water quality indicators.

##Dataset 3: Leading Causes of death in NYC
```{r dataset3}
# Load necessary libraries
library(tidyverse)

# Define the URL of the CSV file
url <- "https://github.com/Jomifum/Project2D607/blob/main/New_York_City_Leading_Causes_of_Death_20241016.csv?raw=true"

# Read the data from the URL and rename it as dataset3
dataset3 <- read_csv(url)

# View the first few rows of the renamed dataset
print(head(dataset3))

```


```{r tyding-dataset3}
# Load necessary libraries
library(tidyverse)

# Define the URL of the CSV file
url <- "https://github.com/Jomifum/Project2D607/blob/main/New_York_City_Leading_Causes_of_Death_20241016.csv?raw=true"

# Read the data from the URL
dataset3 <- read_csv(url, col_types = cols(
  `Year` = col_double(),
  `Leading Cause` = col_character(),
  `Sex` = col_character(),
  `Race Ethnicity` = col_character(),
  `Deaths` = col_double(),
  `Death Rate` = col_double(),
  `Age Adjusted Death Rate` = col_double()
))

# Tidy the dataset
dataset3 <- dataset3 %>%
  mutate(
    `Death Rate` = as.numeric(`Death Rate`),
    `Age Adjusted Death Rate` = as.numeric(`Age Adjusted Death Rate`)
  ) %>%
  drop_na() %>%
  rename(
    Year = `Year`,
    Leading_Cause = `Leading Cause`,
    Sex = `Sex`,
    Race_Ethnicity = `Race Ethnicity`,
    Deaths = `Deaths`,
    Death_Rate = `Death Rate`,
    Age_Adjusted_Death_Rate = `Age Adjusted Death Rate`
  ) %>%
  # Separate Race and Ethnicity, ensuring Hispanic is an Ethnicity with Race as Other
  separate(`Race_Ethnicity`, into = c("Race", "Ethnicity"), sep = " ", extra = "merge", fill = "right") %>%
  mutate(
    Ethnicity = ifelse(Race == "Hispanic", "Hispanic", Ethnicity),
    Race = ifelse(Race == "Hispanic", "Other", Race),
    Ethnicity = ifelse(Race == "Asian" & Ethnicity == "and Pacific Islander", "Asian and Pacific Islander", Ethnicity),
    Race = ifelse(Race == "Asian" & Ethnicity == "Asian and Pacific Islander", "Asian", Race)
  )

# Print the tidied data
print(head(dataset3))

# Save the tidied data to a CSV file
write_csv(dataset3, "tidied_data3.csv")

```

#Visualizations for the third dataset
```{r visuals dataset3}

library(ggplot2)
library(dplyr)
library(stringr)
library(viridis)

# Top 10 leading causes of death - Bar Chart
top_causes <- dataset3 %>%
  group_by(Leading_Cause) %>%
  summarise(Total_Deaths = sum(Deaths), .groups = "drop") %>%
  arrange(desc(Total_Deaths)) %>%
  head(10)

# Plot
ggplot(top_causes, aes(x = reorder(Leading_Cause, Total_Deaths), y = Total_Deaths, fill = Leading_Cause)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 10 Leading Causes of Deaths", x = "Leading Cause", y = "Total Deaths") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10), # Increase y-axis text size
    axis.text.x = element_text(size = 10), # Increase x-axis text size
    plot.title = element_text(size = 14)   # Increase title size
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_fill_viridis_d() # Use the viridis color palette for better color handling


# Leading causes of death by race - Dodge Bar Chart
library(stringr)

# Wrapping labels and adjusting text size
causes_by_race <- dataset3 %>%
  group_by(Race, Leading_Cause) %>%
  summarise(Total_Deaths = sum(Deaths), .groups = "drop") %>%
  arrange(desc(Total_Deaths))

ggplot(causes_by_race, aes(x = reorder(Leading_Cause, Total_Deaths), y = Total_Deaths, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Leading Causes of Death by Race", x = "Leading Cause", y = "Total Deaths") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 14)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))

```
