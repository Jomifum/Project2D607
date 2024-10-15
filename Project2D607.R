
#title: "Project 2"
#author: "Jose Fuentes"
#date: "2024-10-14"


## Project 2 DATA 607: Analysis of HIV/AIDS Diagnoses by Neighborhood, Sex, and Race/Ethnicity in NYC

#Introduction
#This is the analysis of dataset on HIV and AIds diagnoses in New York City, covering 2010 to 2021
#that reveals critical trends and insights showing key years peaks and fluctuations, 
#the data provides valuable context for understanding the impact of HIV across different communities, 
#how this virus affects more neighborhoods than other, sex gender, races.
  

# Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)



#Defining file path and steps to tidy the dataset

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


## Including Plots
#1)HIV Diagnosis Rates by Gender and Race/Ethnicity 

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


#**Temporal Trends: Number of HIV and AIDS Diagnoses Over the Years**
  
  
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

#**Year with most HIV diagnoses**

# Calculate the total HIV diagnoses per year
hiv_by_year <- dataset %>%
  group_by(Year) %>%
  summarise(total_hiv_diagnoses = sum(Total_HIV_Diagnoses, na.rm = TRUE))

# Find the year with the most HIV diagnoses
year_most_hiv <- hiv_by_year %>%
  filter(total_hiv_diagnoses == max(total_hiv_diagnoses))

year_most_hiv

#**HIV Diagnoses by Neighborhood (Percentage)**

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


#**Geographical Patterns: HIV/AIDS Diagnoses by Neighborhood**
  
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

#**Intersectional Analysis: HIV Diagnoses by Gender and Race/Ethnicity**
 
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

#**Relation newly HIV and Aids diagnosis**
  
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

# Statistics Summary:

# Get a statistical summary of all numeric columns in the dataset
summary(dataset)


#**Interpreting the summary this dataset reveals significant variability in HIV and AIDS diagnoses across different neighborhoods. Many areas report no diagnoses  looking both total and per 100,000 population, with the median values indicating that over half the neighborhoods have very low diagnosis rates. However, the mean values are higher, suggesting some neighborhoods experience disproportionately high diagnoses, skewing the data. Now regarding concurrent HIV/AIDS diagnoses are rare, with most neighborhoods reporting none. Notably, the dataset contains missing values, particularly in the proportions of concurrent diagnoses, this suggests varying levels of impact across regions, with certain areas facing more significant health challenges than others.**
  
  #Conclusion
#**The dataset on HIV diagnoses in New York City, spanning from 2010 to 2021, highlights significant
#* trends. Key years include a peak of over 35,136 diagnoses in 2020, with the earliest data 
#* from 2010, a median year of 2017, and the most recent in 2021. With 12,256 entries, the dataset 
#* encompasses categorical variables like Borough, Neighborhood, Sex, and Ethnicity. This comprehensive 
#* data aids in analyzing trends, showing that the most affected population is cisgender males, 
#* although their sexual orientation is unknown due to the absence of this attribute in the dataset. This information can help inform and create better HIV prevention programs such as PrEP and improve understanding of the spread and impact of HIV across different communities. It emphasizes the importance of adherence to antiretroviral therapy to prevent further infections, offering valuable insights for public health strategies. Another challenge in the analysis was the lack of differentiation between neighborhoods and their respective counties in the initial dataset.**