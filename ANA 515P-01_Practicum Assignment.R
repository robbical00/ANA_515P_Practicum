#setting directory
getwd()
setwd("/Users/robertocalitri/Desktop")

#installing and loading packages
install.packages("readxl")
library(readxl)
library(dplyr)
install.packages("lubridate")
library(lubridate)
library(ggplot2)


#read dataset (sheet 1 and sheet 2) and assign it to dataframe
sheet1_survey <- read_excel("survey.xlsx", sheet = "Sheet1")
sheet2_survey <- read_excel("survey.xlsx", sheet = "Sheet 2")


#I noticed from the Excel sheet that observation #4 has $41878.5 as a date format . I assume that is an error, therefore I am going to discard that observation
sheet2_survey <- sheet2_survey[-4, ]


## Sheet 2: the date format (Timestamp column) needs to be corrected (e.g., 41879.42 instead of 8/28/2014  10:07:53 AM) 
# Converting numeric values (timestamp) to a datetime object. In this code ''(sheet2_survey$Timestamp - 25569) * 86400'' is responsible
#for converting the timestamp from the Excel date format to Unix timestamp format
sheet2_survey$Timestamp <- as_datetime(
  (sheet2_survey$Timestamp - 25569) * 86400,
  origin = "1970-01-01",
  tz = "UTC"
)

#Combining two sheets together
combined_sheets <- rbind(sheet1_survey, sheet2_survey)

#Since this dataset has been labeled as a "2014 survey", I want to include only observations from 2014
survey_2014 <- combined_sheets[year(combined_sheets$Timestamp) %in% c(2014), ]


#include only workers with an age between 18 and 100
survey_2014 <- survey_2014[survey_2014$Age >= 18 & survey_2014$Age <= 100, ]

#Standardize Gender Values_Male
survey_2014$Gender <- ifelse(tolower(survey_2014$Gender) %in% c("male", "m", "Male-ish", "Mail", "Maile", "cis man", "male (cis)", "cis male"), "Male", survey_2014$Gender)

#Standardize Gender Values_Female
survey_2014$Gender <- ifelse(tolower(survey_2014$Gender) %in% c("female", "f", "woman", "cis female", "female (trans)", "cis-female/femme", "femake", "female (cis)", "trans woman", "trans-female", "femail"), "Female", survey_2014$Gender)

#discard observations that are not either Male ir Female
clean_1 <- survey_2014[survey_2014$Gender %in% c("Male", "Female"), ]

#Replace 'US' with 'United States' for the column Country
clean_1$Country <- gsub("US", "United States", clean_1$Country)

##Replace 'UK' with 'United Kingdom' for the column Country
clean_1$Country <- gsub("UK", "United Kingdom", clean_1$Country)

#Family History - include only values 'Yes' or 'No'
clean_2 <- clean_1[clean_1$family_history %in% c("Yes", "No"), ]

#Treatment - include only values 'Yes' or 'No'
clean_3 <- clean_2[clean_2$treatment %in% c("Yes", "No"), ]

#Work_interfere - inlcude only certain values
clean_4 <- clean_3[clean_3$work_interfere %in% c("Never", "Sometimes", "NA", "Often", "Rarely"), ]

#remote_work - inlcude only yes or No
clean_5 <- clean_4[clean_4$remote_work %in% c("Yes", "No"), ]

#tech_company - inlcude only yes or No
clean_6 <- clean_5[clean_5$tech_company %in% c("Yes", "No"), ]

#benefits - inlcude only certain values
clean_7 <- clean_6[clean_6$benefits %in% c("Yes", "No", "Don't know"), ]

#care_options convert "not sure" to "don't know" in order to have same values across variables
clean_7$care_options <- gsub("not sure", "Don't know", clean_7$care_options, ignore.case = TRUE)

#seek_help convert standardizing 'not sure' values in order to have same values across variables
clean_7$seek_help <- gsub("not sure|Not sure", "Don't know", clean_7$seek_help, ignore.case = TRUE)

#anonimity convert "not sure" to "don't know" in order to have same values across variables
clean_7$anonymity <- gsub("not sure", "Don't know", clean_7$anonymity, ignore.case = TRUE)

#self_employed - inlcude only certain values
clean_8 <- clean_7[clean_7$self_employed %in% c("Yes", "No"), ]


#At this point, the data have been cleaned and are ready to be analyzed/graphed
write.csv(clean_8, file = "Final_Result.csv", row.names = FALSE)


#histogram showing the distribution of the variable age in tech workplace
ggplot(clean_8, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Age Distribution in Tech Workplace", x = "Age", y = "Frequency")

#barplot showing the distribution of gender in tech workplace
ggplot(clean_8, aes(x = Gender)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Gender in Tech Workplace", x = "Gender", y = "Count")


#clustered bar chart showing the relationship between Gender and Treatment
gender_vs_treatment <- table(clean_8$Gender, clean_8$treatment)
barplot(gender_vs_treatment, beside = TRUE, legend.text = TRUE, col = c("lightblue", "lightgreen"), 
        main = "Gender vs. Drug Treatment", xlab = "Gender", ylab = "Count")

#clustered bar chart showing the relationship between being Self-employed and Treatment
selfemployed_vs_gender <- table(clean_8$self_employed, clean_8$Gender)
barplot(selfemployed_vs_gender, beside = TRUE, legend.text = TRUE, col = c("lightblue", "lightgreen"), 
        main = "Self-employed vs. Gender", xlab = "Gender", ylab = "Count")

#grouped bar chart showing Correlation Between Number of Employees and Care Options Offered
ggplot(clean_8, aes(x = no_employees, fill = care_options)) +
  geom_bar(position = "dodge") +
  labs(title = "Correlation Between Number of Employees and Care Options Offered",
       x = "Number of Employees",
       y = "Count") +
  scale_fill_manual(values = c("Yes" = "green", "No" = "red", "Don't know" = "blue"))


#I want to show the relationship between 'employee under treatment' in US vs UK
#Filtering for self-employed individuals in the US and UK
filtered <- clean_8 %>%
  filter(self_employed == "Yes" & (`Country` %in% c("United States", "United Kingdom")))
#count the number of self-employed individuals
summary_data <- filtered %>%
  group_by(Country) %>%
  summarize(SelfEmployedCount = n())
#Creating barchart
ggplot(summary_data, aes(x = Country, y = SelfEmployedCount, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Self-Employed Individuals in the US vs United Kingdom",
       x = "Country",
       y = "Count") +
  scale_fill_manual(values = c("United States" = "blue", "United Kingdom" = "red"))




