# UoEO MSc AI: Numerical Analysis
# Coursework data activities using Crime Survey dataset
# Maria Ingold (2024)

# Import packages
library(tidyverse) # For data manipulation
library(haven) # For reading SPSS files and as_factor
library(psych) # For describe function

# Load the data
crime_survey <- read_sav("csew1314teachingopen.sav")
View(crime_survey)

# Display a bit about the whole dataset
names(crime_survey) # Display the names of the variables in the dataset
glimpse(crime_survey) # Display the structure of the dataset
print(crime_survey) # Display the dataset
summary(crime_survey) # Summary statistics for all variables
n_count <- nrow(crime_survey) # Number of rows in the dataset

# DATA ACTIVITY 1.2: Create summary statistic for antisocx variable
print(attr(crime_survey$antisocx, "label")) # Display antisocx label
summary(crime_survey$antisocx) # Summary statistics for antisocx
describe(crime_survey$antisocx) # Descriptive statistics for antisocx

# DATA ACTIVITY 2.1: Crime in 12 months prior to survey for bcsvictim
print(attr(crime_survey$bcsvictim, "label")) # Display bcsvicitim label
print(attr(crime_survey$bcsvictim, "labels")) # Display bcsvictim format
summary(crime_survey$bcsvictim) # Summary statistics for bcsvictim variable
describe(crime_survey$bcsvictim) # Descriptive statistics for bcsvictim
victims <- sum(crime_survey$bcsvictim == 1) # Number victims in 12 months prior
print(paste("Victims : ", victims, " out of: ", n_count)) # Display number

# DATA ACTIVITY 2.2: Create bcsvictim frequency table
table(crime_survey$bcsvictim) # Frequency table for bcsvictim variable

# DATA ACTIVITY 2.3: Use bcsvictim lables using as_factor
table(as_factor(crime_survey$bcsvictim))

# DATA ACTIVITY 3: Subset of 75+ who were bcsvictims
print(attr(crime_survey$agegrp7, "label")) # Display agegrp7 label
print(attr(crime_survey$agegrp7, "labels")) # Display agegrp7 labels
subset_75plusvictims <- crime_survey %>%
  filter(agegrp7 == 7 & bcsvictim == 1)
print(paste("Victims 75+ : ", nrow(subset_75plusvictims), " out of: ", n_count))

# DATA ACTIVITY 4.1: Boxplot for antisocx variable
ggplot(crime_survey, aes(y = antisocx)) +
  geom_boxplot(fill = "yellow", outlier.color = "red", na.rm = TRUE) +
  labs(title = "Levels of anti-social behaviour in neighbourhood 'antisocx'",
       y = "antisocx: higher score is higher level") +
  scale_x_continuous(breaks = NULL) # Remove x-axis labels