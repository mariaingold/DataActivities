# UoEO MSc AI: Numerical Analysis
# Coursework data activities using Health Data dataset
# Maria Ingold (2024)
# Data Activity 5, 6: Health Data

# Import packages
library(tidyverse)
library(haven) # To read sav
library(dplyr) # For data manipulation (pull)
library(moments) # For skewness and kurtosis

# Load the data
health_data <- read_sav("health_data.sav")

# Display information about the dataset
View(health_data) # Display the dataset
names(health_data) # Display the names of the variables in the dataset
glimpse(health_data) # Display the structure of the dataset
summary(health_data) # Summary statistics for all variables

#FUNCTIONS

# Mode function
mode <- function(x) {
  ux <- unique(x) # Eliminate duplicates
  tab <- tabulate(match(x, ux)) # Create frequency table
  ux[tab == max(tab)] # Return mode(s)
}

################################################################
# DATA ACTIVITY 5
################################################################

# DATA ACTIVITY 5.1: Find mean, median and mode of sbp, dbp and income
?mean
?median
?table

# sbp
mean(health_data$sbp, na.rm = TRUE) # Mean of sbp
median(health_data$sbp, na.rm = TRUE) # Median of sbp
table(health_data$sbp) # Frequency table for sbp
mode(health_data$sbp) # Mode of sbp

# dbp
mean(health_data$dbp, na.rm = TRUE) # Mean of dbp
median(health_data$dbp, na.rm = TRUE) # Median of dbp
table(health_data$dbp) # Frequency table for dbp
mode(health_data$dbp) # Mode of dbp

# income
mean(health_data$income, na.rm = TRUE) # Mean of income
median(health_data$income, na.rm = TRUE) # Median of income
table(health_data$income) # Frequency table for income
mode(health_data$income) # Mode of income

# DATA ACTIVITY 5.2: Create five-figure summary for income and present it using a boxplot
# Five figure summary
# These calculate quartiles differently
fivenum(health_data$income, na.rm = TRUE) # Five-figure summary
summary(health_data$income) # Alternate five-figure summary

# Boxplot
health_data %>%
  ggplot(aes(y = income)) +
  geom_boxplot(fill = "yellow", outlier.color = "red", na.rm = TRUE) +
  theme_bw() +
  labs(title = "Income levels 'income'",
       y = "income: higher score is higher income") +
  scale_x_continuous(breaks = NULL) # Remove x-axis labels

# DATA ACTIVITY 5.3: Run a hypothesis test
# Determine if any association between systolic blood pressure (sbp)
# and absence of peptic ulcer (pepticuler) using a t-test
# H0: There is no difference in sbp between those with and without peptic ulcer
# HA: There is a difference in sbp between those with and without peptic ulcer

# Two-sample t-test: compare means of two groups
# Create two groups: with and without peptic ulcer
# With ulcer. 1 = Yes for Have peptic ulcer
sbp_with_ulcer <- health_data %>%
  filter(pepticulcer == 1) %>%
  pull(sbp)
sbp_with_ulcer

#Without ulcer. 2 = No for Have peptic ulcer
sbp_without_ulcer <- health_data %>%
  filter(pepticulcer == 2) %>%
  pull(sbp)
sbp_without_ulcer

# Run t-test
t.test(sbp_with_ulcer, sbp_without_ulcer)

# RESULT
# Welch Two Sample t-test
#
# data:  sbp_with_ulcer and sbp_without_ulcer
# t = 1.2142, df = 57.562, p-value = 0.2296
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -2.889367 11.795703
# sample estimates:
# mean of x mean of y
# 131.3171  126.8639

# INTERPRETATION
# p-value is 0.2296 which is greater than 0.05
# ==> we fail to reject the null hypothesis
# 95% confidence interval: 95% confident that difference in means in that range.
# Confidence interval for difference in means ranges: -2.889367 to 11.795703
# The 95% confidence interval includes 0
# ==> difference in means could be 0 => No difference.
# ==> fail to reject the null hypothesis
# t-value is 1.2142 which is less than 1.96, so outside the critical region.
# ==> t-value is also small. Fail to reject the null hypothesis.
# No significant difference in systolic blood pressure between those with and without peptic ulcer.

################################################################
# DATA ACTIVITY 6
################################################################

# DATA ACTIVITY 6.1: Find mean, median and mode of age
# age (quantitative)
mean(health_data$age, na.rm = TRUE) # Mean of age
median(health_data$age, na.rm = TRUE) # Median of age
table(health_data$age) # Frequency table for age
mode(health_data$age) # Mode of age

# DATA ACTIVITY 6.2: Find out whether median diastolic blood pressure
# is same among diabetic and non-diabetic participants
#
# Compares two groups
# Independent samples (diabetic and non-diabetic cannot be same person)
# Asks for median
# Independent variable: diabetes
# Dependent variable: dbp
# ==> Mann-Whitney U test
# H0: No difference in dbp between diabetic and non-diabetic
# HA: Is a difference in dbp between diabetic and non-diabetic

# dbp (quantitative - continuous)
summary(health_data$dbp) # dbp summary

# diabetes (categorical): Create two groups: dbp diabetic and dbp non-diabetic
# Diabetic. 1 = Yes for Have diabetes
# Non-diabetic. 2 = No for Have diabetes

# Diabetic dbp
dbp_diabetic <- health_data %>%
  filter(diabetes == 1) %>%
  pull(dbp) # Pull out dbp from those who are diabetic
dbp_diabetic
summary(dbp_diabetic)

# Non-diabetic dbp
dbp_non_diabetic <- health_data %>%
  filter(diabetes == 2) %>%
  pull(dbp) # Pull out dbp from those who are non-diabetic
dbp_non_diabetic
summary(dbp_non_diabetic)

# Mann-Whitney U test aka Wilcoxon rank sum test
wilcox.test(dbp_diabetic, dbp_non_diabetic)

# RESULT
# Wilcoxon rank sum test with continuity correction
#
# data:  dbp_diabetic and dbp_non_diabetic
# W = 3804.5, p-value = 0.7999
# alternative hypothesis: true location shift is not equal to 0

# INTERPRETATION
# p-value is 0.7999 which is greater than 0.05
# ==> we fail to reject the null hypothesis
# No significant difference in median diastolic blood pressure between diabetic and non-diabetic participants.

# DATA ACTIVITY 6.3: Find out whether systolic BP is different across occupation groups
#
# occupation = 1 = GOVT JOB, 2 = PRIVATE JOB, 3 = BUSINESS, 4 = OTHERS
# Compares more than two groups
# Groups are independent
# Dependent variable: sbp (quantitative - continuous)
# Independent variable: occupation (categorical)
# NOT sure if it is normally distributed (which would be ANOVA)
# If not normally distributed (Kruskal-Wallis test)

# Create four groups: sbp for each occupation group
# Govt job
sbp_govt_job <- health_data %>%
  filter(occupation == 1) %>%
  pull(sbp) # Pull out sbp from those who have govt job
sbp_govt_job
length(sbp_govt_job)

# Private job
sbp_private_job <- health_data %>%
  filter(occupation == 2) %>%
  pull(sbp) # Pull out sbp from those who have private job
sbp_private_job
length(sbp_private_job)

# Business
sbp_business <- health_data %>%
  filter(occupation == 3) %>%
  pull(sbp) # Pull out sbp from those who have business
sbp_business
length(sbp_business)

# Others
sbp_others <- health_data %>%
  filter(occupation == 4) %>%
  pull(sbp) # Pull out sbp from those who have other occupation
sbp_others
length(sbp_others)

# Check for normal distribution

# 1. DESCRIPTIVE TESTS
# Check mean and median--significant difference indicates skewness
summary(sbp_govt_job)
summary(sbp_private_job)
summary(sbp_business)
summary(sbp_others)

#RESULT
# > summary(sbp_govt_job)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   97.0   116.0   125.5   129.4   142.0   190.0
# > summary(sbp_private_job)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  95.0   110.0   120.0   126.3   145.0   174.0
# > summary(sbp_business)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   91.0   111.0   122.0   127.9   140.0   195.0
# > summary(sbp_others)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   100     114     123     127     140     176

# INTERPRETATION
# Mean and median are close for all groups, but are they close enough?
# Mean to right of median means right skewness
# Mean to left of median means left skewness
# Govt job: Median = 125.5 < Mean = 129.4
# Private job: Median = 120 < Mean = 126.3
# Business: Median = 122 < Mean = 127.9
# Others: Median = 123 < Mean = 127
# All means are to the right of the median
# ==> All groups are right skewed

# Check Skewness
# Skewness: 0 = normal, < 0 = left skewed (negative), > 0 = right skewed (positive)
skewness(sbp_govt_job)
skewness(sbp_private_job)
skewness(sbp_business)
skewness(sbp_others)

# RESULT
# > skewness(sbp_govt_job)
# [1] 0.7038924
# > skewness(sbp_private_job)
# [1] 0.4706138
# > skewness(sbp_business)
# [1] 0.974221
# > skewness(sbp_others)
# [1] 0.6910061

# INTERPRETATION
# All groups are right skewed,

# Check Kurtosis
# Kurtosis: = 3 = normal, < 3 = light tails, > 3 = heavy tails
kurtosis(sbp_govt_job)
kurtosis(sbp_private_job)
kurtosis(sbp_business)
kurtosis(sbp_others)

# RESULT
# > kurtosis(sbp_govt_job)
# [1] 3.559904
# > kurtosis(sbp_private_job)
# [1] 2.239651
# > kurtosis(sbp_business)
# [1] 3.880541
# > kurtosis(sbp_others)
# [1] 2.921877

#INTERPRETATION
# Two are greater than 3 (heavy tails)
# Two are less than 3 (light tails)

# 2. VISUAL ASSESSMENT

# Q-Q plot
data.frame(sbp_govt_job) %>%
  ggplot(aes(sample = sbp_govt_job)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("QQ plot for sbp_govt_job")

data.frame(sbp_private_job) %>%
  ggplot(aes(sample = sbp_private_job)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("QQ plot for sbp_private_job")

data.frame(sbp_business) %>%
  ggplot(aes(sample = sbp_business)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("QQ plot for sbp_business")

data.frame(sbp_others) %>%
  ggplot(aes(sample = sbp_others)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("QQ plot for sbp_others")

# INTERPRETATION
# While the QQ plot is mostly linear, there are some deviations at the tails
# These go up at both ends. I think this is showing right skewed.

# Histogram
data.frame(sbp_govt_job) %>%
  ggplot(aes(x = sbp_govt_job)) +
  geom_histogram(bins = 10, fill = "grey", color = "black") +
  theme_bw() +
  ggtitle("Histogram for sbp_govt_job")

data.frame(sbp_private_job) %>%
  ggplot(aes(x = sbp_private_job)) +
  geom_histogram(bins = 10, fill = "grey", color = "black") +
  theme_bw() +
  ggtitle("Histogram for sbp_private_job")

data.frame(sbp_business) %>%
  ggplot(aes(x = sbp_business)) +
  geom_histogram(bins = 10, fill = "grey", color = "black") +
  theme_bw() +
  ggtitle("Histogram for sbp_business")

data.frame(sbp_others) %>%
  ggplot(aes(x = sbp_others)) +
  geom_histogram(bins = 10, fill = "grey", color = "black") +
  theme_bw() +
  ggtitle("Histogram for sbp_others")

# INTERPRETATION
# Histograms do not look particularly normal.

# 3. Statistical tests
# Shapiro-Wilk test is for small samples (n < 50)
# Kolmogorov-Smirnov test is for large samples (n > 50)
# As the count of each group ranges between 49 and 60
# Try both tests

# Shapiro-Wilk test
# Test of normality - assumes normally distributed
# H0: Data is normally distributed
# HA: Data is not normally distributed
# p-value < 0.05 => reject H0
# p-value > 0.05 => fail to reject H0
shapiro.test(sbp_govt_job)
shapiro.test(sbp_private_job)
shapiro.test(sbp_business)
shapiro.test(sbp_others)

# RESULTS
# Shapiro-Wilk normality test
#
# data:  sbp_govt_job
# W = 0.95197, p-value = 0.01929
#
# data:  sbp_private_job
# W = 0.94361, p-value = 0.02049
#
# data:  sbp_business
# W = 0.93249, p-value = 0.00761
#
# data:  sbp_others
# W = 0.94677, p-value = 0.0213

# INTERPRETATION
# p-values are all less than 0.05
# ==> reject the null hypothesis
# ==> data is not normally distributed

# Kolmogorov-Smirnov test
# Non-parametric test for normality - no assumptions about the distribution
# H0: Data is normally distributed
# HA: Data is not normally distributed
# p-value < 0.05 => reject H0
# p-value > 0.05 => fail to reject H0

ks.test(sbp_govt_job, "pnorm")
ks.test(sbp_govt_job, "pnorm", mean = mean(sbp_govt_job), sd = sd(sbp_govt_job))

# RESULTS
# > ks.test(sbp_govt_job, "pnorm")
#
# Asymptotic one-sample Kolmogorov-Smirnov test
#
# data:  sbp_govt_job
# D = 1, p-value < 2.2e-16
# alternative hypothesis: two-sided
#
# Warning message:
# In ks.test.default(sbp_govt_job, "pnorm") :
# ties should not be present for the Kolmogorov-Smirnov test
# > ks.test(sbp_govt_job, "pnorm", mean = mean(sbp_govt_job), sd = sd(sbp_govt_job))
#
# Asymptotic one-sample Kolmogorov-Smirnov test
#
# data:  sbp_govt_job
# D = 0.08654, p-value = 0.7599
# alternative hypothesis: two-sided
#
# Warning message:
# In ks.test.default(sbp_govt_job, "pnorm", mean = mean(sbp_govt_job),  :
# ties should not be present for the Kolmogorov-Smirnov test

# INTERPRETATION
# By default mean = 0 and standard deviation = 1
# But we are testing against a normal distribution of itself?
# This test seems problematic.
# It warns that "ties should not be present"
# This means that there are duplicate values in the data which it doesn't handle well
# Also, when setting the mean and deviation, it returns a p-value of 0.7599
# Which fails to reject the null hypothesis that the data is normally distributed
# ==> This test is not appropriate for this data

# NOT NORMALLY DISTRIBUTED
# ==> Kruskal-Wallis test
# H0: No difference in systolic blood pressure (median?) between occupation groups
# HA: (At least one?) Difference in systolic blood pressure between occupation groups
# p-value < 0.05 => reject H0
# p-value > 0.05 => fail to reject H0

kruskal.test(list(sbp_govt_job, sbp_private_job, sbp_business, sbp_others))

# RESULT
# Kruskal-Wallis rank sum test
# data:  list(sbp_govt_job, sbp_private_job, sbp_business, sbp_others)
# Kruskal-Wallis chi-squared = 0.77906, df = 3, p-value = 0.8545

# INTERPRETATION
# p-value is 0.8545 which is greater than 0.05
# ==> fail to reject the null hypothesis
# No significant difference in systolic blood pressure between occupation groups
