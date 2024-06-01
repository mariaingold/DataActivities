# UoEO MSc AI: Numerical Analysis
# Coursework data activities using Health Data dataset
# Maria Ingold (2024)
# Unit 8: Scenario-Based Exercise = 95% Confidence Interval

library(tidyverse)
library(reshape2)   # For melt
library(car)        # For leveneTest

# DATA

# Vector of scores for 3 vendors
vendor1 <- sort(c(45, 29, 56, 52, 45, 45, 41))
vendor2 <- sort(c(61, 53, 41, 58, 53, 47, 44))
vendor3 <- sort(c(35, 21, 33, 27, 22, 26, 30))

# Dataframe (puts vectors into 3 columns with each vendor in a column)
vendor_scores_df <- data.frame(vendor1, vendor2, vendor3)

# Reshape dataframe to long (2 columns with vendor and score in each row)
# This is needed for box plotting
df_long <- melt(vendor_scores_df, variable.name = "Vendor", value.name = "Score")

# DESCRIPTIVE STATISTICS

# Print ordered data
vendor1 # 29, 41, 45, 45, 45, 52, 56
vendor2 # 41, 44, 47, 53, 53, 58, 61
vendor3 # 21, 22, 26, 27, 30, 33, 35

# Summary statistics (includes mean, median, min, max, quartiles)
# Find the mean efficiency for each vendor
summary(vendor1)
summary(vendor2)
summary(vendor3)

# Find the mean efficiency for each vendor
# Outcome: The mean scores are different for each vendor.
# Vendor 1: 44.71  But vendor 1 has a low outlier of 29.
# Vendor 2: 51.    This is the highest mean score.
# Vendor 3: 27.71. This is the lowest mean score.
mean(vendor1)
mean(vendor2)
mean(vendor3)

# VISUALISATION

# Boxplot
# Outcome: The median scores are different for each vendor.
# Vendor 2 has the highest median score.
# Vendor 3 has the lowest median score.
df_long %>%
  ggplot(aes(x = Vendor, y = Score, fill = Vendor)) +
  geom_boxplot() +
  labs(title = "Vendor Scores",
       x = "Vendor",
       y = "Score") +
  theme_bw() +
  theme(legend.position = "none")

# Histogram
# The number of bins really makes a difference.
# Range is 21 to 61. 
# 8 bins seems to be the best.
# Outcome: They are partly overlaid over each other. 
df_long %>%
  ggplot(aes(x = Score, fill = Vendor)) +
  geom_histogram(bins = 8, alpha = 0.5, position = "identity") +
  labs(title = "Vendor Scores",
       x = "Score",
       y = "Frequency") +
  theme_bw()

# Scatterplot
# Outcome: Definitely not related
# This doesn't seem that helpful
df_long %>%
  ggplot(aes(x = Vendor, y = Score, color = Vendor)) +
  geom_point() +
  labs(title = "Vendor Scores",
       x = "Vendor",
       y = "Score") +
  theme_bw()

# QQ plot
# Curves up at bottom and down at top
# Outcome: The scores are not normally distributed?
df_long %>%
  ggplot(aes(sample = Score)) +
  geom_qq() +
  geom_qq_line()
  labs(title = "QQ Plot of Vendor Scores",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_bw()

# STATISTICAL TEST
# Small sample size
# Shapiro test for normality
# Per vendor to test if single vendor is normally distributed
# Null hypothesis: The data is normally distributed
# Alternative hypothesis: The data is not normally distributed
# vendor 1: p-value = 0.4728 > 0.05 ==> Fail to reject null, not significant
# vendor 2: p-value = 0.7896 > 0.05 ==> Fail to reject null, not significant
# vendor 3: p-value = 0.7426 > 0.05 ==> Fail to reject null, not significant
# This indicates that the data is normally distributed.
shapiro.test(vendor1)
shapiro.test(vendor2)
shapiro.test(vendor3)

# Observations are independent
# But still don't know if variances are equal
# So can't yet confirm ANOVA
# Levene's test for homogeneity of variance
# Null hypothesis: The variances are equal
# Alternative hypothesis: The variances are not equal
# Pr(>F) is p-value = 0.8075 > 0.05 ==> Fail to reject null, not significant
# ===> The variances are equal
leveneTest(Score ~ Vendor, data = df_long)

# INTERPRETATION
# The data is normally distributed 
# and the variances are equal
# and the data is independent
# ==> ANOVA can be used

# HYPOTHESIS TESTING
# Determine which vendor shows a higher significance of improvement
# in employee efficiency statistically at the 95% level.

# ANOVA test
# Null hypothesis: The means of the scores are equal for all vendors
# Alternative hypothesis: The means of the scores are not equal for all vendors
anova_test <- aov(Score ~ Vendor, data = df_long)
anova_test
summary(anova_test) # Have to use summary to get the p-value

# RESULT
# ANOVA test
# Terms:
#                   Vendor Residuals
# Sum of Squares  2031.7143  930.8571
# Deg. of Freedom         2        18
#
# Residual standard error: 7.191265
# Estimated effects may be unbalanced
#
# Summary of ANOVA test
#             Df Sum Sq Mean Sq F value   Pr(>F)
# Vendor       2 2031.7  1015.9   19.64 2.98e-05 ***
# Residuals   18  930.9    51.7
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# INTERPRETATION
# The p-value is 2.98e-05 < 0.05 with *** indicating highly significant
# Reject the null hypothesis, the means of the scores are not equal for all vendors
# ==> At least one vendor is significantly different from the others

# POST-HOC TEST
# Determine which vendor is significantly different from the others
# Tukey's HSD test
# Null hypothesis: The means of the scores are equal for all vendors
# Alternative hypothesis: The means of the scores are not equal for all vendors
TukeyHSD(anova_test)

# RESULT
# Tukey multiple comparisons of means
# 95% family-wise confidence level
#
# Fit: aov(formula = Score ~ Vendor, data = df_long)
#
# $Vendor
#                       diff        lwr        upr     p adj
# vendor2-vendor1   6.285714  -3.524527  16.095955 0.2570011
# vendor3-vendor1 -17.000000 -26.810241  -7.189759 0.0009128
# vendor3-vendor2 -23.285714 -33.095955 -13.475473 0.0000286

# INTERPRETATION
# p adj are the p-values adjusted for multiple comparisons
#
# vendor2-vendor1: p-value = 0.2570011 > 0.05
# ==> Fail to reject the null hypothesis that the means of all scores are equal
# ==> The means of the scores are equal for vendor 1 and vendor 2
#
# vendor3-vendor1: p-value = 0.0009128 < 0.05
# vendor3-vendor2: p-value = 0.0000286 < 0.05
# ==> Reject the null hypothesis that the means of all scores are equal
# ==> The means of the scores are not equal
# At -17.00, Vendor 1 has a signficantly higher mean score than vendor 3
# At -23.29, Vendor 2 has a signficantly higher mean score than vendor 3
# But can't statistically say which is better between vendor 1 and vendor 2

# plot confidence intervals
old_mar <- par("mar") # old margins
par(mar = c(5, 8, 4, 2) + 0.1) # new: bottom, left, top, right
plot(TukeyHSD(anova_test, conf.level=.95), las = 2, col = "blue")
par(mar = old_mar)

# Kruskal-Wallis test
# Just for fun, let's try a non-parametric test
kruskal.test(Score ~ Vendor, data = df_long)