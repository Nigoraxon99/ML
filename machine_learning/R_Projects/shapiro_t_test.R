# I created a data frame with the data differences between AlgorithmA and AlgorithmB
differences <- c(93.99 - 76.17, 91.36 - 91.04, 71.68 - 72.38, 97.28 - 95.99, 85.94 - 84.49, 97.81 - 93.17, 77.47 - 69.92, 77.1 - 71.96, 85.81 - 84.49, 86.39 - 83.33, 83.7 - 81.85, 89.03 - 79.35, 95.68 - 92.29, 92.02 - 90.31, 87.89 - 79.72, 91.23 - 91.23, 72.84 - 77.71, 85.81 - 81.08, 50.15 - 29.5, 97.48 - 93.88, 99.04 - 88.94, 95.36 - 52.04, 66.67 - 69.62, 90.11 - 90.57, 64.04 - 63.92)

# Shapiro-Wilk test
shapiro.test(differences)

# Data for Algorithm A and Algorithm B
algorithm_A <- c(93.99, 91.36, 71.68, 97.28, 85.94, 97.81, 77.47, 77.1, 85.81, 86.39, 83.7, 89.03, 95.68, 92.02, 87.89, 91.23, 72.84, 85.81, 50.15, 97.48, 99.04, 95.36, 66.67, 90.11, 64.04)
algorithm_B <- c(76.17, 91.04, 72.38, 95.99, 84.49, 93.17, 69.92, 71.96, 84.49, 83.33, 81.85, 79.35, 92.29, 90.31, 79.72, 91.23, 77.71, 81.08, 29.5, 93.88, 88.94, 52.04, 69.62, 90.57, 63.92)

# Paired t-test
t.test(algorithm_A, algorithm_B, mu = 0, alternative = "two.sided", paired = TRUE)

# Wilcoxon Signed-Rank Test
wilcox.test(algorithm_A, algorithm_B, mu = 0, alternative = "two.sided", paired = TRUE)

# Sign Test (Binomial test)
binom.test(sum(differences > 0), length(differences), p = 0.5)

# (1) The null hypothesis (H0) is that there is no significant difference between Algorithm A and Algorithm B.
# (2) I rejected null hypothesis
# (3) The p-value is less than α (α = 0.05), so I rejected the null hypothesis.