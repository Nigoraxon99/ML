install.packages("PMCMR")
install.packages("PMCMRplus")
library(PMCMR)
library(PMCMRplus)

data <- data.frame(
  Dataset = c(
    "anneal", "Balance-scale", "Breast-cancer", "Breast-w", "Credit-a", "Dermatology", "Diabetes", "Glass", 
    "Heart-c", "Heart-h", "Heart-statlog", "Hepatitis", "Hypothyroid", "Ionosphere", "Kr-vs-kp", "Labor", 
    "Letter", "Lymph", "Primary-tumor", "Sick", "Sonar", "Splice", "Vehicle", "Vote", "Waveform-5000"
  ),
  AlgorithmA = c(
    93.99, 91.36, 71.68, 97.28, 85.94, 97.81, 77.47, 77.1, 85.81, 86.39, 
    83.7, 89.03, 95.68, 92.02, 87.89, 91.23, 72.84, 85.81, 50.15, 97.48, 
    99.04, 95.36, 66.67, 90.11, 64.04
  ),
  AlgorithmB = c(
    76.17, 91.04, 72.38, 95.99, 84.49, 93.17, 69.92, 71.96, 84.49, 83.33, 
    81.85, 79.35, 92.29, 90.31, 79.72, 91.23, 77.71, 81.08, 29.5, 93.88, 
    88.94, 52.04, 69.62, 90.57, 63.92
  ),
  AlgorithmC = c(
    97.77, 89.6, 71.68, 97, 87.54, 98.09, 76.04, 78.97, 84.82, 86.05, 
    84.07, 89.68, 95.86, 93.45, 91.24, 82.46, 85.2, 85.81, 49.56, 97.51, 
    99.04, 96.21, 73.52, 94.25, 64.02
  ),
  AlgorithmD = c(
    97.44, 89.6, 72.03, 97, 87.1,97.81, 76.04, 78.97, 85.48, 85.37, 
    83.7, 89.68, 95.89, 93.45, 91.27, 85.96, 85.2, 86.49, 50.15, 97.38, 
    99.04, 95.67, 73.52, 94.25, 64.02
  ) )

# Convert the data frame to a matrix
data_matrix <- as.matrix(data[, -1])
# Perform the Friedman Test with "Dataset" as groups
friedman.test(data_matrix, data$Dataset)

# Transpose the data to compare algorithms across datasets
data_transposed <- data.frame(
  Algorithm = rep(names(data)[-1], each = nrow(data_matrix)),
  Value = as.vector(t(data_matrix[, -1])),
  Dataset = rep(data$Dataset, times = ncol(data_matrix) - 1)
)

# Perform the Friedman Test
friedman_result <- friedman.test(Value ~ Algorithm | Dataset, data = data_transposed)

# Perform the Nemenyi post-hoc test
nemenyi_result <- frdAllPairsNemenyiTest(friedman_result, data_matrix$Dataset)

# Print the results
print(nemenyi_result)

