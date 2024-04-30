set.seed(42)

# Simulating Type 1 error rate with multiple datasets
num_datasets <- 1000
results <- vector("numeric", num_datasets)

for (i in 1:num_datasets) {
  groep1 <- rnorm(20, mean = 6, sd = 2)
  groep2 <- rnorm(20, mean = 8, sd = 1.2)
  
  result <- t.test(groep1, groep2)$p.value
  results[i] <- result
}

# Calculating Type 1 error rate
type1_error_rate <- sum(results < 0.05) / num_datasets
cat("Type 1 Error Rate:", type1_error_rate, "\n")

# Simulating Type 1 error rate with different means (null hypothesis is not true)
num_datasets <- 1000
results <- vector("numeric", num_datasets)

for (i in 1:num_datasets) {
  groep1 <- rnorm(20, mean = 6, sd = 2)
  groep2 <- rnorm(20, mean = 6, sd = 1.2) # Same mean for both groups
  
  result <- t.test(groep1, groep2)$p.value
  results[i] <- result
}

# Calculating Type 1 error rate
type1_error_rate <- sum(results < 0.05) / num_datasets
cat("Type 1 Error Rate (Different Means):", type1_error_rate, "\n")

# Simulating outlier removal with correct implementation
set.seed(123)
gegevens1 <- c(rnorm(81, mean = 5, sd = 2.5), rnorm(5, mean = 20, sd = 2))
gegevens2 <- c(rnorm(80, mean = 8, sd = 7))

# Calculate the z-score for each observation
z_scores <- abs(scale(gegevens1))

# Remove outliers based on a predetermined threshold (e.g., z-score <= 3)
nieuwe_gegevens1 <- gegevens1[z_scores <= 3]

# Boxplot of the new dataset without outliers
boxplot(nieuwe_gegevens1, gegevens2)

# T-test with the new dataset
t_test_result <- t.test(nieuwe_gegevens1, gegevens2)
cat("T-test p-value (without outliers):", t_test_result$p.value, "\n")
