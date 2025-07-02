# resampling_correlation.R
# Title: Bootstrap and Jackknife Estimation of Correlation Coefficient
# Author: Surath Chakraborti
# Description: This script demonstrates resampling methods (bootstrap and jackknife)
# to estimate the correlation coefficient in a bivariate normal distribution.

# --- Setup ---
library(MASS)
set.seed(1)

# --- Simulate Bivariate Normal Data (n = 10) ---
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.3, 0.3, 1), ncol = 2)
n <- 10
bivariate_data <- as.data.frame(mvrnorm(n = n, mu = mu, Sigma = Sigma))

# --- Sample Estimates ---
sample_mean <- c(mean(bivariate_data$V1), mean(bivariate_data$V2))
sample_cov <- cov(bivariate_data)
sample_cor <- cor(bivariate_data)[1, 2]

# --- Bootstrap ---
B <- 1000
bootstrap_statistics <- numeric(B)

for (i in 1:B) {
  # Generate new data using the sample mean and covariance
  bootstrap_sample <- as.data.frame(mvrnorm(n = n, mu = sample_mean, Sigma = sample_cov))
  
  # Compute correlation
  bootstrap_statistics[i] <- cor(bootstrap_sample)[1, 2]
}

# Bootstrap Results
bootstrap_mean <- mean(bootstrap_statistics)
bootstrap_variance <- var(bootstrap_statistics)
bootstrap_bias <- bootstrap_mean - 0.3  # Bias w.r.t true correlation 0.3
bootstrap_median <- median(bootstrap_statistics)

# --- Jackknife ---
jackknife_statistics <- numeric(n)

for (i in 1:n) {
  jackknife_sample <- bivariate_data[-i, ]
  jackknife_statistics[i] <- cor(jackknife_sample)[1, 2]
}

# Jackknife Results
jackknife_mean <- mean(jackknife_statistics)
jackknife_variance <- var(jackknife_statistics)
jackknife_bias <- (n - 1) * (jackknife_mean - 0.3)

# --- Output Summary ---
cat("Sample Correlation:", round(sample_cor, 4), "\n")
cat("Bootstrap Mean Correlation:", round(bootstrap_mean, 4), "\n")
cat("Bootstrap Variance:", round(bootstrap_variance, 6), "\n")
cat("Bootstrap Bias:", round(bootstrap_bias, 4), "\n")
cat("Bootstrap Median:", round(bootstrap_median, 4), "\n\n")

cat("Jackknife Mean Correlation:", round(jackknife_mean, 4), "\n")
cat("Jackknife Variance:", round(jackknife_variance, 6), "\n")
cat("Jackknife Bias:", round(jackknife_bias, 4), "\n")
