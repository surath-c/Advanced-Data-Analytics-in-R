# bootstrap_duncan_rlm.R
# Title: Bootstrap Estimation of Robust Linear Regression (Duncan Dataset)
# Author: Surath Chakraborti
# Description: Demonstrates three bootstrap methods for estimating coefficients of a robust linear model.

# --- Libraries ---
library(car)
library(MASS)

# --- Robust Linear Model on Duncan Data ---
set.seed(12345)  # for reproducibility
data <- Duncan
mod.duncan.hub <- rlm(prestige ~ income + education, data = data, maxit = 200)
summary(mod.duncan.hub)

# --- Parameters ---
n <- nrow(data)
B <- 1999
original_coefficients <- coef(mod.duncan.hub)

# Matrix to store bootstrap coefficients
bootstrap_coefficients <- matrix(NA, nrow = B, ncol = length(original_coefficients))

# -------------------------------
# 1. Simple Bootstrap (Case Resampling)
# -------------------------------
for (i in 1:B) {
  bootstrap_sample <- data[sample(1:n, replace = TRUE), ]
  bootstrap_model <- lm(prestige ~ income + education, data = bootstrap_sample)
  bootstrap_coefficients[i, ] <- coef(bootstrap_model)
}

# -------------------------------
# 2. Residual Resampling (Fixed X Bootstrap)
# -------------------------------
# Extract fitted values and residuals from the robust model
Y_hat <- fitted(mod.duncan.hub)
residuals <- residuals(mod.duncan.hub)
leverage <- hatvalues(mod.duncan.hub)
r_bar <- mean(residuals)

for (i in 1:B) {
  scaled_residuals <- residuals / sqrt(1 - leverage - r_bar)
  bootstrap_residuals <- sample(scaled_residuals, replace = TRUE)
  Y_star <- Y_hat + bootstrap_residuals
  bootstrap_model <- lm(Y_star ~ income + education, data = data)
  bootstrap_coefficients[i, ] <- coef(bootstrap_model)
}

# -------------------------------
# 3. Random X Bootstrap (Case Resampling Again)
# -------------------------------
for (i in 1:B) {
  bootstrap_indices <- sample(1:n, replace = TRUE)
  bootstrap_sample <- data[bootstrap_indices, ]
  bootstrap_model <- lm(prestige ~ income + education, data = bootstrap_sample)
  bootstrap_coefficients[i, ] <- coef(bootstrap_model)
}

# -------------------------------
# Bootstrap Results Summary
# -------------------------------

# Mean and SD of bootstrap estimates
bootstrap_means <- colMeans(bootstrap_coefficients)
bootstrap_sd <- apply(bootstrap_coefficients, 2, sd)

# Variance-Covariance matrix
bootstrap_vcov <- cov(bootstrap_coefficients)

# Confidence Intervals using Normal Approximation
alpha <- 0.90
z_value <- qnorm(1 - (1 - alpha) / 2)
ci_normal <- cbind(
  bootstrap_means - z_value * bootstrap_sd,
  bootstrap_means + z_value * bootstrap_sd
)
colnames(ci_normal) <- c("Lower", "Upper")
rownames(ci_normal) <- names(original_coefficients)

# Percentile-based Confidence Intervals for income and education
percentiles <- c(0.68, 0.90, 0.95)
ci_percentiles <- lapply(percentiles, function(p) {
  apply(bootstrap_coefficients[, 2:3], 2, function(col)
    quantile(col, probs = c((1 - p)/2, 1 - (1 - p)/2)))
})
names(ci_percentiles) <- paste0(percentiles * 100, "%")

# -------------------------------
# Print Output
# -------------------------------
cat("\nBootstrap Means:\n")
print(round(bootstrap_means, 4))

cat("\nBootstrap Variance-Covariance Matrix:\n")
print(round(bootstrap_vcov, 5))

cat("\nConfidence Intervals (Normal Approximation, 90%):\n")
print(round(ci_normal, 4))

cat("\nConfidence Intervals (Percentile Method):\n")
for (p in names(ci_percentiles)) {
  cat("\nPercentile CI at", p, "\n")
  print(round(ci_percentiles[[p]], 4))
}
