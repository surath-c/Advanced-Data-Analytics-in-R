
---

### 📄 `orthodont_analysis.R` (cleaned R script)

```r
# Orthodont Dataset - OLS and WLS Regression
# Author: Surath Chakraborti

# Load required libraries
library(nlme)
library(MASS)

# Load the dataset
data("Orthodont")
head(Orthodont)

# Extract variables
dist <- Orthodont$distance
age <- Orthodont$age
sex <- as.factor(Orthodont$Sex)

# --------------------------
# Ordinary Least Squares (OLS)
# --------------------------
fit_ols <- lm(dist ~ age + sex)

# Show estimated coefficients
summary(fit_ols)

# Design matrix
X <- model.matrix(dist ~ age + sex)

# X'X and its inverse
XtX <- t(X) %*% X
XtX_inv <- solve(XtX)

# Residuals and residual variance matrix
residuals_ols <- resid(fit_ols)
V <- residuals_ols %*% t(residuals_ols)
v_diag <- diag(diag(V))

# Variance of beta estimates
var_beta <- XtX_inv %*% t(X) %*% v_diag %*% X %*% XtX_inv
se_beta <- sqrt(diag(var_beta))  # Standard errors

# Test statistics (Z-values)
beta_hat <- as.matrix(fit_ols$coefficients)
z_stats <- beta_hat / se_beta

# P-values from normal distribution
p_values_ols <- 2 * (1 - pnorm(abs(z_stats)))

# --------------------------
# Weighted Least Squares (WLS)
# --------------------------

# Define a custom positive definite weight vector
set.seed(123)
W <- sample(100:1000, length(dist), replace = FALSE)
w_matrix <- diag(W)

# Fit WLS model
fit_wls <- lm(dist ~ age + sex, weights = W)
summary(fit_wls)

# Design matrix for WLS
X_wls <- model.matrix(dist ~ age + sex)

# X'WX and its inverse
XtWX <- t(X_wls) %*% w_matrix %*% X_wls
XtWX_inv <- solve(XtWX)

# Reuse residuals from OLS (as in original script)
V1 <- residuals_ols %*% t(residuals_ols)
v1_diag <- diag(diag(V1))

# Variance of beta estimates under WLS
var_beta_wls <- XtWX_inv %*% t(X) %*% w_matrix %*% v1_diag %*% t(w_matrix) %*% X %*% XtWX_inv
se_beta_wls <- sqrt(diag(var_beta_wls))

# Z statistics and P-values under WLS
z_stats_wls <- beta_hat / se_beta_wls
p_values_wls <- 2 * (1 - pnorm(abs(z_stats_wls)))

# --------------------------
# Output
# --------------------------
cat("OLS Coefficients:\n")
print(beta_hat)
cat("\nStandard Errors (OLS):\n")
print(se_beta)
cat("\nZ-Statistics (OLS):\n")
print(z_stats)
cat("\nP-values (OLS):\n")
print(p_values_ols)

cat("\n\nWLS Coefficients:\n")
print(as.matrix(fit_wls$coefficients))
cat("\nStandard Errors (WLS):\n")
print(se_beta_wls)
cat("\nZ-Statistics (WLS):\n")
print(z_stats_wls)
cat("\nP-values (WLS):\n")
print(p_values_wls)
