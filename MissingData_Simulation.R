# missing_data_simulation.R
# Simulating Missing Data Mechanisms: MCAR and MAR
# Author: Surath Chakraborti
# Description: Demonstrates how missingness affects the sample mean
# Distribution: Normal(2, 0.4)

# --- Setup ---
set.seed(1345)                 # For reproducibility
pop_mean <- 2                 # True mean
pop_var <- 0.4                # True variance
pop_sd <- sqrt(pop_var)      # Convert to standard deviation

# --- Generate Complete Data ---
n <- 100
comp_data <- rnorm(n, mean = pop_mean, sd = pop_sd)

# Sample Mean from Complete Data
sample_mean <- mean(comp_data)

# --- MCAR: Missing Completely at Random (20% Missing) ---
Ri_mcar <- rbinom(n, size = 1, prob = 0.2)  # 1 = Missing
mcar_data <- comp_data
mcar_data[Ri_mcar == 1] <- NA
mean_mcar <- mean(mcar_data, na.rm = TRUE)

# --- MAR: Missing At Random (depends on value) ---
# Simulate missingness probability using logistic function
Ri_mar <- rbinom(n, size = 1, prob = plogis(comp_data - 2))  # Higher values → more missing
mar_data <- comp_data
mar_data[Ri_mar == 1] <- NA
mean_mar <- mean(mar_data, na.rm = TRUE)

# --- Output Summary ---
summary_df <- data.frame(
  Population_Mean = pop_mean,
  Sample_Mean = sample_mean,
  Mean_Under_MCAR = mean_mcar,
  Mean_Under_MAR = mean_mar
)
print(summary_df)
