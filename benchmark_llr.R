# Load required packages
library(microbenchmark)

# Source the llr_functions.R to get the llr function
source("llr_functions.R")

# Create sample data for testing (adjust sizes as needed)
n = 15
## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)
omega <- 1

# Benchmark the llr function
results <- microbenchmark(
  llr_results = llr(x, y, z, omega = 1),
  times = 100  # Number of times to run the function for the benchmark
)

# Print out the results
print(results)
cat("Mean time for llr function:", mean(results$time), "nanoseconds\n")
