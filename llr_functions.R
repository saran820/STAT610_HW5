## new comment added after the file added was already filled.

## Top-level function
llr <- function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x=x, y=y, omega=omega)
  return(fits)
}

## Second-level function
compute_f_hat = function(z, x, y, omega) {
  
  #changed line
  Wz = make_weight_matrix(z, x, omega)

  X = make_predictor_matrix(x)
  
  # changed line
  scaled_X = apply(X, 2, function(col) col * Wz)
  scaled_y = Wz * y
  f_hat = c(1, z) %*% solve(t(X) %*% scaled_X) %*% t(X) %*% scaled_ys
  
  return(f_hat)
}


## Helper function to create the weight matrix
make_weight_matrix <- function(z_i, x, omega) {
  n <- length(x)
  W <- diag(n)
  for (j in 1:n) {
    W[j, j] <- compute_weight(z_i - x[j], omega)
  }
  return(W)
}

# Helper function to compute the weight for a single point based on its distance from z
compute_weight <- function(distance, omega) {
  if (abs(distance) < 1) {
    return((1 - abs(distance/omega)^3)^3)
  } else {
    return(0)
  }
}

# Helper function to create the predictor matrix
make_predictor_matrix <- function(x) {
  return(cbind(1, x))
}

