# Yaw Amankwah
# HW6

# Question 1
#(a) f(x) = 1-|x|, |x| <= 1
f = function (x) {
  return (1 - abs(x))
}

# f decreasing function on [0,1].
# Max occurs at x = 0, f(0) = 1
# f increasing on [-1,0].
# Max occurs at x = 0, f(0) = 1
# Overall max at x = 0. f(0) = 1
# Pick M = 2

n = 1e6
samples = rep(0,n)
k = 1
M = 2
while (k <= n) {
  x = 2 * runif(1) - 1
  V = runif(1)
  if (V < (f(x)/M)) {
    samples[k] = x; k = k + 1
  }
}
hist(samples, main='1a')


#(b) f(x) = (1/360)(4x^3 + 3x^2 + 10), x in [0,4]
f = function (x) {
  return ((1/360) * ((4 * x^3) + (3 * x^2) + 10))
}

# f strictly increasing function on [0,4].
# Max occurs at x = 4, f(4) = 0.872
# Pick M = 1

n = 1e6
samples = rep(0, n)
k = 1
while (k <= n) {
  x = 4 * runif(1)
  V = runif(1)
  if (V < f(x)) {
    samples[k] = x; k = k + 1
    }
}
hist(samples, main = '1b')


# Question 2

sigma_tilde = function (x_array) {
  n = length(x_array)
  x_bar = mean(x_array)
  sum = 0
  for (i in (1:n)) {
    sum = sum + (x_array[i] - x_bar)^2
  }
  return (sqrt(sum/(n-1)))
}

sigma_tilde_array = c()
n_array = c()
n = 100
x_array = runif(n)
sigma_tilde_array = append(sigma_tilde_array, sigma_tilde(x_array))
n_array = append(n_array, n)

i = 2
while (TRUE) {
  n = n * 100
  x_array = runif(n)
  sigma_tilde_array = append(sigma_tilde_array, sigma_tilde(x_array))
  n_array = append(n_array, n)
  if ((abs(sigma_tilde_array[i] - sigma_tilde_array[i-1])) < 1e-4) {
    break
  }
  i = i + 1
}

true_sigma_array = rep(sqrt(1/12), length(n_array))
M = cbind(n_array, sigma_tilde_array, true_sigma_array)
M
cat("\nAverage sigma tilde: ", mean(sigma_tilde_array))
cat("\nTrue sigma: ", sqrt(1/12))

# Question 3
f = function (x, L) {
  if (x <=1) {
    return ((1/(1+2*L)) * 2*x)
  }
  else {
    return (2/(1+2*L))
  }
}

# For any given L, 2x <=2.
# Max function occurs at 2/(1+2L) < 1
# Choose M = 1

n = 1e6
samples = rep(0, n)
k = 1
L = 5
while (k <= n) {
  x = (L+1)*runif(1) 
  V = runif(1)
  if (V < f(x, L)) {
    samples[k] = x; k = k + 1
  }
}
hist(samples)
# Question 4
#(a)
f = function (n, p, sigma) {
  flips_array = c()
  X_k_array = c()
  tails_array = c()
  for (i in (1:n)) {
    x = runif(1)
    if (x <= p) { # heads
      flips_array = append(flips_array, 1)
      x_k= rnorm(1)
      X_k_array = append(X_k_array, x_k)
    }
    else { # tails
      flips_array = append(flips_array, 0)
      x_k = rnorm(1,10, sigma^2)
      tails_array = append (tails_array, x_k)
      X_k_array = append(X_k_array, x_k)
    }
  }
  x_bar = mean(flips_array)
  t = length(tails_array)
  p_hat = 1 - (sum((flips_array - x_bar)^2))/(sum(flips_array))
  sigma_squared_hat = mean(tails_array^2) - 100
  
  hist(X_k_array)
  return (c(p_hat, sigma_squared_hat))
  
}

m = f(100, 3/4, 5)

#(b)
MSE = function (N) {
  total_p_mse = 0
  total_sigma_mse = 0
  for (i in (1:N)) {
    m = f(100, 3/4, 5)
    total_p_mse = total_p_mse + ((m[1] - .75)^2)
    total_sigma_mse = total_sigma_mse + ((m[2] - 25)^2)
  }
  return (c(total_p_mse/N, total_sigma_mse/N))
}
mse = MSE (10)
cat("\nEmpirical MSE p: ", mse[1])
cat("\nEmpirical MSE sigma: ", mse[2])


    

