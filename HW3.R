# YAW AMANKWAH
# ASSIGNMENT 3

#QUESTION 2

N = 1e4
x = rnorm(N)
gaussian = dnorm(x)
# (a)
f_x = exp((-x^2)/2)
Y = f_x/gaussian
cat("QUESTION 2a ")
cat("\n")
cat("Mean Y: ", mean(Y))
cat("\n")
cat("Exact value: ", sqrt(2*pi)) # Exact value
cat("\n")

# (b)
f_x = exp(-abs(x))
Y = f_x/gaussian
cat("\n")
cat("QUESTION 2b ")
cat("\n")
cat("Mean Y: ", mean(Y))
cat("\n")
cat("Exact value: ", 2) # Exact value is 2
cat("\n")


# (c)
f_x = 1/(1+x^2)
Y = f_x/gaussian
cat("\n")
cat("QUESTION 2c ")
cat("\n")
cat("Mean Y: ", mean(Y))
cat("\n")
cat("Exact value: ", pi) # Exact value is pi

# QUESTION 4

# Use interval [0,8] for ease
# Draw X_i ~ U([0,8])
# I'll use the ff definitions:
# 0 <= x_i < 1, Y_i = 4
# 1 <= x_i < 2, Y_i = 3
# 2 <= x_i < 4, Y_i = 8
# 4 <= x_i < 8, Y_i = 1

my_sampling = function(N) {
  result = c()
  U = runif(N, 0, 8)
  for (i in U) {
    if (0 <= i & i < 1) {result = append(result, 4)}
    else if (1 <= i & i < 2) {result = append(result, 3)}
    else if (2 <= i & i < 4) {result = append(result, 8)}
    else {result = append(result, 1)}
  }
  return(result)
}

hist(my_sampling(N), main = 'my sampling')


# QUESTION 5

Y = runif(N,0,1)
# (a)
X_1 = (16*Y)^(1/4)
hist(X_1, main = 'f(t) = (t^3)/4')

# (b)
X_2= -log(1-Y)*0.5
hist(X_2, main = 'f(t) = 2e^(-2t)')

