# Yaw Amankwah
# Homework7

# PROBLEM 1

n = 30
m = 1e4
# Let theta1 = usual unbiased estimator 
# Let theta2 = np(1-p)

p_array = c()
MSE_theta1_array = c()
MSE_theta2_array = c()
for (p in seq(from=0.1, to=0.5, by=0.05)) {
  x = rbinom(m,n,p)
  x_bar = mean(x)
  p_hat = sum(x/n)/(m)
  theta1 = (sum((x - x_bar)^2))/(n-1)
  theta2 = n * p_hat * (1 - p_hat)
  var_x = var(x)
  MSE_theta1 = ((var_x - theta1)^2)/m
  MSE_theta2 = ((var_x - theta2)^2)/m
  p_array = append(p_array, p)
  MSE_theta1_array = append(MSE_theta1_array, MSE_theta1)
  MSE_theta2_array = append(MSE_theta2_array, MSE_theta2)
}

plot(p_array, MSE_theta1_array, type="l", col="red")
lines(p_array, MSE_theta2_array, type = "l", col="blue")

# MSE_theta2_array values too small to show on the same graph

 

# PROBLEM 2
N = 1e3
var1_array = c()
var2_array = c()
n_array = c()
for (n in seq(from=100, to=1000, by=100)) {
  n_array = append(n_array, n)
  var1_total = 0
  var2_total = 0
  for (i in (1:N)) {
    x = rnorm(n)
    x_bar = mean(x)
    var1 = (sum((x - x_bar)^2))/(n-1)
    var2 = (sum((x - x_bar)^2))/n
    var1_total = var1_total + var1
    var2_total = var2_total + var2
  }
  var1_array = append(var1_array, var1_total/N)
  var2_array = append(var2_array, var2_total/N)
}

plot(n_array, var1_array, type="b", pch=19, col="red",
     ylim = c(0.990, 1.005), xlab = "n", ylab = "variance",
     main="Problem 2: Variance vs n")
lines(n_array, var2_array, pch = 18, col="blue", type="b", lty=2)
legend("bottomright", legend=c("unbiased", "biased"),
       col = c("red", "blue"), lty = 1:2, cex=0.8)

# PROBLEM 3
# MLE for U([a,b]). a = e, b = 3pi
a = exp(1)
b = 3 * pi
n = 100
X = runif(n, a, b)

Likelihood_fun = function(a,b,n=100){
  return (1/(b-a)^n)
}

# create a (x,y) grid and evaluate Lfun on it
start_time = Sys.time()

N = 1e4
x = seq(0, 4, by=4/N)
y = seq(6, 12, by=6/N)
Lgrid = outer(x,y,Likelihood_fun)

# find the maximizer of Lgrid
maxidx = which(Lgrid == max(Lgrid), arr.ind = TRUE)  
a_MLE = x[maxidx[1]]
b_MLE = y[maxidx[2]]

end_time = Sys.time()
print(end_time-start_time)
print(c(a,b,a_MLE,b_MLE))

# PROBLEM 4

# (a)
z_alpha = -qnorm(0.025, 0 , 1)
x_bar_greater = (0.17) * z_alpha + 3.5
x_bar_less = (0.17) * (-z_alpha) + 3.5
cat("Reject null hypothesis if x_bar in interval: [0,", x_bar_less,"]")
print("OR")
cat("Reject null hypothesis if x_bar in interval: [", x_bar_greater, ",6]")

# (b)
n = 100
N = 1e5
alpha = 0.05
c_alpha = -qnorm(alpha/2,0,1)
my_sampling1 = function(n) {
  result = c()
  U = runif(n, 0, 8)
  for (i in U) {
    if (0 <= i & i < 2) {result = append(result, 1)}
    else if (2 <= i & i < 3) {result = append(result, 2)}
    else if (3 <= i & i < 4) {result = append(result, 3)}
    else if (4 <= i & i < 5) {result = append(result, 4)}
    else if (5 <= i & i < 6) {result = append(result, 5)}
    else {result = append(result, 6)}
  }
  return(result)
}

num_fails1 = 0
for (i in (1:N)) {
  X = my_sampling1(n)
  if ((mean(X) >= x_bar_greater) | (mean(X) <= x_bar_less)) {
    num_fails1 = num_fails1 + 1
  }
}
cat("Fail z-test: ", num_fails1/N * 100, "%")

# (c)

n = 100
N = 1e5
alpha = 0.05
c_alpha = -qnorm(alpha/2,0,1)
my_sampling2 = function(n) {
  result = c()
  U = runif(n, 0, 8)
  for (i in U) {
    if (0 <= i & i < 1) {result = append(result, 1)}
    else if (1 <= i & i < 2) {result = append(result, 2)}
    else if (2 <= i & i < 4) {result = append(result, 3)}
    else if (4 <= i & i < 6) {result = append(result, 4)}
    else if (6 <= i & i < 7) {result = append(result, 5)}
    else {result = append(result, 6)}
  }
  return(result)
}

num_fails2 = 0
for (i in (1:N)) {
  X = my_sampling2(n)
  if ((mean(X) >= x_bar_greater) | (mean(X) <= x_bar_less)) {
    num_fails2 = num_fails2 + 1
  }
}
cat("Fail z-test: ", num_fails2/N * 100, "%")


# (d)

# The two loaded dice have the same expected value (3.5) as a fair dice.
# I expect that their means will be similar to a fair dice
# and hence have similar reject rates for the z-test (I assumed 0%)
