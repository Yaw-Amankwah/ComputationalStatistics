# MATH377 / YAW AMANKWAH / HW1

# QUESTION 1

sum1 = 0
for (i in 0:10) {
  for (j in 0:i) {
    sum1 = sum1 + sin(i*i)*cos(j*j)
  }
}
sum1

sum2 = 0
for (j in 0:10) {
  for (i in 0:j) {
    sum2 = sum2+sin(i*i)*cos(j*j)
  }
}
sum2

# QUESTION 2

# Generator for when n is strictly positive
x_vector_generator = function(n) {
  x = rep(1:n)
  p = ceiling(n/2)
  q = 2*p
  for (i in 1:p) {
    j = 2*i - 1
    x[j] = q-j 
  }
  return(x)
}

# Generator for when n is allowed to be negative
x2_vector_generator = function(n) {
  m = abs(n)
  x = rep(0:m)
  x = x[-c(1)]
  p = ceiling(m/2)
  q = 2*p
  for (i in 1:p) {
    j = 2*i - 1
    x[j] = q-j 
  }
  if (n > 0) {return (x)}
  else if (n < 0) { return (-1*x)}
}

#QUESTION 3

A = matrix(NA, nrow = 20, ncol = 1e4)
A[1, ] = sample(1:6, 1e4, replace = TRUE)
for (i in 2:20) {
  A[i, ] = sample(1:6, 1e4, replace = TRUE)
}
count = 0
col_sum = colSums(A)
# Get number of occurrences of 70
occ_70 = length(col_sum[col_sum==70])
# Divide number of occurrences of 70 by 1e4 to get probability
prob_70 = occ_70/1e4
prob_70

# QUESTION 4

n_prob = function(n) {
  # Get list of count of #heads per each simulation
  erica = rbinom (1000, n, 0.5)
  fred = rbinom (1000, n+1, 0.5)
  # Get list of indices that match "#heads fred > #heads erica"
  count = which(fred > erica) 
  # return the probability
  return (length(count)/1000)
}




