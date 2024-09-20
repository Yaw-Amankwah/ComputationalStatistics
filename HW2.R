# PROBLEM 1

x = 0
for (i in 1:10) {
  if (runif(1) < 0.5) {x = x+1}
  else break
}
x = x+1
print(x)

# The above code runs for 10 iterations and 
# generates a uniform value between [0,1] in each iteration.
# If the value is less than 0.5, we consider that a failure and run the next
# iteration until we get a value >= 0.5, which we consider a success 
# and break from the loop.
# Variable x prints out the total number of iterations until first success.

# Doesn't always output geometric X ~ (0.5) because only runs 10 iterations.
# It will automatically break out of the loop and output 11 after 10 failures
# even if no successes have been recorded.

# Geometric function
geometric = function(p) {
  m = 0
  while (runif(1) > p) { # consider runif(1) > p to be failure
    m = m + 1
  }
  print(m+1)
}

# PROBLEM 2

n = 1e4
r = runif(n)
s = rep(0, n)
for (i in 1:n) {
  s[i] = min (r[i], 1-r[i])
}
print(mean(s))

# CDF of S
cdf_S = function(x) {2*x}
curve(cdf_S, from=0, to=0.5, n=300, col="blue", xlab="x", ylab="F(x)",
      lwd=2, main="F(x) of S")

# PROBLEM 3

# Method 1: Break once, then break shorter piece
Method1 = function(x) {
  mean_of_means1 = rep(0,x)
  for (i in 1:x) {
    n = 1e4
    r = runif(1)
    maximum = min(r, 1-r) # shorter of the first stick break
    # Run uniform on stick length from 0 to shorter of the first stick break
    q = runif(n, min = 0, max = maximum) 
    mean_of_means1[i] = mean(q)
    #cat("Mean Method 1: ", mean(q))
  }
  cat("Average mean 1: ", mean(mean_of_means1))
}

# Method 2: Make 2 breaks simultaneously. Take shortest piece
Method2 = function(x) {
  x = 2
  mean_of_means2 = rep(0,x)
  for (j in 1:x) {
    n = 1e4
    r1 = runif(n) # first break
    r2 = runif(n) # second break
    s = rep(0, n) # vector to hold shortest length
    for (i in 1:n) {
      a = r1[i]
      b = r2[i]
      shortest = min(a, b,1-a, 1-b,abs(b-a))
      s[i] = shortest
    }
    mean(s)
    mean_of_means2[j] = mean(s)
  }
  cat("\nAverage mean 2: ", mean(mean_of_means2))
}

Average_Mean = function(x) {
  Method1(x)
  Method2(x)
}

# Method 2 seems to produce a shorter piece on average

# PROBLEM 4

poisson = rpois(1e3,4)
probabilities = prop.table(table(poisson))
cum_prob = cumsum(probabilities)
plot(cum_prob, col="red",xlab="x", ylab="F(x)",
     lwd=2, main="Empirical F(x)") # empirical cdf

dens_func = function(x){
 ((1/exp(4))*(4^x)*(1/factorial(x)))
}
cdf = function(x){sum(dens_func(0:x))}
y = Vectorize(cdf)
curve(y, from = 0, to=20, type="p", col="blue",
      xlab="x", ylab="F(x)", lwd=2, main="Actual F(x)")# actual cdf

curve(y, from = 0, to=20, type="p", col="blue")# actual cdf
par(new=TRUE)
plot(cum_prob,type="p", col="red") # empirical cdf
