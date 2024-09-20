# Yaw Amankwah
# Homework 5

# QUESTION 1

#(a)

n = 1e6
k = 1
samples = rep(0,n)
while (k <=n) {
  X = (5+pi)*runif(1) - pi
  Y = 0.5*runif(1)
  if ((X <=0 & X >=-pi) & (Y <=1/pi)){
    samples[k] = X
    k = k+1
    }
  else if ((X <=5 & X >=0) & (Y <=X/25)) {
    samples[k] = X
    k = k+1
  }
}
hist(samples, main = 'Histogram Problem 1a')
#(b)

n = 1e6

f = function(x){return(2/(pi*(1+x^2)))} 

k = 1; samples = rep(0,n)
while(k<=n){
  X = 2*runif(1) - 1; V = runif(1)
  if(V<f(X)){samples[k]=X; k=k+1} 
}
hist(samples, main = 'Histogram Problem 1b')

# QUESTION 2
n_vector = c()
m_vector = c()
train_error_vector = c()
test_error_vector = c()
n_range = c(50,100,500)

linregress = function(x,y){
  xmean = mean(x)
  ymean = mean(y)
  a = sum((x-xmean)*(y-ymean))/sum((x-xmean)^2)
  b = ymean - a*xmean
  return(c(a,b))
}

reg_func = function (a,b,x) {
  return (a*x + b)
}

for (n in n_range) {
  for (m in c(n, 2*n)) {
    x_train = 5 * runif(n)
    y_train = 3 * x_train + 1
    reg_coefficients = linregress(x_train, y_train)
    a = reg_coefficients[1]
    b = reg_coefficients[2]
    training_error = sum((y_train - reg_func(a,b,x_train))^2)/n
    x_test = 5 * runif(m)
    y_test = 3 * x_test + 1
    testing_error = sum((y_test - reg_func(a,b,x_test))^2)/m
    n_vector = append(n_vector, n)
    m_vector = append(m_vector, m)
    train_error_vector = append(train_error_vector, training_error)
    test_error_vector = append(test_error_vector, testing_error)
  }
}

result = cbind(n_vector, m_vector, train_error_vector, test_error_vector,
               test_error_vector - train_error_vector)
result

# OBSERVATION
# No clear patterns in my testing. Both training and testing 
# error negligible in all instances. Training error was larger in some
# instances and smaller in others.
# I would have assumed the testing error to be larger in almost all instances
# because model was trained to fit the training data so the estimates would 
# not adequately fit the test data.

# QUESTION 5

theta_values = c(1,2,4,8)
n_values = c(1e2,1e4,1e6,1e8)
theta_vector = c()
n_vector = c()
max_vector = c()
mean_vector = c()
for (theta in theta_values) {
  for (n in n_values) {
    vals = theta * runif(n)
    theta_vector = append(theta_vector, theta)
    n_vector = append(n_vector, n)
    max_vector = append(max_vector, max(vals))
    mean_vector = append(mean_vector, mean(vals))
  }
}
result = cbind(theta_vector, n_vector, max_vector,mean_vector)
result
