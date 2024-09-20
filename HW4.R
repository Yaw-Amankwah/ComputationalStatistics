# QUESTION 3

#(a) Rejection Sampling
n = 1e4

X = runif(n, -1,1)
Y = runif(n, -0.5, 0.5)
x_keep = c()
y_keep = c()

for (i in 1:n) {
  if((((X[i])^2) + (4*(Y[i])^2)) <=1) {
    x_keep = append(x_keep, X[i])
    y_keep = append(y_keep, Y[i])
  }
}
plot(x_keep, y_keep, main='(4a)Rejection Sampling')

#(b) Uniform
n = 1e4
r = sqrt(runif(n))
theta = runif(n, 0, 2*pi)
x = r*cos(theta)
y = r*sin(theta)*0.5
plot(x,y, main = '(4b)Uniform Sampling')

#QUESTION 4

n = 1e4
M = rnorm(n,69,2.9)
W = rnorm(n, 64,2.7)
W_taller = which(Female > Male)
Prob = length(W_taller)/n
Prob
# Answer = 0.106

#QUESTION 5
n = 1e4
for (i in (1:log10(n))) {
  j = 10^i
  x = rnorm(j)
  y = rnorm(j)
  X = x/((x^2+y^2)**0.5)
  Y = y/((x^2+y^2)**0.5)
  plot(X,Y, main = 'Question 5')
}