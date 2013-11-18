### Wonjohn Choi (SID: 23123143, GSI: Nanyu)

long=read.csv("long.csv")[[1]]
short=read.csv("short.csv")[[1]]
medium=read.csv("medium.csv")[[1]]
### d. We will plot the likelihood function
# In a), we found that the likelihood function (theta) is
lik = function(data) {
  function(theta) {
    # sum(log(data)) - 2 * length(data) * log(theta) - sum(data^2)/(2 * theta^2)
    prod(data/theta^2 * exp(-data^2/(2*theta^2)))
  }
}
  
points = seq(0.1, 10, 0.01)
# For short,
mle_y = sapply(points, lik(short))
plot(points, mle_y, type="l") # PLOT
mle_plot = points[which(mle_y==max(mle_y))] # MLE FROM PLOT: 1.12
mle_a = sqrt(sum(short^2)/(2*length(short))) # MLE FROM a): 1.117
var_c= (sum(short^2)/length(short)) /(8*length(short)) # APPROX. VARIANCE FROM c) # 0.00329

# For medium,
mle_y = sapply(points, lik(medium))
plot(points, mle_y, type="l") # PLOT
mle_plot = points[which(mle_y==max(mle_y))] # MLE FROM PLOT: 2.08
mle_a = sqrt(sum(medium^2)/(2*length(medium))) # MLE FROM a): 2.076
var_c= (sum(medium^2)/length(medium)) /(8*length(medium)) # APPROX. VARIANCE FROM c): 0.00434

# For long,
mle_y = sapply(points, lik(long))
plot(points, mle_y, type="l") # PLOT
mle_plot = points[which(mle_y==max(mle_y))] # MLE FROM PLOT: 3.32
mle_a = sqrt(sum(long^2)/(2*length(long))) # MLE FROM a): 3.324
var_c= (sum(long^2)/length(long)) /(8*length(long)) # APPROX. VARIANCE FROM c): 0.0211

### e
# For short,
mom_b = sqrt(2/pi)/length(short)*sum(short) # MOM FROM b): 1.1637
var_c = (2/pi)*(4-pi)/(pi*length(short))*(sum(short)/length(short))^2 # APPROX. VARIANCE FROM c): 0.0039

# For medium,
mom_b = sqrt(2/pi)/length(medium)*sum(medium) # MOM FROM b): 2.069
var_c = (2/pi)*(4-pi)/(pi*length(medium))*(sum(medium)/length(medium))^2 # APPROX. VARIANCE FROM c): 0.00472

# For long,
mom_b = sqrt(2/pi)/length(long)*sum(long) # MOM FROM b): 3.4124
var_c = (2/pi)*(4-pi)/(pi*length(long))*(sum(long)/length(long))^2 # APPROX. VARIANCE FROM c): 0.02429

### f
rayleigh = function(theta, r) {
  r/theta^2 * exp(-r^2/(2 * theta^2))
}
hist(short, freq=F) # Histogram FOR SHORT
points = seq(0.1, 10, 0.01)
ray_y = rayleigh(points, rayleigh(mom_short))
lines(points, ray_y)

### g

### h
genSamRay = function(theta, n) {
  # Use method discussed in section
  theta * sqrt(log(1/(1 - runif(n))^2))
}
