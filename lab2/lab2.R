### Wonjohn Choi (SID: 23123143, GSI: Nanyu)

long=read.csv("long.csv")[[1]]
short=read.csv("short.csv")[[1]]
medium=read.csv("medium.csv")[[1]]
### a, b, c: done on paper.

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
plot(points, mle_y, type="l", main="plot of likelihood function for short", ylab="Likelihood", xlab="THETA") # PLOT
mle_plot = points[which(mle_y==max(mle_y))] # MLE FROM PLOT: 1.12
mle_short = sqrt(mean(short^2)/2) # MLE FROM a): 1.117
var_mle_short= mean(short^2)/(8*length(short)) # APPROX. VARIANCE FROM c) # 0.00329

# For medium,
mle_y = sapply(points, lik(medium))
plot(points, mle_y, type="l", main="plot of likelihood function for medium", ylab="Likelihood", xlab="THETA") # PLOT
mle_plot = points[which(mle_y==max(mle_y))] # MLE FROM PLOT: 2.08
mle_medium = sqrt(mean(medium^2)/2) # MLE FROM a): 2.076
var_mle_medium= mean(medium^2)/(8*length(medium)) # APPROX. VARIANCE FROM c) # 0.00434

# For long,
mle_y = sapply(points, lik(long))
plot(points, mle_y, type="l", main="plot of likelihood function for long", ylab="Likelihood", xlab="THETA") # PLOT
mle_plot = points[which(mle_y==max(mle_y))] # MLE FROM PLOT: 3.32
mle_long = sqrt(mean(long^2)/2) # MLE FROM a): 3.324
var_c= mean(long^2)/(8*length(long)) # APPROX. VARIANCE FROM c) # 0.0211

### e
# For short,
mom_short = sqrt(2/pi)*mean(short) # MOM FROM b): 1.1637
var_short = 2*(4-pi)/(pi^2*length(short))*(mean(short))^2 # APPROX. VARIANCE FROM c): 0.0039

# For medium,
mom_medium = sqrt(2/pi)*mean(medium) # MOM FROM b): 2.069
var_medium = 2*(4-pi)/(pi^2*length(medium))*(mean(medium))^2 # APPROX. VARIANCE FROM c): 0.00472

# For long,
mom_long = sqrt(2/pi)*mean(long) # MOM FROM b): 3.4124
var_long = 2*(4-pi)/(pi^2*length(long))*(mean(long))^2 # APPROX. VARIANCE FROM c): 0.02429

### f
# ralyeigh function which return a function that computes value of rayleigh(theta) for its input
rayleigh = function(theta) {
  function(r) {
    r/theta^2 * exp(-r^2/(2 * theta^2))
  }
}

# For short,
hist(short, freq=F, main="Histogram of Measurements For Short", xlab="Measurements") # Histogram FOR SHORT
points = seq(0.1, 10, 0.01)
y_mom = sapply(points, rayleigh(mom_short))
y_mle = sapply(points, rayleigh(mle_short))
lines(points, y_mom, col="red", type="p", cex=.5) # plot MOM
lines(points, y_mle, col="forest green", type="p", cex=.5) # plot MLE
legend(x=2.1,y=.6, legend = c("MLE Fit", "MOM Fit"), fill=c("forest green","red"))


# For medium,
hist(medium, freq=F, main="Histogram of Measurements For Medium", xlab="Measurements") # Histogram FOR MEDIUM
points = seq(0.1, 10, 0.01)
y_mom = sapply(points, rayleigh(mom_medium))
y_mle = sapply(points, rayleigh(mle_medium))
lines(points, y_mom, col="red", type="p", cex=.5) # plot MOM
lines(points, y_mle, col="forest green", type="p", cex=.5) # plot MLE
legend(x=4,y=.3, legend = c("MLE Fit", "MOM Fit"), fill=c("forest green","red"))

# For long,
hist(long, freq=F, main="Histogram of Measurements For Long", xlab="Measurements") # Histogram FOR LONG
points = seq(0.1, 10, 0.01)
y_mom = sapply(points, rayleigh(mom_long))
y_mle = sapply(points, rayleigh(mle_long))
lines(points, y_mom, col="red", type="p", cex=.5) # plot MOM
lines(points, y_mle, col="forest green", type="p", cex=.5) # plot MLE
legend(x=8,y=.15, legend = c("MLE Fit", "MOM Fit"), fill=c("forest green","red"))

### g
# There seems to be a relationship between my estimates
# and the genomic separation of the point
# because rayleigh plot with estimated paramters
# fit the histogram very well.

### h
# Show that if X follows a Rayleigh distribution with...: this will be shown on paper.
# Show how Proposition D of Section 2.3 of the text can be applied to accomplish this: this will be shown on paper.
# Get sample from Rayleight distribtion (theta) of size n.
genSamRay = function(theta, n) {
  # Use method discussed in section (with Proposition D)
  theta * sqrt(log(1/(1 - runif(n))^2))
}

# We will do this for medium.
# Question says it's suffice to use B = 100 but I use 1000 (this doesn't hurt).
B = 1000
N = length(medium)
# genSamRay(mle_medium, N): Get N samples from Rayleigh distribution with theta of mle_medium
# sqrt(mean( ^2)/2): Formula to compute MLE of theta that we found previously.
sam_mles = replicate(B, sqrt(mean(genSamRay(mle_medium, N)^2)/2))
hist(sam_mles, freq=F, main="Histogram of Means of Bootstrap Samples Of Medium Data", xlab="Mean", ylab="Count")

# Does the distribution appear roughly normal?
# Yes, histogram looks approximately normal.

# Do you think that the large sample theory can be reasonably applied here?
# Yes, becaues histogram of mles of simulated rayleigh samples is roughly
# normal. The histogram gets more normal as I incease B.
# Hence, mles of simulation are asymptotically normal.

# Compare the standard deviation calculated from the bootstrap to the standard
# errors you found previously
# Previously, we got sqrt(0.00434)==0.0659 as the approximate standard error for medium.
sd_mles = sd(sam_mles)*sqrt((length(sam_mles)-1)/length(sam_mles))
# Here, I got 0.06635215 as the standard deciation of mles from the bootstrap.
# Those values are very close.

### i
sorted_mles = sort(sam_mles)
# 95% confidence interval from bootstrap
CL_bootstrap = c(sorted_mles[26], sorted_mles[975]) # 1.952830 2.196689
# 95% confidence interval from large sample theory
CL_lst = c(mle_medium - sqrt(var_mle_medium) * 1.96, mle_medium + sqrt(var_mle_medium) * 1.96) # 1.946794 2.205171
# bootstrap confidence interval is very similar to the interval found by large sample theory.
