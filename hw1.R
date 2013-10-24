
#########################################

### Wonjohn Choi
### HW1 #65
### Credit: Got a lot of help from GSI.

setwd('/Users/!cmfmath128a/Desktop/')
cancer = read.csv(file="cancer.txt", header= FALSE, sep = ",")
colnames(cancer)= c("mortality","population")
#a
hist(cancer$mortality, xlab= "mortality", main= "Mortality", breaks=18)

#b
mean(cancer$mortality) # population mean:  39.85714
sum(cancer$mortality) # total cancer mortality: 11996
pop_var = var(cancer$mortality)*(length(cancer$mortality)-1)/(length(cancer$mortality))
pop_var # population variance: 2590.1025154
sqrt(pop_var) # standard deviation: 50.89305

#c
nreps = 1000
ms = vector (length = nreps)
for (i in 1:nreps)
{
  ms[i] = mean(sample(cancer[,"mortality"], size = 25, replace = F))
}

SD = sd(ms)*sqrt((length(ms)-1)/length(ms))
hist(ms, xlab = "Sample Mean", main = "Sampling Mean")
abline( v = average, col = "red", lty = "dashed", lwd = 2)
abline( v = average-2*SD, col = "blue", lty = "dashed", lwd = 2)
abline( v = average+2*SD, col = "blue", lty = "dashed", lwd = 2)
### By CLT, if n is large enough,
### the means of the samples will be normal


#d
smpl= sample(cancer[,"mortality"],size=25, replace = F)
mean(smpl) # mean of sample
mean(smpl)*length(cancer$mortality) # estimated total cancer mortality

#e
n=length(smpl)
N=length(cancer$mortality)
var(smpl) # s^2
est_pop_var=var(smpl)/n*(1-n/N)
est_pop_var # estimate of pop. variance
est_sd = sqrt(est_pop_var)
est_sd # estimate of pop. stdev

#f
se_est = est_sd/sqrt(length(smpl))
c = se_est * se_est * c(-1,1) * qnorm(0.975)
c # conf. interval
