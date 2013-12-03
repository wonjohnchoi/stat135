### Wonjohn Choi (23123143)
### HW 8

### 36
data = read.table("ampicillin.txt", header = T)
micro = data[,1]
hydro = data[,2]
abs(mean(micro) - mean(hydro)) # difference in mean: 0.44
diff = micro - hydro
var(diff) * (n-1)/n # var(X-Y): 20.61923
# if pairing is ignored,
sqrt(14 * (var(micro) + var(hydro)) * (n-1)/n / 28) * sqrt(2/15)

### 42
seeded = read.table("seededclouds.txt",header = F)[,1]
control = read.table("controlclouds.txt",header = F)[,1]
n <- length(seeded) # OK to do this because length of both samples are the same.
### a)
pi_hat <- wilcox.test(seeded,control,exact=F)$statistic/n^2 # 0.6997041

# Used a large portion of code from lab.
boot = function(){
  samp1 = sample(seeded,n,replace = T)
  samp2 = sample(control,n,replace = T)
  return(wilcox.test(samp1,samp2,exact=F)$statistic/n^2)
}
B = 1000
boot_samp = replicate(B,boot())

### b)
std_err <- sqrt(var(boot_samp)*(B-1)/B) # 0.07455018

### c)
# 95% confidence interval: 0.5620932 0.8476331
CI <- 2*pi_hat-quantile(boot_samp,prob=c(0.975,0.025))

### 44
s0 = read.table("vitaminc1b.txt", header=T)[,1]
s2 = read.table("vitaminc1b.txt", header=T)[,2]
sd = s0 - s2
n0 = read.table("vitaminc1a.txt", header=T)[,1]
n2 = read.table("vitaminc1a.txt", header=T)[,2]
nd = n0 - n2

### a)
boxplot(list(s0, n0), names=c("Schizophrenics at 0", "Nonschizophrenics at 0"), main="concentration at 0")
boxplot(list(s2, n2), names=c("Schizophrenics at 2", "Nonschizophrenics at 2"), main="concentration at 2")
boxplot(list(sd, nd), names=c("Difference of Schizophrenics", "Difference of Nonschizophrenics"), main="Difference in Concentration")

### b)
# For 0,
t.test(s0, n0, var.equal=F) # p-value = 0.03701
# This is rejected for alpha of 0.5. Hence, evidence of difference is strong.

# For 2,
t.test(s2, n2, var.equal=F) # p-value = 0.01275
# This is rejected for alpha of 0.5. Hence, evidence of difference is strong.

# For difference,
t.test(sd, nd, var.equal=F) # p-value = 0.07919
# This is not rejected for alpha of 0.5. Hence, evidence of difference is not strong.

### c)
wilcox.test(s0, n0, pairing=F, exact=F) # p-value = 0.07439
# This is not rejected for alpha of 0.5. Hence, evidence of difference is not strong.
wilcox.test(s2, n2, pairing=F, exact=F) # p-value = 0.01298
# This is rejected for alpha of 0.5. Hence, evidence of difference is strong.
wilcox.test(sd, nd, pairing=F) # p-value = 0.1059
# This is not rejected for alpha of 0.5. Hence, evidence of difference is not strong.

s0 = read.table("vitaminc2a.txt", header=T)[,1]
s1 = read.table("vitaminc2a.txt", header=T)[,2]
n0 = read.table("vitaminc2b.txt", header=T)[,1]
n1 = read.table("vitaminc2b.txt", header=T)[,2]

### d)
# descriptive statistics
summary(s0)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.10    6.30   35.78   32.40  359.30 
summary(n0)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0    22.7   101.9   123.0   117.9   620.4
summary(s1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0100  0.1000  0.5295  0.3975  5.9900
summary(n1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.295   1.500   1.779   1.995   7.950 

# graphical presentation
boxplot(list(s0, n0), names=c("Schizophrenics Total", "Nonschizophrenics Total"), main="Totals")
boxplot(list(s1, n1), names=c("Schizophrenics mg/kg", "Nonschizophrenics mg/kg"), main="mg/kg")

# Histograms to see if data is normally distributed
hist(s0, main="Histogram of Schizophrenics Total")
hist(n0, main="Histogram of Nonschizophrenics Total")
hist(s1, main="Histogram of Schizophrenics mg/kg")
hist(n1, main="Histogram of Nonschizophrenics mg/kg")
# Data is not normally distributed.

### e
# For total,
t.test(s0, n0, var.equal = F) # p-value = 0.06694
# This is not rejected for alpha of 0.05, so evidence of difference is not strong.
# But normality assumption was not reasonable from d).

# For mg/kg,
t.test(s1, n1, var.equal = F) # p-value = 0.05487
# This is not rejected for alpha of 0.05, so evidence of difference is not strong.
# But normality assumption was not reasonable from d).

### f
# For total,
wilcox.test(s0, n0, pairing = F, exact = F) # p-value = 0.01404
# This is rejected for alpha of 0.05, so evidence of difference is strong.

# For mg/kg,
wilcox.test(s1, n1, pairing = F, exact = F) # p-value = 0.01156
# This is rejected for alpha of 0.05, so evidence of difference is strong.

# The result is exactly opposite how it was in e).

sp = read.table("vitaminc3.txt", header=T)[,1]
su = read.table("vitaminc3.txt", header=T)[,2]
cp = read.table("vitaminc3.txt", header=T)[,3]
cu = read.table("vitaminc3.txt", header=T)[,4]

### g
# descriptive statistics
summary(sp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5100  0.6950  0.7700  0.8907  1.1100  1.2800 
summary(su)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.09   27.97   86.20   80.86  111.10  182.10 
summary(cp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.470   0.770   0.860   0.878   1.005   1.380 
summary(cu)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 28.26  130.50  149.80  160.30  187.30  285.30 

# graphical presentation
boxplot(list(sp, cp), names=c("Schizophrenics Plasma", "Control Plasma"), main="Plasma")
boxplot(list(su, cu), names=c("Schizophrenics Urine", "Control Urine"), main="Urine")

### h
t.test(sp, cp, var.equal=F) # 0.8899
# This is not rejected for alpha of 0.05, so evidence of difference is weak.
t.test(su, cu, var.equal=F) # 0.001876
# This is rejected for alpha of 0.05, so evidence of difference is strong.

# Histograms to see if data is normally distributed
hist(sp, main="Histogram of Schizophrenics Plasma")
hist(cp, main="Histogram of Control Plasma")
hist(su, main="Histogram of Schizophrenics Urine")
hist(cu, main="Histogram of Control Urine")
# Schizophrenics is not normally distributed.
# Control data is normally distributed.
# Since both distributions must be normally distributed for normality assumption for t-test, it is not reasonable to assume so.

### i
wilcox.test(sp, cp, exact=F, pairing=F) # p-value=1
# This is not rejected for alpha of 0.05, so evidence of difference is weak.
wilcox.test(su, cu, exact=F, pairing=F) # p-value=0.00323
# This is rejected for alpha of 0.05, so evidence of difference is strong.