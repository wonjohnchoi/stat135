### Wonjohn Choi

fam = read.csv("~/../Google Drive/stat135/lab1/families.csv")
fs = nrow(fam)

### Problem 1
"Problem 1"
ss = 500
si =sample(1:fs, 500)
sam = fam[si,]

# the proportion of heads of household who did not receive a high school diploma.
p = sum(sam$EDUCATION <= 38) / ss
p

# the estimated standard error of this estimate
se = sqrt(p * (1 - p) / (ss - 1) * (1 - ss/fs))
se

# form a 95% condence interval.
ci = c(p - 1.96 * se, p + 1.96 *se)
ci

### Problem 2
"Problem 2"
ss = 400
samidx = replicate(100, sample(1:fs, ss))

# a) For each sample, find the average family income.
"a)"
ai = apply(samidx, 2, function(idx) mean(fam$INCOME[idx]))

# b)
"b)"
# the average of these 100 estimates
aai = mean(ai)
aai
# the standard deviation of these 100 estimates
sai = sd(ai) # TODO(wonjohn): may need to adjust this value 
sai
# make a histogram of the estimates
hist(ai, main="Histogram of the Estimates (and Normal Curve with their Mean and SD)", xlab="Values of Estimates", freq=F)

# c) Superimpose a curve of normal density with the mean and standard deviation of the histogram and comment on the t.
"c)"
x = seq(36000, 46000, length=10000)
pdf = dnorm(x, mean=aai, sd=sai)
lines(x, pdf, col = 'red')
legend(36500, 0.00025, legend=c("histogram","normal pdf"), fill=c("black", "red"))
# Comment on the fit
# The curve of normal density fits the histogram very well.

# d) Plot the empirical CDF (see section 10.2).
"d)"
x = seq(36000, 46000, length=10000)
ecdf = sapply(x, function(xx) {sum(ai <= xx)/length(ai)})
plot(x, ecdf, type = "l", lwd = 1, xlab="Values of Estimates", main="Empirical CDF of Income Estimates (and CDF with their Mean and SD)")
# On this plot, superimpose the normal cumulative distribution function with mean and sd as dened in (c).
cdf = pnorm(x, mean=aai, sd=sai)
lines(x, cdf, col='red')
legend(36500, 0.8, legend=c("empirical cdf","normal cdf"), fill=c("black", "red"))
# Comment on the fit.
# The curve of normal cdf fits the curve of empirical cdf very well.

# e) Another way of examining a normal approximation is via normal probability plots (section 9.9)
# Make such a plot.
plot(sapply(seq(0,1,length=100), qnorm), sort(ai), xlab="Normal Quantiles", ylab="Values of Estimates", main="Normal Probability Plot of Estimates")
# Comment on what it shows about the approximation.
Sincce the plot roughly forms a straight line, the approximation is normally distributed.

# f)
"f)"
# For each of the 100 samples, find a 95% confidence interval for the population average income.
se = function(idx) sqrt(var(fam$INCOME[idx]) / ss * (1 - ss/fs))
# start values of 95% confidence interval
ci_start = apply(samidx, 2, function(idx) { mean(fam$INCOME[idx]) - 1.965 * se(idx) })
# end values of 95% confidence interval
ci_end = apply(samidx, 2, function(idx) { mean(fam$INCOME[idx]) + 1.965 * se(idx) })
# How many of the intervals actually contain the population target?
sum(ci_start <= mean(fam$INCOME) & mean(fam$INCOME) <= ci_end)

### Problem 3
"Problem 3"
# Take a simple random sample of size 500 and compare the incomes of the three family types by
# comparing histograms and boxplots (section 10.6).

sam = fam[sample(1:nrow(fam),500),]
sams = list(
  sam[sam[1]==1,]$INCOME,
  sam[sam[1]==2,]$INCOME,
  sam[sam[1]==3,]$INCOME
)
hist1=hist(sams[[1]], freq=F)
hist2=hist(sams[[2]], freq=F)
hist3=hist(sams[[3]], freq=F)
max_x = max(sapply(sams, function(sam_inc) max(sam_inc)))
min_x = min(sapply(sams, function(sam_inc) min(sam_inc)))
max_y = max(c(hist1$density,hist2$density,hist3$density)) 
par(mfrow=c(1,3))
hist(sams[[1]], freq=F, ylim=c(0,max_y), xlim=c(min_x, max_x), xlab="income", main="Histogram of Income of Type1")
hist(sams[[2]], freq=F, ylim=c(0,max_y), xlim=c(min_x, max_x), xlab="income", main="Histogram of Income of Type2")
hist(sams[[3]], freq=F, ylim=c(0,max_y), xlim=c(min_x, max_x), xlab="income", main="Histogram of Income of Type3")
par(mfrow=c(1,1))
# Only type1 has some families with income over 100000, so it may be that two people in this familiy earns money.
# Type3 has the most families with low incomes ranging from 0 to 50000.
# Type2's families are not as rich as those of type1 but not as poor as those of type3.
par(mfrow=c(1,3))
boxplot(sams[[1]], main="Box plot of incomes of familiy type1", lim=c(min_x, max_x))
boxplot(sams[[2]], main="Box plot of incomes of familiy type2", lim=c(min_x, max_x))
boxplot(sams[[3]], main="Box plot of incomes of familiy type3", lim=c(min_x, max_x))
par(mfrow=c(1,1))
# The boxplot supports my comment based on histograms.

### Problem 4
"Problem 4"
# a)
"a)"
boxplot(sams, main="Parallel box plot of incomes of each familiy type")
# The boxplot supports my comment based on histograms.


# b)
"b)"