### Wonjohn Choi

fam = read.csv("~/stat135/families.csv")
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
ci = c(p - 1.965 * se, p + 1.965 *se)
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
hist(ai, main="Histogram of the Estimates", xlab="Value of Estimate")

# c) Superimpose a curve of normal density with the mean and standard deviation of the histogram and comment on the t.
"c)"
x = seq(38000, 46000, length=1000)
y = dnorm(x, mean=aai, sd=sai)
plot(x, y, type="l", lwd=1)
# TODO(wonjohn): fix above and find what comment on the t is.

# d) Plot the empirical CDF (see section 10.2). On this plot, superimpose the normal cumulative distribution function with mean and sd as dened in (c). Comment on the t.
"d)"
cdf = pnorm(x, mean=aai, sd=sai)
plot(cdf, type="l", lwd=1)

# e) Another way of examining a normal approximation is via normal probability plots (section 9.9). Make such a plot, and comment on what it shows about the approximation.
"e)"
# TODO(wonjohn): do this.

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
sam = fam[sample(1:fs, 500),]
summary(sam[sam$TYPE == 1,]$INCOME)
summary(sam[sam$TYPE == 2,]$INCOME)
summary(sam[sam$TYPE == 3,]$INCOME)

### Problem 4
"Problem 4"
# a)
"a)"


# b)
"b)"