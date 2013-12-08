

### 39
vp=read.table("tankvolume.txt", head=T)
v = vp$Volume
p = vp$Pressure

### a
# Plot pressure versus volume. Does the relationship appear linear?
plot(v, p, main="Pressure Versus Volume", xlab="Volume(kiloliters)", ylab="Pressure(pascals)")
# The relationship appears linear.

### b
# Calculate the linear regression of pressure on volume, and plot the residuals
# versus volume. What does the residual plot show?
# Linear regression
fit = lm(p~v)
fit
# B1 (slope): 2316.5
# B0 (intercept): -257.3

# Residuals
e = residuals(fit)

# Residuals versus volume
plot(v, e, main="Residuals (Linear Fit) Versus Volume", xlab="Volume(kiloliters)", ylab="Residuals(pascals)")
abline(h=0)
# The residual plot does not appear to be a horizontal line with y = 0, so linear mode does not fit very well.

### c
# Try fiting pressure as a quadratic function of volume. What do you think of
# the fit?
fit = lm(p~I(v^2)+v)
fit
# Coefficients:
# (Intercept)       I(v^2)           v  
#    -205.00        83.19      2164.03

e = residuals(fit)
# Residuals versus volume
plot(v, e, main="Residuals (Quadratic Fit) Versus Volume", xlab="Volume(kiloliters)", ylab="Residuals(pascals)")
abline(h=0)

plot(v, p, main="Pressure Versus Volume (Quadratic Fit)", xlab="Volume(kiloliters)", ylab="Pressure(pascals)")
lines(v, fitted(fit))
# The fitted line fits the plot very well.

### 43
cysts = read.table("cysts.txt", head=T)
d = cysts$Diameter
t10 = cysts$X10C
n10 = cysts$n10
t25 = cysts$X25C
n25 = cysts$n25
t28 = cysts$X28C
n28 = cysts$n28
# Does the time required appear to be a linear or a quadratic function of diameter? Can you find a model that fits?
fit10linear = lm(t10~d)
plot(d, t10, main="Time(10) vs Diameter (With Linear Model Fit)", xlab="Diameter", ylab="Time")
lines(d, fitted(fit10linear))
plot(d, residuals(fit10linear), main="Residuals (Linear Fit) Versus Volume", xlab="Volume(kiloliters)", ylab="Residuals(pascals)")
abline(h=0)
summary(fit10linear)
# F-statistic: 76.42 on 1 and 5 DF,  p-value: 0.0003245

fit10quadratic = lm(t10~I(d^2)+d)
plot(d, t10, main="Time(10) vs Diameter (With Quadratic Model Fit)", xlab="Diameter", ylab="Time")
lines(d, fitted(fit10quadratic))
plot(d, residuals(fit10quadratic), main="Residuals (Quadratic Fit) Versus Volume", xlab="Volume(kiloliters)", ylab="Residuals(pascals)")
abline(h=0)
summary(fit10quadratic)
# F-statistic: 781.6 on 2 and 4 DF,  p-value: 6.514e-06
# For 10, quadratic fits better than linear. Also, p value for F-statistics is lower for quadratic fit.

fit25linear = lm(t25~d)
plot(d, t25, main="Time(25) vs Diameter (With Linear Model Fit)", xlab="Diameter", ylab="Time")
lines(d, fitted(fit25linear))
plot(d, residuals(fit25linear), main="Residuals (Linear Fit) Versus Volume", xlab="Volume(kiloliters)", ylab="Residuals(pascals)")
abline(h=0)
summary(fit25linear)
# F-statistic: 72.93 on 1 and 5 DF,  p-value: 0.0003625 

fit25quadratic = lm(t25~I(d^2)+d)
plot(d, t25, main="Time(25) vs Diameter (With Quadratic Model Fit)", xlab="Diameter", ylab="Time")
lines(d, fitted(fit25quadratic))
plot(d, residuals(fit25quadratic), main="Residuals (Quadratic Fit) Versus Volume", xlab="Volume(kiloliters)", ylab="Residuals(pascals)")
abline(h=0)
summary(fit25quadratic)
# F-statistic: 617.4 on 2 and 4 DF,  p-value: 1.043e-05 
# For 25, quadratic fits better than linear. Also, p value for F-statistics is lower for quadratic fit.

fit28linear = lm(t28~d)
plot(d, t28, main="Time(28) vs Diameter (With Linear Model Fit)", xlab="Diameter", ylab="Time")
lines(d, fitted(fit28linear))
plot(d, residuals(fit28linear), main="Residuals (Linear Fit) Versus Volume", xlab="Volume(kiloliters)", ylab="Residuals(pascals)")
abline(h=0)
summary(fit28linear)
# F-statistic: 105.7 on 1 and 5 DF,  p-value: 0.0001497 

fit28quadratic = lm(t28~I(d^2)+d)
plot(d, t28, main="Time(28) vs Diameter (With Quadratic Model Fit)", xlab="Diameter", ylab="Time")
lines(d, fitted(fit28quadratic))
plot(d, residuals(fit28quadratic), main="Residuals (Quadratic Fit) Versus Volume", xlab="Volume(kiloliters)", ylab="Residuals(pascals)")
abline(h=0)
summary(fit28quadratic)
# F-statistic: 611.8 on 2 and 4 DF,  p-value: 1.062e-05 
# For 28, quadratic fits better than linear. Also, F-statistics is lower for quadratic fit.

# Since quadratic fit is better for all 10, 25, 28,  the time
# required appears to be a quadratic function of diameter.

# Find the model that fits: we found quadratic models that fit for 10, 25, 28 above.
plot(d, t10, main="Time vs Diameter", xlab="Diameter", ylab="Time", col="red")
points(d, t25, col="blue")
points(d, t28, col="forest green")
# How do the settling rates at the three temperatures compare?
# From the grpah, it is clear that the settling rates is slowest with 10 and highest with green.
# (settling rate is higher if the time needed to settle is lower)

### 44
ahr = read.table("asthma.txt", head=T)
chr = read.table("cystfibr.txt", head=T)
ah = ahr$height
ar = ahr$resistance
plot(ah, ar)
cor(ahr, method="pearson")

ch = chr$height
cr = chr$resistance
plot(ch, cr)
cor(chr, method="pearson")
