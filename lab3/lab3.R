

### 39
vp=read.table("tankvolume.txt", head=T)
v = vp$Volume
p = vp$Pressure

### a
# Plot pressure versus volume. Does the relationship appear linear?
plot(v, p, main="Pressure Versus Volume", xlab="Volume(kiloliters)", ylab="Pressure(pascals)")

# The relationship appears...

### b
# Calculate the linear regression of pressure on volume, and plot the residuals
versus volume. What does the residual plot show?
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
# The residual plot shows..


### c
# Try fiting pressure as a quadratic function of volume. What do you think of
the fit?
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
# I think the fit is... 

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
lines(d, fit10linear)

fit10quadratic = lm(t10~I(d^2)+d)
plot(d, t10, main="Time(10) vs Diameter (With Quadratic Model Fit)", xlab="Diameter", ylab="Time")
lines(d, fit10quadratic)

# Repeat above two more times
# The time required appears to be linear/quadratic function of diameter from the graphs.

# Find the model that fits.. using average?

# How do the settling rates at the three temperatures compare?


### 44
ahr = read.table("asthma.txt", head=T)
chr = read.table("cystfibr.txt", head=T)
ah = ahr$height
ar = ahr$resistance
ch = chr$height
cr = chr$resistance
