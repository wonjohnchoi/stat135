

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
fit10linear
# Coefficients:
# (Intercept)            d  
#       384.8        -16.3  

fit10quadratic = lm(t10~d + I(d^2))
fit10quadratic
# Coefficients:
# (Intercept)            d       I(d^2)  
#    776.379      -67.334        1.608




# How do the settling rates at the three temperatures compare?


### 44
read.table("asthma.txt")
hr = read.table("cystfibr.txt", head=T)
h = hr$height
r = hr$resistance
