

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
y = p
x = v
# Linear regression
B1 = sum((x-mean(x))*(y-mean(y)))
B0 = mean(y)-B1*mean(x)

# Residuals
e = y-B0-B1*x

# Residuals versus volume
plot(v, e, main="Residuals Versus Volume", xlab="Volume(kiloliters)", ylab="Residuals(pascals)")

# The residual plot shows..


### c
# Try ﬁtting pressure as a quadratic function of volume. What do you think of
the ﬁt?

### 43
cysts = read.table("cysts.txt", head=T)
d = cysts$Diameter
t10 = cysts$X10C
n10 = cysts$n10
t25 = cysts$X25C
n25 = cysts$n25
t28 = cysts$X28C
n28 = cysts$n28
# Does the time required appear to be a linear or a quadratic function of diameter? Can you ﬁnd a model that ﬁts?
# plot...?

# How do the settling rates at the three temperatures compare?


### 44
read.table("asthma.txt")
hr = read.table("cystfibr.txt", head=T)
h = hr$height
r = hr$resistance
