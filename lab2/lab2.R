### Wonjohn Choi (SID: 23123143, GSI: Nanyu)

long=read.csv("long.csv")
short=read.csv("short.csv")
medium=read.csv("medium.csv")

### c. What are the approximate variances of the mle and the method of moments estimate?
# Since we just need to calculate an "approximate" variances, we simply generate a lot of samples of size 1 from Rayleigh distributions and use them to compute mle's and mom's and calculate their variances.
# Generate a sample from Rayleigh distribution
genSamRay = function() {
  
}