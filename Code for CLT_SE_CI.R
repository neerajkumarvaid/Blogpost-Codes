#This file contains code for (1) random sampling from Beta distribution, 
#(2) plotting histograms and density functions, 
#(3) Computing descriptive statistics such as mean and standard deviation of a random sample 
#and (4) computing inferential statistics such as standard error of the mean and confidence intervals. 
#Please check the accompanying Medium blogpost here (https://urlzs.com/3GVMy
).

# Written by Neeraj Kumar

# Generate a population distribution
populationdistribution <- rbeta(10000,5,2)*100 # Beta distribution represents the marks of students

# Colored Histogram with marks in bins of 10
hist(populationdistribution, 
     breaks=100, 
     col="red",
     main = "Probability distribution",
     xlab = "Marks", ylab = "# of students", freq = FALSE)
# Overlay density estimate over the histogram
#lines(density(populationdistribution), col = "blue", lwd = 3)

# Compute population mean
mu <-  mean(populationdistribution)
stdev <- sd(populationdistribution)

# Obtaining multiple random samples from the population distribution
n <- 80 # number of samples to be drawn
r <- 50 # number of students in each random sample

mu_samples <- rep(NA,n)

for (i in 1:n){
  random_sample <- sample(populationdistribution, size = r)
  mu_samples[i] <-  mean(random_sample)
}

# Colored Histogram of means of sample distributions
hist(mu_samples, 
     col="red",
     main = "Probability distribution",
     ylim = c(0,0.18),
     xlab = "Means of random samples", ylab = "Frequency", freq = FALSE)
# Overlay density estimate over the histogram
lines(density(mu_samples), col = "blue", lwd = 3)
abline(v=mean(mu_samples), col='yellow', lwd = 2, lty = 2)
abline(v=mu, col='green', lwd = 2)

# Using a single random sample with large n
n <- 1 # number of samples to be drawn
r <- 4000 # number of students in each random sample
random_sample <- sample(populationdistribution, size = r)
# Colored Histogram of sample distribution
hist(random_sample, 
     col="red",
     main = "Probability distribution",
     #ylim = c(0,0.18),
     xlab = "Marks", ylab = "Frequency", freq = FALSE)
# Overlay density estimate over the histogram
lines(density(random_sample), col = "blue", lwd = 3)
abline(v=mean(random_sample), col='yellow', lwd = 2, lty = 2)
abline(v=mu, col='green', lwd = 2)

# Compute standard error and 95% confidence intervals (CI)
# calculate sample mean and standard deviation
mu_estimate <-  mean(random_sample)
s = sd(random_sample)
standard_error  <-  s/sqrt(length(random_sample))

left_CI <-  mu_estimate - 1.96*standard_error
right_CI <- mu_estimate - 1.96*standard_error




