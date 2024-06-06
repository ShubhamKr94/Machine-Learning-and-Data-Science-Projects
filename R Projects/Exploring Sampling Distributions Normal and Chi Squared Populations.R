###################################################
# Computer Lab 4 - F71SM
###################################################

## In the tasks below, fill in any missing code as 
## required.
##
## Make sure you use the help menus in R (e.g."?mean" 
## will open up a help window for "mean()").
##
## In some cases, the numerical answers you will find 
## may be slightly different from those in the tutorial 
## answers, due to rounding in intermediate steps.

###################################################
## Task 1
# Sampling distn for X.bar - Normal population
# See also notes, Ch6 p.3

#(i)
# Generate 500 samples of size n=30 from N(10,4) population
# For each sample compute the sample mean
# Plot a histogram of the sample means
# Compute the mean and variance of the sampling distribution
# and compare them with their theoretical values

nsim = 500	# No. of simulated samples
n = 30 	# Sample size
xbar.30 = numeric(nsim)	# Initialise vector
for (i in 1:nsim){
	x = rnorm(n,10,2) 
	xbar.30[i] = mean(x)
}
hist(xbar.30,xlim=c(5,15),freq=F)	
# freq=F used to produce bars with proportional areas 
# this will be useful later
mean(xbar.30); var(xbar.30)

# Repeat code above (including the 'for' loop) to see the randomness 
# in the sampling distribution


#(ii)
# Repeat the exercise in part (i) for sample size n=100

nsim = 500	# No. of simulated samples
n = 100 	# Sample size
xbar.100 = numeric(nsim)	# Initialise vector
for (i in 1:nsim){
	x = rnorm(n,10,2) 
	xbar.100[i] = mean(x)
}
hist(xbar.100,xlim=c(5,15),freq=F)	
mean(xbar.100); var(xbar.100)


#(iii)
# Add the theoretical pdf of the distribution of Xbar on the histogram
# Remember, this should be a N(10, 4/100) distribution (why?).
# Comment

hist(xbar.100,xlim=c(5,15),freq=F,main="")	 # Draw histogram again
title(main="Histogram of Xbar and pdf of N(10,0.04)")
x.grid = seq(9,11,length=200)	 # Grid of values on ax axis
lines(x.grid,dnorm(x.grid,10,sqrt(0.04)),col="red",lwd=2)	# Draw pdf



###################################################
## Task 2
# Sampling distn for X.bar - Chi-squared population
# See also notes, Ch6 p.6

#(i)
# Repeat part (ii) from Task 1, by generating 500 samples 
# of size n=100 from Chi.Squared(4) population

nsim = 500	# No. of simulated samples
n = 100 	# Sample size
xbar.100 = numeric(nsim)	# Initialise vector
for (i in 1:nsim){
	x = rchisq(n,4) 
	xbar.100[i] = mean(x)
}
hist(xbar.100,xlim=c(2,8),freq=F)	
mean(xbar.100); var(xbar.100)

#(ii)
# Repeat part (iii) from Task 1.
# What is the distribution of Xbar now?

hist(xbar.100,xlim=c(2,8),freq=F,main="")	 # Draw histogram again
title(main="Histogram of Xbar and pdf of N(4,8/100)")
x.grid = seq(2.5,5.5,length=200)	 # Grid of values on x axis
lines(x.grid,dnorm(x.grid,4,sqrt(8/100)),col="red",lwd=2)	# Draw pdf


