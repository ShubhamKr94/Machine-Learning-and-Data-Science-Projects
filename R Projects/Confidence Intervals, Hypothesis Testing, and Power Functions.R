###################################################
# Computer Lab 5 - F71SM
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
# Example 8.1
# Compute 95% CI for mean and for variance

sum.x = 4035; sum.x.sq = 857115; n = 20
# First vcompute sample mean, sample variance and check answers
x.bar = sum.x/n; x.bar
s.squared = (sum.x.sq - sum.x^2/n) / (n-1); s.squared

# Compute CI for mean
ci.lower = x.bar + qt(0.025,(n-1))*sqrt(s.squared/n)
ci.upper = x.bar + qt(0.975,(n-1))*sqrt(s.squared/n)
ci.mean = c(ci.lower, ci.upper)
ci.mean
# or (compute both endpoints of CI at the same trime)
ci.mean = x.bar + c(-1,1)*qt(0.975,(n-1))*sqrt(s.squared/n)
ci.mean

# and CI for variance
ci.var = (n-1)*s.squared / c(qchisq(0.975,(n-1)), qchisq(0.025,(n-1)))
ci.var

# Note that the CI for the mean can also be computed using 
# the built-in R function t.test(). However, for this we need 
# to have the original detailed data available.
# Use ?t.test in R for help
# See also Task 6 later.

###################################################
## Task 2
# Examples 8.2, 9.9
# CI and hypothesis test for binomial proportion

##(i) 8.2: Compute 95% CI for theta, when n=1200, x=420
n=1200; x=420
theta.hat = x/n
ci.theta = theta.hat + c(-1,1)*qnorm(0.975)*sqrt(theta.hat*(1-theta.hat)/n)
ci.theta

# Note that R has a built-in function for computing an "exact" CI 
# for a binomial proportion. Try the following in R:
binom.test(x, n, conf.level = 0.95)$conf.int
binom.test(x, n, conf.level = 0.95)

##(ii) 9.9: Test H0: theta = 0.38, v H1: theta not.equal 0.38 
## using same data as above
binom.test(x, n, p = 0.38, alternative = c("two.sided"), conf.level = 0.95)
# Check the output and report the p-value of the test

# Now compute the approximate p-value manually in R
2*pnorm(420.5,n*0.38,sqrt(n*0.38*(1-0.38)))


###################################################
## Task 3
# Example 8.3
# 95% CI for Poisson mean

lambda.hat = 442/36
ci.lambda = lambda.hat + c(-1,1)*qnorm(0.975)*sqrt(lambda.hat/36)
ci.lambda

# Again, R has a built-in function for computing an "exact" CI 
# for a Poisson mean. Try poisson.test() in R:
poisson.test(x=442, T=36, conf.level = 0.95)$conf.int


###################################################
## Task 4
# Examples 8.7, 9.5
# CI and hypothesis test for two means and two variances

##(i) 8.7: 95% CI for two means
x1.bar = 7.986; s1.squared = 1.4392; n1 = 10
x2.bar = 6.159; s2.squared = 3.9449; n2 = 13
s2.p = 2.871
ci.means = (x1.bar-x2.bar) + c(-1,1)*qt(0.975,21)*sqrt(s2.p*(1/n1 + 1/n2))
ci.means

## 8.7 95% CI for variances
ci.vars = (s1.squared/s2.squared) / c( qf(0.975,n1-1,n2-1),  qf(0.025,n1-1,n2-1)) 
ci.vars

##(ii) 9.5 Test H0: mu1=mu2, v H1: not.equal using same data as above
## Compute the p-value of the test
test.t = (x1.bar-x2.bar)/sqrt(s2.p*(1/n1 + 1/n2))
test.t
p.val = 2*(1-pt(test.t,(n1+n2-2))); p.val


###################################################
## Task 5
# Examples 8.8, 9.6
# CI and hypothesis test for two proportions

##(i) 8.8: 95% CI for two proportions
# Use prop.test() in R with 'correct=FALSE'
prop.test(x=c(54,23), n=c(300,200), conf.level = 0.95, correct = FALSE)$conf.int

# Now calculate a 99% CI and comment
prop.test(x=c(54,23), n=c(300,200), conf.level = 0.99, correct = FALSE)$conf.int
# The CI is now wider and includes the value 0

##(ii) 9.6: Test H0: theta1 = theta2, v H1: not.equal
## (use prop.test() again)
## Report the p-value and comment
prop.test(x=c(54,23), n=c(300,200), correct = FALSE)$p.value
#p-value = 0.0485311, so reject H0 at 5% but not at levels lower than 4.8%


###################################################
## Task 6
# Example 8.10
# Compute a 95% CI for difference in means with these paired data

# First enter the data
before = c(46, 58, 75, 81, 47, 35, 93, 28, 35, 21)
after = c(37, 52, 62, 75, 45, 30, 88, 26, 37, 15)
# Compute differences
d = before - after

# Compute CI
ci.means.d = mean(d) + c(-1,1)*qt(0.975,9)*sqrt(var(d)/10)
ci.means.d

# Alternatively use t.test() for two samples
# Use the help menu ?t.test for how to do this
# Check that you get exactly the same answer as above
t.test(x=before, y= after,alternative = c("two.sided"),paired = TRUE, conf.level = 0.95)


###################################################
## Task 7
# Example 9.4
# Test for binomial proportion
# Use n=100, x=40
# Test H0: theta = 0.5 v. H1: theta < 0.5

##(i) Compute exact p-value
pbinom(q=40, size = 100, prob=0.5)

##(ii) Compute approximate p-value with normal approximation
## Use continuity correction. Comment
pnorm(40.5, mean=100*0.5, sd=sqrt(100*0.5*0.5))
# Approximate p-value is very close to exact



###################################################
## Task 8
# Example 9.3
## Write a function in R that plots the power function 
## of the test in 9.3 as in p.4 of Ch9.
## The value of the mean under H0 (mu0), the sample size (n)
## and the significance level (alpha) should be specified by 
## the user when the function is called.
## see also Lab 3, Task 7 for a plotting function.

power.fn <- function(mu0,n,alpha){ 
	mu.grid = seq(0,3, length.out=200)
	pow = 1 - pnorm( (mu0-mu.grid)*sqrt(n) + qnorm(1-alpha) )
	plot(mu.grid,pow,type="l",ylab="power")
	title(main="Power function")
}

# e.g.
power.fn(1,9,0.05)

## Modify the function above, to also compute the power
## of the test at specific values of mu (given by the user
## when the funtion is called.

power.fn2 <- function(mu,mu0,n,alpha){ 
	mu.grid = seq(0,3, length.out=200)
	pow = 1 - pnorm( (mu0-mu.grid)*sqrt(n) + qnorm(1-alpha) )
	plot(mu.grid,pow,type="l",ylab="power")
	title(main="Power function")
	pow.mu = 1 - pnorm( (mu0-mu)*sqrt(n) + qnorm(1-alpha) )
	cat("\n", "Power(", mu, ")=", pow.mu,  "\n\n")
}

## Compute the power at values mu = 1, 1.2, 2.2
## Comment
power.fn2(mu=1,mu0=1,9,0.05)
power.fn2(mu=1.2,mu0=1,9,0.05)
power.fn2(mu=2.2,mu0=1,9,0.05)
# The power at mu=mu0=1 is the significance level (prob of type I error)
# Then, as mu0 is further away from the true value mu, the power increases.























