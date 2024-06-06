###################################################
# Computer Lab 3 - F71SM
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
# Tutorial 4a, Question 1 
# Confirm the answers 

#(i) 
1 - pbinom(11,20,0.6)
# Note that this can also be computed as
pbinom(11,20,0.6,lower.tail = FALSE)

#(ii)
pbinom(12,20,0.6)


###################################################
## Task 2
# Tutorial 4a, Question 4(a)
# Confirm the answers 

p.more.three = 0.4^3; p.more.three 
# Note that alternatively, we can use pgeom() in R
# But notice that R uses the version of geometric
# distn that corrsponds to "no. of failures before
# first success", i.e. x = 0,1,2,...
# So "more than 3 attempts until passing" is the same 
# as "more than 2 failures before passing", i.e.  
p.more.three.v2 = 1 - pgeom(2,0.6); p.more.three.v2


###################################################
## Task 3
# Tutorial 4a, Question 5
# Confirm the answers 

#Using binom
pbinom(9,310,0.04)
#Or
1-pbinom(300,310,0.96)

#Using Poisson
ppois(9,(310*0.04))


###################################################
## Task 4
# Tutorial 4b, Questions 2(b),(c):
# Confirm the answers 

# X~exp(0.01) tubes
# Y~exp(0.05) device

#(b)
p1 = pexp(40,0.05); p1
p2 = dbinom(2,3,p1) + dbinom(3,3,p1); p2
#Or
1-pbinom(1,3,p1)

#(c)

#Median 

#P(X < mx) = 0.5

qexp(0.5,0.01)
# Check using CDF:
pexp(69.31472,0.01)

qexp(0.5, 0.05)
# Check using CDF:
pexp(13.86294,0.05)

###################################################
## Task 5
# Tutorial 4b, Questions 3(a), (c), (d):
# Confirm the answers 

#(a)
p.ta = pnorm(75,65,4); p.ta

#(c)
p.tb = pnorm(75,64,6); p.tb

#(d)
#TA - Tb ~ N(mean=65-64,var=16+36=52)
p.tatb = pnorm(0,1,sqrt(52)); p.tatb


###################################################
## Task 6
# Tutorial 4b, Question 4(c):
# Confirm the answers 

p.a = ( pnorm(5.01,5.016,0.035) - pnorm(4.99,5.016,0.035) ) / 0.88; p.a
p.b = ( pnorm(4.99,5.016,0.035) - pnorm(4.94,5.016,0.035) ) / 0.88; p.b
p.c = ( pnorm(5.06,5.016,0.035) - pnorm(5.01,5.016,0.035) ) / 0.88; p.c


###################################################
## Task 7
## Write a function in R that plots the graph of the 
## cdf of a specified normal distribution and also returns 
## two specified quantiles of the distribution.
## The parameters of the distribution and the probabilities 
## for the required quantiles should be specified by the user
## when the function is called.

normal.fn <- function(mu=0,sigma=1,p1=0.025,p2){ 
  x = seq(qnorm(0.0001,mu,sigma), qnorm(0.9999,mu,sigma), length.out=200)
  plot(x,pnorm(x,mu,sigma),type="l",ylab="F(x)")
  title(main="Plot of CDF")
  q = qnorm(p=c(p1,p2), mu,sigma)
  #q
  cat("\n", "Q(", p1, ")=", q[1], ",   Q(", p2, ")=", q[2], "\n\n")
}

# e.g.
normal.fn(0,1,0.025,0.975)
abline(h=0.5)

normal.fn(30,9,0.25,0.975)
normal.fn(p2=0.975)





























