Y <- -50:50
plot(dnorm(Y, mean = -5, sd = 4) ~ Y, type = 'l')
points(dnorm(Y, mean = -7, sd = 4) ~ Y, type = 'l', col = 'red')
grid()
legend('topright', c('Treatment C', 'Treatment E'), lty = 1, col = c('black', 'red'), bg = 'white')

n <- 25
meandiff <- replicate(10000, mean(rnorm(n, mean = -7, sd = 4)) - mean(rnorm(n, mean = -5, sd = 4)))
hist(meandiff, freq = FALSE, col = 'grey', main = 'Sampling distribution of the mean difference', xlab = '')
x <- seq(-50, 50, .01)
points(dnorm(x, mean = -7 - -5, sd = sqrt((4^2/n) + (4^2/n))) ~ x, type = 'l', col = 'red')

n <- 25
x <- seq(-10, 20, .01)
plot(dnorm(x, mean = -7 - -5, sd = sqrt((4^2/n) + (4^2/n))) ~ x, type = 'l', col = 'black', main = '', xlab = '')
points(dnorm(x, mean = -5 - -5, sd = sqrt((4^2/n) + (4^2/n))) ~ x, type = 'l', col = 'blue')
grid()
legend('topright', c('Sampling distr. under H0', 'Sampling distr. under expected Ha'), lty = 1, col = c('blue', 'black'))

#1) If we assume that the statistical test is to be performed one-sided with ??=0.025, how large 
#should the observed mean difference in Y be to end up with a statistically significant result 
#(i.e. what is the critical value in this setting)? Hint: you can use the qnorm() function. 
#Add the critical value to the plot above.
?qnorm
qnorm(0.025, mean=0,sd=4) #-7.83985
qnorm(0.025, mean = 0, sd = sqrt((4^2/n) + (4^2/n))) #-2.217446

#2) Determine the power of the test for the expected effect.
d <-  (-7 - -5) ??? sqrt((4^2/n) + (4^2/n))

#power 0.4099896

# 3) Obviously, using n=25 will not provide sufficient power. Run the commands above with various larger values of n. 
# How large should n approximately be to obtain a power of (at least) 0.8?




####################################################################################################
# Exercise 2
####################################################################################################

f1 <- function(n, piE, piC, numsim = 1000){
  sE <- rbinom(numsim, n, piE)
  sC <- rbinom(numsim, n, piC)
  pvals <- sapply(1:numsim, FUN = function(i) prop.test(x = c(sE[i], sC[i]), n = c(n, n), alternative = "less")$p.value)
  mean(pvals < 0.025)
}

#It generates 1000 samples for both treatment arms under the assumed alternative hypothesis 
#(note: feel free to increase numsim where necessary). Then, it determines whether the null 
#hypothesis is to be rejected or not. The proportion of rejections equals the power, 
#which is returned by the function. Make sure to understand each of the commands in the function.

?rbinom #rbinom(n, size, prob), rbinom generates random deviates


#1) Run f1() for n=100,200,300,...,2000. Store the output and plot the power curve. 
#Note: warning messages due to low expected values can be ignored. 
#Based on this initial assessment, 
#how many subjects are (approximately) needed in each treatment arm to obtain a power of 0.80?

  
piE <- 0.35
piC <- 0.4
seq(100,2000,by=100)
y <- NULL
for (n in seq(100,2000,by=100)){
 y[n] <- (f1(n, piE, piC, numsim = 1000))
  }

f1(100, piE, piC, numsim = 1000) #0.092
f1(200, piE, piC, numsim = 1000) #0.146
f1(300, piE, piC, numsim = 1000) #0.222

y <- y[seq(100,2000,by=100)]
y
plot(seq(100,2000,by=100),y)

# 2) The function f1() can also be used to obtain the empirical type 1 error rate. 
# Run f1() again, this time to determine the empirical type 1 error rate, for the same values of n. 
# Again, generate a plot to visualize the results.


#type I error (or error of the first kind) is the incorrect rejection of a true null hypothesis. 
# if the null hypothesis is true piE must be the sam as piC, so both 0.4
z <- NULL
for (n in seq(100,2000,by=100)){
  z[n] <- (f1(n, 0.4, 0.4, numsim = 1000))
}
z <- z[seq(100,2000,by=100)]
z
plot(seq(100,2000,by=100),z)


#3) Now suppose that, for ethical and/or logistical reasons, 
#the investigator wants to be able to halt the trial if the observed difference already is 
#significant after observing the outcomes for half of the subjects (if it is not significant at that 
#point, the trial is to be continued until all 2n subjects are included). Assuming that the same 
#?? of 0.025 is used in both tests? Adjust f1() to represent this sequential testing scenario, and 
#repeat the exercises above. What would be the impact on the power and type 1 error?


4) What is the impact of choosing ??=0.001 for the interim analysis (i.e. only halting the trial early if the evidence in favour of the experimental treatment is very clear)?