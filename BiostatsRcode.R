################################################################################
# Exercise 2
################################################################################

f1 <- function(n, piE, piC, numsim = 1000){
  sE <- rbinom(numsim, n, piE)
  sC <- rbinom(numsim, n, piC)
  pvals <- sapply(1:numsim, FUN = function(i) prop.test(x = c(sE[i], sC[i]), n = c(n, n),
                                                        alternative = "less")$p.value)
  mean(pvals < 0.025)
}

#It generates 1000 samples for both treatment arms under the assumed alternative
#hypothesis (note: feel free to increase numsim where necessary). Then, it
#determines whether the null hypothesis is to be rejected or not. The proportion
#of rejections equals the power, which is returned by the function. Make sure to
#understand each of the commands in the function.

?rbinom #rbinom(n, size, prob), rbinom generates random deviates

#####QUESTION 1######

#Run f1() for n=100,200,300,...,2000. Store the output and plot the power curve.
#Note: warning messages due to low expected values can be ignored. Based on this
#initial assessment, how many subjects are (approximately) needed in each
#treatment arm to obtain a power of 0.80?
  
piE <- 0.35
piC <- 0.4
numbs <- seq(100, 2000, by=100)
y <- NULL
for (n in numbs){
 y[n] <- (f1(n, piE, piC, numsim = 1000))
}

f1(100, piE, piC, numsim = 1000) #0.092
f1(200, piE, piC, numsim = 1000) #0.146
f1(300, piE, piC, numsim = 1000) #0.222

y <- y[numbs]
y

plot(numbs, y)
abline(h=0.8, col="red")


#####QUESTION 2######

#The function f1() can also be used to obtain the empirical type 1 error rate.
#Run f1() again, this time to determine the empirical type 1 error rate, for the
#same values of n. Again, generate a plot to visualize the results.

#type I error (or error of the first kind) is the incorrect rejection of a true
#null hypothesis. if the null hypothesis is true piE must be the sam as piC, so
#both 0.4

z <- NULL
for (n in numbs){
  z[n] <- (f1(n, 0.4, 0.4, numsim = 1000))
}
z <- z[numbs]
z
plot(numbs, z)


#####QUESTION 3######

#Now suppose that, for ethical and/or logistical reasons, the investigator wants
#to be able to halt the trial if the observed difference already is significant
#after observing the outcomes for half of the subjects (if it is not significant
#at that point, the trial is to be continued until all 2n subjects are
#included). Assuming that the same alpha of 0.025 is used in both tests? Adjust
#f1() to represent this sequential testing scenario, and repeat the exercises
#above. What would be the impact on the power and type 1 error?

f2 <- function(n, piE, piC, numsim = 1000, int.alpha=0.025){
  sE <- rbinom(numsim, (0.5*n), piE)
  sC <- rbinom(numsim, (0.5*n), piC)
  pvals <- sapply(1:numsim, FUN = function(i) prop.test(x = c(sE[i], sC[i]), n = c((0.5*n), (0.5*n)),
                                                        alternative = "less")$p.value)
  
  n.signif <- mean(pvals < int.alpha)
  fulltrial <- sum((pvals > int.alpha) == TRUE)

  nonsig <- numeric(0)
  
  for(i in 1:numsim){
    if(pvals[i] < int.alpha){
     nonsig[i] <- 0
    }
    else{
     nonsig[i] <- 1
    }
  }
  
  r.sE <- nonsig*sE
  r.sE <- r.sE[!r.sE %in% 0]
  sE2 <- rbinom(fulltrial, (0.5*n), piE)
  sEt <- (r.sE+sE2)
  
  r.sC <- nonsig*sC
  r.sC <- r.sC[!r.sC %in% 0]
  sC2 <- rbinom(fulltrial, (0.5*n), piC)
  sCt <- (r.sC+sC2)

  
  pvals2 <- sapply(1:length(sEt), FUN = function(i) prop.test(x = c(sEt[i], sCt[i]), n = c(n, n),
                                                        alternative = "less")$p.value)
  n.signif.retrial <- mean(pvals2 < 0.025)
  n.signif2 <- (n.signif.retrial*(fulltrial/numsim))
  
  n.signif + n.signif2
}

f2(1500, piE, piC, numsim = 1000)
f1(1500, piE, piC, numsim = 1000)
#Power is slightly higher on f2.
#simulated 1000x
#f1 mean power = .7967
#f2 mean power = .8181

piE <- 0.35
piC <- 0.4
seq(100,2000,by=100)
y <- NULL
for (n in seq(100,2000,by=100)){
  y[n] <- (f2(n, piE, piC, numsim = 1000))
}

y <- y[seq(100,2000,by=100)]
y

plot(numbs, y)
abline(h=0.8, col="red")

#####QUESTION 4######

#What is the impact of choosing a=0.001 for the interim analysis (i.e. only
#halting the trial early if the evidence in favour of the experimental treatment
#is very clear)?


piE <- 0.35
piC <- 0.4
y <- NULL
for (n in numbs){
  y[n] <- (f2(n, piE, piC, numsim = 1000,int.alpha=0.001))
}

y <- y[numbs]
y

plot(numbs, y)
abline(h=0.8, col="red")