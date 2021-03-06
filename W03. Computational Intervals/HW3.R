#Q1 

install.packages("rmarkdown")

rm(list = ls())

library(dplyr)
library(lubridate)

#(a)  Create a normal distribution (mean=940, sd=190) and standardize it (let��s call it rnorm_std)
a <- rnorm(10000, mean=940, sd=190)


rnorm_std <- (a - mean(a))/sd(a)

# (i) What should we expect the mean and standard deviation of rnorm_std to be, and why?
mean(rnorm_std)  
sd(rnorm_std)


#reason:

# The  standard deviation of rnorm_std must be 1 and its mean should be 0 (pretty close).
# Because it's what we called Normalization, which is being used to standardize the 
# the distance of sample data away from the population mean, which is set as 940.
# In brief, it's doing a pararel swift and devide by the standard devietion.
# Therefore, the original mean would not affect the result of the one of normalization.




# ii) What should the distribution (shape) of rnorm_std look like, and why?

ii <- density(rnorm_std)    
plot(ii, main="Distribution (shape) of rnorm_std")

#Reason:

#It's a bell shape. Intuitively, due to the original data is standard bell shape
#the closer the data are to the mean, 
#the greater the amount of data and the lower the residual value.

# iii) What do we generally call distributions that are normal and standardized?
    

#Ans Standard Normal Distribution.


# (b) Create a standardized version of minday discussed in question 3 
#(let��s call it minday_std)

bookings <- read.table("first_bookings_datetime_sample.txt", header=TRUE)
hours  <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins   <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins

sprintf("Orignial: The mean: %f, the standard deviation: %f",mean(minday) ,sd(minday))


standardize <- function(i){
    i <- (i - mean(i)/sd(i))
    return(i)
}


minday_std   =standardize(minday)
# (b-i) What should we expect the mean and standard deviation of minday_std to be, and why?
#Ans: 
sprintf("The mean: %f, the standard deviation: %f",round(mean(minday_std),3) ,sd(minday_std) )

#We should expect that the mean of minday_std is around 942 (mean(minday)) 
#, and standard deviation of minday_std should be about 190. (sd(minday))
# Hence, the sample mean is 937.527000, and the standard deviation is 189.663078

#According to the law of large number, as the rising of sample size, the higer the chance that the sample mean close to 
# the expected value.

# (b-ii) What should the distribution of minday_std look like compared to minday, and why?
par(mfrow = c(1,2)) 
plot(density(minday), main="minday")+abline(v=mean(minday), lwd=2)
plot(density(minday_std), main="minday_std")+abline(v=mean(minday_std), lwd=2)
#Explanation: 

#The reason thay look alike is that the duction of the mean can be seen as a leftward pararlel shift, which would not change the shape of the distribution.
#Then we divide it by the standard deviation only affect the scale. Therefore, the shape of them are alike.


# Question 2) 
# Run visualize_sample_ci(), which simulates samples drawn randomly from a population. 
# sample is a horizontal line with a dark band for its 95% CI, and a lighter band for its 99% CI, and a dot for its mean.  


#Gitub code Start Here
# Visualize the confidence intervals of samples drawn from a population
#   e.g.,
#     visualize_sample_ci(sample_size=300, distr_func=rnorm, mean=50, sd=10)
#     visualize_sample_ci(sample_size=300, distr_func=runif, min=17, max=35)
visualize_sample_ci <- function(num_samples = 100, sample_size = 100, 
                                pop_size=10000, distr_func=rnorm, ...) {
    # Simulate a large population
    population_data <- distr_func(pop_size, ...)
    pop_mean <- mean(population_data)
    pop_sd <- sd(population_data)
    
    # Simulate samples
    samples <- replicate(num_samples, 
                         sample(population_data, sample_size, replace=FALSE))
    
    # Calculate descriptives of samples
    sample_means = apply(samples, 2, FUN=mean)
    sample_stdevs = apply(samples, 2, FUN=sd)
    sample_stderrs <- sample_stdevs/sqrt(sample_size)
    ci95_low  <- sample_means - sample_stderrs*1.96
    ci95_high <- sample_means + sample_stderrs*1.96 
    ci99_low  <- sample_means - sample_stderrs*2.58
    ci99_high <- sample_means + sample_stderrs*2.58
    
    # Visualize confidence intervals of all samples
    plot(NULL, xlim=c(pop_mean-(pop_sd/2), pop_mean+(pop_sd/2)), 
         ylim=c(1,num_samples), ylab="Samples", xlab="Confidence Intervals")
    add_ci_segment(ci95_low, ci95_high, ci99_low, ci99_high,
                   sample_means, 1:num_samples, good=TRUE)
    
    # Visualize samples with CIs that don't include population mean
    bad = which(((ci95_low > pop_mean) | (ci95_high < pop_mean)) |
                    ((ci99_low > pop_mean) | (ci99_high < pop_mean)))
    add_ci_segment(ci95_low[bad], ci95_high[bad], ci99_low[bad], ci99_high[bad],
                   sample_means[bad], bad, good=FALSE)
    
    # Draw true population mean
    abline(v=mean(population_data))
}

add_ci_segment <- function(ci95_low, ci95_high, ci99_low, ci99_high, 
                           sample_means, indices, good=TRUE) {
    segment_colors <- list(c("lightcoral", "coral3", "coral4"),
                           c("lightskyblue", "skyblue3", "skyblue4"))
    color <- segment_colors[[as.integer(good)+1]]
    
    segments(ci99_low, indices, ci99_high, indices, lwd=3, col=color[1])
    segments(ci95_low, indices, ci95_high, indices, lwd=3, col=color[2])
    points(sample_means, indices, pch=18, cex=0.6, col=color[3])
}


visualize_sample_ci(sample_size=300, distr_func=rnorm, mean=50, sd=10)
visualize_sample_ci(sample_size=300, distr_func=runif, min=17, max=35)
#Gitub code end Here

# (a) Simulate 100 samples (each of size 100), from a normally distributed population of 10,000:
#     visualize_sample_ci(num_samples = 100, sample_size = 100, pop_size=10000, 
#                         distr_func=rnorm, mean=20, sd=3)

visualize_sample_ci(num_samples = 100, sample_size = 100, pop_size=10000, 
                    distr_func=rnorm, mean=20, sd=3)

# (a-i) How many samples do we expect to NOT include the population mean in its 95% CI?
#(a-i)-Ans


#we expect (1-0.95)*100 = 5 not include in this 95% C.I.

(1-0.99)*100

# (a-ii) How many samples do we expect to NOT include the population mean in their 99% CI?

#(a-ii)-Ans
#We expected (1-0.99)*100 = 1 not include in this 99% C.I.


#(b)-(i)
#Rerun the previous simulation with the same number of samples, but larger sample size (sample_size=300):


visualize_sample_ci(num_samples = 100, sample_size = 300, pop_size=10000, 
                    distr_func=rnorm, mean=20, sd=3)

#Now that the size of each sample has increased, do we expect their 95% and 99% CI to become wider or narrower than before?

#(i)-Ans 

# Narrower than before

#(b)-(ii) This time, how many samples (out of the 100) would we expect to NOT include the population mean in its 95% CI?
#(ii) -Ans:
# This times, we expected (1-0.95)*300 = 15 not include in this 95% C.I.

# (Unsure)

# (c) If we ran the above two examples (a and b) using a uniformly distributed population 
#(specify distr_func=runif for visualize_sample_ci), 
#how do you expect your answers to (a) and (b) to change, and why?

par(mfrow = c(1,2))
visualize_sample_ci(sample_size =100, distr_func=runif,min = 21, max = 44)+
    title(main = "Uniform_sample_size(100)",col.main = "darkgreen")

visualize_sample_ci(sample_size = 300,distr_func=runif,min = 21, max = 44)+
title(main = "Uniform_sample_size(300)",col.main = "darkgreen")

#Ans: (c)
#As the increasing of sample size, no matter whether it's the normal distribution or uniform distribution,
#the 99% and 95% CI still get narrower.To make it rolling, I set min = 21, and max = 44. 
#The mean of the distribution is (44+21)/2 = 32.5, standard deviation = (44-21)**2/12= 44.083. The calculation of 95%CI and 99% CI is still.
# Therefore, (a), (b) answer in this scenario does not change.

#(Question 3) 
# 3-(a) What is the ��average�� booking time for new members making their first restaurant booking?
#     (use minday, which is the absolute minute of the day from 0-1440)
bookings <- read.table("first_bookings_datetime_sample.txt", header=TRUE)
hours  <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins   <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins

par(mfrow = c(1,1)) 
plot(density(minday), main="Minute (of the day) of first ever booking", col="blue", lwd=2)

mean(minday)
#Ans  942.4964


# i) Use traditional statistical methods to estimate the population mean of minday, 
#its standard error, and the 95% confidence interval (CI) of the sampling means

#(a)- (i-1)estimate �g of minday via sample mean

x <- mean(minday);x  #sample mean
sprintf("The sample mean is  %f ,which can be used to estimate the population mean", x)
#(i-2)sample standard error & the 95% confidence interval (CI) of the sampling means(s)
n <-length(minday)
SE <- sd(minday)/sqrt(n)
sprintf("The sample Standard error: is  %f ", SE)

#Formula: 95%  CI 
z <- c(-1.96,1.96)
CI <- x+z*(s/(n**0.5))
sprintf("CI of sample mean: is  [%f,%f] ", CI[1], CI[2])

# Ans: The CI = [ 941.3208, 943.6719]

#(ii) Bootstrap to produce 2000 new samples from the original sample


compute_sample_mean <- function(sample0) {
    resample <- sample(sample0, length(sample0), replace=TRUE)
    mean(resample)
}

resamples <- replicate(2000,compute_sample_mean(minday))

# (iii) Visualize the means of the 2000 bootstrapped samples
head(resamples)
plot(density(resamples), lwd=2, 
     main="Distribution of resamples",  col = 'red')
abline(v = mean(resamples), lwd = 1, col = 'blue')

plot_resample_density <- function(sample_i) {
    lines(density(sample_i), col=rgb(0.0, 0.35, 0.05, 0.1))
    return(mean(sample_i))
}
m_text <- paste('Mean:', mean(resamples))
text(x = 942.5, y =0.1 , m_text, col = "blue", cex = .65)

# (a-iv) Estimate the 95% CI of the bootstrapped means.

x_ <- mean(resamples);
n_boot <- length(resamples);
s_boot <- sd(resamples)/sqrt(n_boot)
z_boot <- c(-1.96, 1.96)
CI_boot <- x_+(z_boot*s_boot)
sprintf("The 95 percent CI of the bootstrapped means is [ %f, %f ] ",CI_boot[1], CI_boot[2])

# 3-b) By what time of day, have half the new members of the day already arrived at their restaurant?
# 3-b-i) Estimate the median of minday


median(minday)#[1] 1040 this value is incorrect.
wilcox.test(minday, alternative = "two.sided", correct = TRUE, conf.int = TRUE,conf.level = .95)
#correct = true means doing the continually recorrection 

#Summary
# Wilcoxon signed rank test with
# continuity correction
# 
# data:  minday
# V = 4999450015, p-value < 2.2e-16
# alternative hypothesis: true location is not equal to 0
# 95 percent confidence interval:
#     930 930
# sample estimates:
#     (pseudo)median 
# 930 

# 3-b-ii) Visualize the medians of the 2000 bootstrapped samples

# Plot population and original sample densities
plot(density(resamples), col="blue", lty="dashed",lwd=2.5,main = 
        "Median of the  bootstrapped samples")
abline(v=median(resamples), lwd=2.5, col = 'red')

md_text <- paste('Median:', mean(resamples))
text(x = mean(resamples), y =0.2 , md_text, col = "red", cex =1)


# 3-b-iii) Estimate the 95% CI of the bootstrapped medians.

wilcox.test(resamples, alternative = "two.sided", correct = TRUE, conf.int = TRUE,conf.level = .95)

# Wilcoxon signed rank test with
# continuity correction
# 
# data:  resamples
# V = 2001000, p-value < 2.2e-16
# alternative hypothesis: true location is not equal to 0
# 95 percent confidence interval:
#     942.4862 942.5407
# sample estimates:
#     (pseudo)median 
# 942.5133 