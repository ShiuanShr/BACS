
#110078509
#(a)
# Three normally distributed data sets
d1 <- rnorm(n=100, mean=15, sd=5)
d2 <- rnorm(n=200, mean=30, sd=4)
d3 <- rnorm(n=500, mean=45, sd=5)

# Combining them into a composite dataset
d123 <- c(d1, d2, d3)

# Let’s plot the density function of d123
plot(density(d123), col="blue", lwd=2, 
     main = "Distribution 2")

# Add vertical lines showing mean and median
#mean
abline(v=mean(d123), lwd=3)

#median 
abline(v=median(d123),  lwd=1) #lty="dashed"


#(b) Create a “Distribution 3”: 
#to create a single large dataset (n=800). 

b123 <- rnorm(800, mean= 0, sd = 1); 
plot(density(b123), col="blue", lwd=2, 
     main = "Distribution 3")
#mean
abline(v=mean(b123), lwd=3)

#median with green color
abline(v=median(b123),  lwd=1, col = "green") 

# the line of median and the line of mean are overlapped.



# (c) In general, which measure of central tendency (mean or median) do you think will be more sensitive (will change more) to outliers being added to your data?

#Ans: mean is more sensitive


#Section 2 -(a)
set.seed(110078509)
rdata <-rnorm(n=2000, mean=0, sd=1); head(rdata)
plot(density(rdata), col="blue", lwd=2, 
     main = "(a)")
#mean
abline(v=mean(rdata), lwd=2)
rdata_std = sd(rdata)
#1 standard deviations (68%)
abline(v=0+1*rdata_std,  lwd=1,  lty="dashed",  col="blue") 
abline(v=0-1*rdata_std,  lwd=1,  lty="dashed",  col="blue") 
#2 standard deviations (95%)
abline(v=0+2*rdata_std,  lwd=1,  lty="dashed",  col="yellow")
abline(v=0-2*rdata_std,  lwd=1,  lty="dashed",  col="yellow") 
#3 standard deviations (99.7%)
abline(v=0+3*rdata_std,  lwd=1,  lty="dashed",  col="red")
abline(v=0-3*rdata_std,  lwd=1,  lty="dashed",  col="red") 

#(b) -1
#Q1  
Q1 <- unname(quantile(rdata)['25%'] );Q1 #-0.7121004
rdata_Q1std_dist <- (Q1-mean(rdata) )/ rdata_std ; rdata_Q1std_dist#[1] -0.6965661
#Q2
Q2 <- unname(quantile(rdata)['50%']); Q2 #0.01494514
rdata_Q2std_dist <- (Q2-mean(rdata) )/ rdata_std ; rdata_Q2std_dist #[1] 0.02772567
#Q3
Q3 <- unname(quantile(rdata)['75%']); Q3#[1] 0.6405586

rdata_Q3std_dist <- (Q3-mean(rdata) )/ rdata_std ; rdata_Q3std_dist #[1] 0.6533957

#(C)
set.seed(110078509)
cdata <-rnorm(n=2000, mean=35, sd=3.5); head(rdata)
cdata_std = sd(cdata)
cdata_Q1 <- unname(quantile(cdata)['25%'] );cdata_Q1 #[1] 32.50765
cdata_Q3 <- unname(quantile(cdata)['75%'] );cdata_Q3 #[1] 37.25048
cdata_Q1std_dist <- (cdata_Q1 - mean(cdata))/cdata_std ; cdata_Q1std_dist
#[1] -0.6965661

cdata_Q3std_dist <- (cdata_Q3 - mean(cdata))/cdata_std ; cdata_Q3std_dist
#[1]  0.6533957



# Comparsion 要下啥結論? (c)cdata Q1, Q3 to (b)rdata Q1, Q3
rdata_Q1std_dist; 
cdata_Q1std_dist; 

#
rdata_Q3std_dist
cdata_Q3std_dist


#(d)
d123.std <- sd(d123)
d123_Q1 <- unname(quantile(d123)['25%'] );d123_Q1 
d123_Q3 <- unname(quantile(d123)['75%'] );d123_Q3  

d123_Q1std_dist <- (d123_Q1 - mean(d123))/d123.std ; d123_Q1std_dist

d123_Q3std_dist <- (d123_Q3 - mean(d123))/d123.std ; d123_Q3std_dist


#Comparsion (d)d123  Q1, Q3 to (b)rdata Q1, Q3
d123_Q1std_dist
rdata_Q1std_dist; 
d123_Q3std_dist; 
rdata_Q3std_dist; 

# Section 3
# (a) From the question on the forum, which formula does Rob Hyndman’s 
# answer (1st answer) suggest to use for bin widths/number? 
# Also, what does the Wikipedia article say is the benefit of that formula?
#Ans (a)- 1  Freedman-Diaconis
#It replaces 3.5σ of Scott's rule with 2 IQR, which is less sensitive than the standard deviation to outliers in data.

# (b) 
set.seed(110078509)
rand_data <- rnorm(800, mean=20, sd = 5) 
rand.size <- length(rand_data)
rand.range <- max(rand_data)- min(rand_data)
#b-i. Sturges’ formula
rand.sturge.bin_num <- ceiling(log(rand.size, base = 2))+1
rand.sturge.bin_num #[1] 11
rand.sturge.bin_width <- ceiling(rand.range/rand.sturge.bin_num) 
rand.sturge.bin_width #[1] 4

#note: sturge=> 不適合非常態分布、sample size < 30, 當n =100時，最準


#b-ii. Scott’s normal reference rule (uses standard deviation)

Scott.std <- sd(rand_data) #4
#thouht the formula is sample standard deviation(s) instead of standard deviation.
#In order to follow the the requirement above, we use std to replace the s
rand.Scott.bin_width <- 3.49*Scott_std/ (rand.size^(1/3))
rand.Scott.bin_width #1.919042
rand.Scott.bin_num <- ceiling(rand.range/rand.Scott.bin_width)
rand.Scott.bin_num # 18

#iii. Freedman-Diaconis’ choice (uses IQR)
rand.Freedman.bin_width <- 2 * IQR(rand_data)/(rand.size^(1/3))
rand.Freedman.bin_num <- ceiling(rand.range/rand.Freedman.bin_width)
rand.Freedman.bin_width #[1] 1.467264
rand.Freedman.bin_num #24


#(c) Repeat part (b) but extend the rand_data dataset with some outliers 
#(create a new dataset out_data):
set.seed(110078509)
out_data <- c(rand_data, runif(10, min=40, max=60))
out.size <- length(out_data);out.size #810
out.range <- max(out_data)- min(out_data); out.range #54.24097

#c-i. Sturges’ formula
outdata.sturge.bin_num <- ceiling(log(out.size, base = 2))+1
outdata.sturge.bin_num #[1] 11
outdata.sturge.bin_width <- ceiling(out.range/outdata.sturge.bin_num) 
outdata.sturge.bin_width #[1] 5


#c-ii. Scott’s normal reference rule
outdata.Scott.std <- sd(out_data)

#In order to follow the the requirement above, we use std to replace the s
outdata.Scott.bin_width <- 3.49*outdata.Scott.std/ (out.size^(1/3))
outdata.Scott.bin_width #[1] 2.236739

outdata.Scott.bin_num <- ceiling(out.range/outdata.Scott.bin_width)
outdata.Scott.bin_num #25

#c-iii. Freedman-Diaconis’ choice (uses IQR)
outdata.Freedman.bin_width <- 2 * IQR(out_data)/(out.size^(1/3))
outdata.Freedman.bin_num <- ceiling(out.range/outdata.Freedman.bin_width)
outdata.Freedman.bin_width #[1] 1.483496
outdata.Freedman.bin_num #[1] 37


# From your answers above, in which of the three methods does the bin width (h) 
#change the least when outliers are added (i.e., which is least sensitive to outliers), 
#and (briefly) WHY do you think that is?

#the width before outlier
rand.sturge.bin_width #[1] 4

rand.Scott.bin_width#[1] 1.919042

rand.Freedman.bin_width#[1] 1.467264


#the width after outlier
outdata.sturge.bin_width #[1] 5

outdata.Scott.bin_width#[1] 2.236739

outdata.Freedman.bin_width #[1] 1.483496


#the percentage they changed separately
Sturge.change <- abs(outdata.sturge.bin_width-rand.sturge.bin_width)/rand.sturge.bin_width
Scott.change <- abs(outdata.Scott.bin_width-rand.Scott.bin_width)/rand.Scott.bin_width
Freedman.change<- abs(outdata.Freedman.bin_width-rand.Freedman.bin_width)/rand.Freedman.bin_width

Change_list <- c(Sturge.change, Scott.change, Freedman.change)
names(Change_list) <- c('Sturge', 'Scott', 'Freedman'); Change_list

#Ans: it show that the Freedman changed the least after the ouliers was added

#Explaination:
# Because Sturges is based on sample size and Scott is based on standard deviation. 
#However, Freedman method is based on IQR, which is the idea of median instead of mean.
# Hence, it's less sensitive among the three as the sample size rising due to the adding of outliers.




