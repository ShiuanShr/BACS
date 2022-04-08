#HW4_R
## Q1. Given the critical DOI score that Google uses to detect malicious apps
#(a)

pnorm(-3.7)


#(b)
2.2*1000000*pnorm(-3.7)

## Question 2) 


# a. The Null distribution of t-values:
#     
#i. Visualize the distribution of Verizon¡¦s repair times, marking the mean with a vertical line

raw <- read.csv("verizon.csv", header = T) 

plot(density(raw$Time), lwd=2, col="blue", 
     main="distribution of Verizon¡¦s repair times")

abline(v=mean(raw$Time), lwd=2, col="red")


# ii. Given what PUC wishes to test,
#how would you write the hypothesis? (not graded)


# PUC wants to test whether Verizontake average 7.6 minutes to repair phone services for its clients.
# And they intend to verify this claim at 99% confidence. 
# H0: mean=7.6 min, H1: mean!= 7.6 min.



