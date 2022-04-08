
library(dplyr)
df_1 <- read.csv('pls-media1.csv', header = TRUE, sep = ",")
df_2 <- read.csv('pls-media2.csv', header = TRUE, sep = ",")
df_3 <- read.csv('pls-media3.csv', header = TRUE, sep = ",")
df_4 <- read.csv('pls-media4.csv', header = TRUE, sep = ",")


df = list(df1_intend0=df_1$INTEND.0,
          df2_intend0=df_2$INTEND.0, 
          df3_intend0=df_3$INTEND.0,
          df4_intend0=df_4$INTEND.0);

max_length <- max(length(df_1$INTEND.0),
                  length(df_2$INTEND.0),
                  length(df_3$INTEND.0), 
                  length(df_4$INTEND.0))

attributes(df) = list(names = names(df),
                      row.names=1:max_length,
                      class='data.frame')
head(df)


sapply(df, mean)

mean_vector <- c(mean(df_1[['INTEND.0']]),
                 mean(df_2[['INTEND.0']]), 
                 mean(df_3[['INTEND.0']]), 
                 mean(df_4[['INTEND.0']]))
                   
df_intend0 <- data.frame(mean_vector) ;  
colnames(df_intend0) <- 'mean_intend0'
rownames(df_intend0) <- c('type_1', 'type_2', 'type_3', 'type_4')
df_intend0[1]

#---------------

#------------
#turn into dataframe for the use of ggplot
df = list(df1_intend0=df_1$INTEND.0,
          df2_intend0=df_2$INTEND.0, 
          df3_intend0=df_3$INTEND.0,
          df4_intend0=df_4$INTEND.0); 

max_length <- max(length(df_1$INTEND.0),
                  length(df_2$INTEND.0),
                  length(df_3$INTEND.0), 
                  length(df_4$INTEND.0))

attributes(df) = list(names = names(df),
                      row.names=1:max_length,
                      class='data.frame')
head(df)
tail(df)
#--------


# plot(density(df_1$INTEND.0),main='Density Plot of Media Types ',lwd=2, ylim=c(0,0.3))
# 
# abline(v=df_intend0$mean_intend0[1],lty=1,col="black",lwd=1)
# 
# lines(density(df_2$INTEND.0),lwd=2,col='red')
# abline(v=df_intend0$mean_intend0[2],lty=1,col="red",lwd=1)
# 
# lines(density(df_3$INTEND.0),lwd=2,col='green')
# abline(v=df_intend0$mean_intend0[3],lty=1,col="green",lwd=1)
# 
# lines(density(df_4$INTEND.0),lwd=2,col='purple')
# abline(v=df_intend0$mean_intend0[4],lty=1,col="purple",lwd=1)
# 
# abline(v=df_intend0$mean_intend0,lty=2,col="blue",lwd=1)
# 
# 
# legend(-1.25,0.3, legend=c("Type 1", "Type 2", "Type 3", "Type 4", "mean"),
#        col=c("black", "red", 'green', 'purple', 'blue'), lty=c(1,1,1,1,2), cex=0.8)
# 


#----------
color_set = c('black','green', 'yellow', 'blue' )
plot(1,main='Density Plot of Media Types',lwd=2, xlim = c(0, 8) ,ylim=c(0,0.35) , col=color_set[1])
for(i in 1:length(df)) {
    lines(density(df[[i]]),lwd=2,col= color_set[i])
    abline(v=mean(df[[i]]),lty=2,col= color_set[i] ,lwd=1)
}
legend(x = 0, y = 0.3, legend=colnames(df),col= color_set ,  lty=c(1,1,1,1), cex=0.8)
#----------

mean(sapply(df, mean))
m<-mean(each_mean); m

# sstr1=length(df[,1])*((each_mean [1] - m)^2)
# sstr2=length(df[,2])*((each_mean [2] - m)^2)
# sstr3=length(df[,3])*((each_mean [3] - m)^2)
# sstr4=length(df[,4])*((each_mean [4] - m)^2)

sstr_count <- function(x){
    length(x)*((mean(x) - m)^2)
}

sstr <- as.numeric(sum(sapply(df,sstr_count)))
df_mstr= 4-1

mstr= sstr/df_mstr; mstr

sse <- function(x){
    sum((length(x)-1)*var(x))
}
sapply(df, sse)
df_sse <- sum(sapply(df, sse))
total_n <- length(df[,1])+ length(df[,2]) + length(df[,3]) + length(df[,4])
df_mse <- total_n-length(df) #166-4
mse <- df_sse/df_mse ; mse
F_score<-mstr/mse
F_score
qf(p=0.95,df1=3, df2=162)



x <- seq(0, 1, by = 0.02); x

?plot()

# Calling qf() Function
y <- qf(x, df1 = 2, df2 = 3); 
y
plot(x)
plot(density(x),main='Density Plot of Media Types ',lwd=2, ylim=c(0,1))
abline(density(y),lwd=2,col = 'blue',lwd=1,lty=1)

cutoff<-c(qf(p=0.95,df1=3, df2=162), qf(p=0.99,df1=3, df2=162))
cutoff

?pf
head(df)
library(reshape2)
long_df <-melt(df, id.vars= NULL, variable.name= "Set",value.name= "value")
tail(long_df)
# Run oneway.test()function for one-way ANOVA
# var.equal=FALSE
long_df$Set <- as.factor(long_df$Set)
oneway.test(long_df$value~factor(long_df$Set), var.equal=FALSE)

# Test the sample variance of 4 group
# sum(x - sample mean)/ n-1

sample_variance_test <- function(x){
    df_len <- length(x)-1;
    sum(x - mean(x))/ df_len
}
sapply(df, sample_variance_test)


head(long_df)
#--------

set.seed(1045)
data <- data.frame(group = rep(c("P1", "P2", "P3"), each = 40),
                   values = c(rnorm(40, 0, 3),rnorm (40, 0, 6),rnorm (40, 1, 5)))
head(data)
# one-way ANOVA model
model <- aov(value~Set, data=long_df); model

aov(medias_intention$intention ~factor(medias_intention$media_num)))

summary(model)
onew_test <- oneway.test(long_df$value~factor(long_df$Set), var.equal=TRUE)
onew_test
attributes(onew_test)
onew_test[1]


#----
head(long_df)
summary(long_df)
?aov
anova_model <- aov(Set~value,long_df);
anova_model
TukeyHSD(anova_model, conf.level = 0.01)
#-----
media_list <- list()
media_list$media1 <- (read.csv("pls-media1.csv",header = TRUE))$INTEND.0
media_list$media2 <- (read.csv("pls-media2.csv",header = TRUE))$INTEND.0
media_list$media3 <- (read.csv("pls-media3.csv",header = TRUE))$INTEND.0
media_list$media4 <- (read.csv("pls-media4.csv",header = TRUE))$INTEND.0
media_list
m1 <- data.frame(set=rep("m1",42), value=media_list$media1)
m2 <- data.frame(set=rep("m2",38), value=media_list$media2)
m3 <- data.frame(set=rep("m3",40), value=media_list$media3)
m4 <- data.frame(set=rep("m4",46), value=media_list$media4)
medias_intention <- rbind(m1, m2, m3, m4)
medias_intention

long_df

medias_intention[40:45,2]
long_df[40:45,2]
summary(aov(medias_intention$intention ~factor(medias_intention$media_num)))

m1 <- data.frame(set=rep("m1",length(df$df1_intend0)), intention=df$df1_intend0)
m2 <- data.frame(set=rep("m2",length(df$df2_intend0)), intention=df$df2_intend0)
m3 <- data.frame(set=rep("m3",length(df$df3_intend0)), intention=df$df3_intend0)
m4 <- data.frame(set=rep("m4",length(df$df4_intend0)), intention=df$df4_intend0)
long_df <- rbind(m1, m2, m3, m4)

#---

head(df)
loads_long <- melt(df, id.vars= NULL, na.rm = TRUE, 
                   variable.name = "Set", 
                   value.name = "Count")
loads_long[83:90,2]


#----

head(loads_long)
bartlett.test(Count ~ Set , data = loads_long)
?bartlett.test
df_1
colnames(df_1)

#-----------

head(medias_intention)
dim(medias_intention)


head(medias_intention)
# Rank all the combined values across groups
rank<- rank(long_df$value)

# combine rank into medias_intention
long_df_rk <- cbind(long_df, rank)
head(long_df_rk)
# split the same rank into same group
group_rank <- split(long_df_rk$rank, long_df_rk$set )
group_rank

group_rank %>% {
    sapply(sum)

}
group_ranksum <- sapply(group_rank, sum);
group_rank_length <- sapply(group_rank, length);
N <- sum(group_rank_length)
H <- (12 / (N * (N + 1))) * sum((group_ranksum^2) / group_rank_length) - 3 * (N + 1)
paste("H value is", H)
head(long_df)
