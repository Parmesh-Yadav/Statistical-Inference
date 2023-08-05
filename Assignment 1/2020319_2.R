#QUESTION 2

rm(list=ls())
set.seed(123)

# read csv file
x = read.csv("data.csv")

# store the weights
x = x[,c(2)]

mean = mean(x)
sd = sd(x)
variance = var(x)

#PART-A
#MLE ESTIMATE

#LOG LIKELYHOOD FUNCTION
l_log <- function(t){
  sum(dnorm(x, mean = t[1], sd = t[2], log = TRUE))
}

#MLE
mle_par <- optim(c(mean(x), sd(x)), l_log, control = list(fnscale = -1))$par


#PART-B
#LIKELYHOOD FUNCTION ATTAINS MAXIMA AT THE ABOVE ESTIMATE

#WILL TAKE MEAN TO BE THE FIXED MLE VALUE OBTAINED ABOVE
M = mle_par[1]

#VARIOUS VALUES OF VARIANCE
SDs <- seq(3.5,4.5,by = 0.01)

#CALCULATE MLE FOR THESE DIFFERENT VALUES
final <- c()
for(i in SDs){
  a <- l_log(c(M,i))
  final <- c(final,a)
}
#PLOT TO SHOW THAT MAX VALUE OCCURS AT THE MLE CALCULATED IN PART A
plot(SDs,final,type="b",col="red",cex=1.5,pch=3,xlab="Standard Deviation",ylab="Log-Likelilhood",main="Log-Likelihood vs Standard Deviation with fixed mean")

















