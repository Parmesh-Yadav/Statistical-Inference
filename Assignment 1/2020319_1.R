#QUESTION 1
rm(list=ls())
set.seed(123)


#PART A
#SIMULATE RANDOM SAMPLE OF SIZE 1000 FROM EXPONENTIAL DISTRIBUTION

#LAMBDA = 1
x1 <- rexp(1000, rate = 1)
#LAMBDA = 2
x2 <- rexp(1000, rate = 2)
#LAMBDA = 3
x3 <- rexp(1000, rate = 3)
#LAMBDA = 4
x4 <- rexp(1000, rate = 4)

#PART B
#MLE ESTIMATE

#NEGATIVE LOG LIKELYHOOD FUNCTION
l_log_neg <- function(x,lbda){
  -sum(dexp(x,rate = lbda,log = T))
}

#METHOD OF MOMENTS
x1_mom <- 1/mean(x1)
x2_mom <- 1/mean(x2)
x3_mom <- 1/mean(x3)
x4_mom <- 1/mean(x4)

#CREATING 3 DIFFERENT SETS
mom = c(x1_mom,x2_mom,x3_mom,x4_mom)
set2 = c(0.5,1.5,2.5,3.5)
set3 = c(1,2,3,4)

#MLE - 3 DIFFERENT SET OF VALUES WHERE ONE IS DERIVED BY MOM
x1_mle_1mom <- optim(par = mom, fn = l_log_neg, x = x1)$par
x1_mle_2 <- optim(par = set2, fn = l_log_neg, x = x1)$par
x1_mle_3 <- optim(par = set3, fn = l_log_neg, x = x1)$par

x2_mle_1mom <- optim(par = mom, fn = l_log_neg, x = x2)$par
x2_mle_2 <- optim(par = set2, fn = l_log_neg, x = x2)$par
x2_mle_3 <- optim(par = set3, fn = l_log_neg, x = x2)$par

x3_mle_1mom <- optim(par = mom, fn = l_log_neg, x = x3)$par
x3_mle_2 <- optim(par = set2, fn = l_log_neg, x = x3)$par
x3_mle_3 <- optim(par = set3, fn = l_log_neg, x = x3)$par

x4_mle_1mom <- optim(par = mom, fn = l_log_neg, x = x4)$par
x4_mle_2 <- optim(par = set2, fn = l_log_neg, x = x4)$par
x4_mle_3 <- optim(par = set3, fn = l_log_neg, x = x4)$par


#PART C
#MAKING PLOTS

#FIRST SET OF INITIAL VALUES
y <- mom
plot(y,x1_mle_1mom,col="red",type="b",ylim=c(0.5,4.5),xlim=c(0.5,5),ylab="MLE",xlab="First set of initial values i.e. MOM")
lines(y,x2_mle_1mom,col="blue",type = "b")
lines(y,x3_mle_1mom,col="green",type = "b")
lines(y,x4_mle_1mom,col="black",type = "b")
legend("topright", legend = c("lambda=1", "lambda=2","lambda=3","lambda=4"), col = c("red", "blue","green","black"), pch = 16, cex = 1)

#SECOND SET OF INITIAL VALUES
y <- set2
plot(y,x1_mle_2,col="red",type="b",ylim=c(0.5,4.5),xlim=c(0.5,5),ylab="MLE",xlab="Second set of initial values i.e. Random Values")
lines(y,x2_mle_2,col="blue",type = "b")
lines(y,x3_mle_2,col="green",type = "b")
lines(y,x4_mle_2,col="black",type = "b")
legend("topright", legend = c("lambda=1", "lambda=2","lambda=3","lambda=4"), col = c("red", "blue","green","black"), pch = 16, cex = 1)

#THIRD SET OF INITIAL VALUES
y <- set3
plot(y,x1_mle_3,col="red",type="b",ylim=c(0.5,4.5),xlim=c(0.5,5),ylab="MLE",xlab="Third set of initial values i.e. Random Values")
lines(y,x2_mle_3,col="blue",type = "b")
lines(y,x3_mle_3,col="green",type = "b")
lines(y,x4_mle_3,col="black",type = "b")
legend("topright", legend = c("lambda=1", "lambda=2","lambda=3","lambda=4"), col = c("red", "blue","green","black"), pch = 16, cex = 1)





