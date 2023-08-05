rm(list=ls())

#### Example 1 #####
#### H0: mu<=166.3 versus H1: mu>166.3
### 1 sample test of mu (population mean) with z score will be applied as population standard deviation is known
n<-40
pop.stddeviation<-26
mu.null<-166.3
sample.mean<-172.55
z.stat_1<-(sample.mean-mu.null)/(pop.stddeviation/sqrt(n))
### Since the alternative is in the right tail so z_stat upper tail value will be used
z_criticalval_1<-(qnorm(0.05,mean=0,sd=1,lower.tail = FALSE)) ## lower.tail is same as left tail
### Here z_stat does not lie in the rejection region thus we fail to reject H0 ###

p.val_1<-pnorm(z.stat_1,mean=0,sd=1,lower.tail = FALSE)

### Here p value>0.05 thus we fail to reject H0 ###


#### Example 2 #####
#### H0: mu<=166.3 versus H1: mu>166.3

### 1 sample test of mu (population mean) with t statistic will be applied
### population std. deviation; population variance are unknown.
n<-40
mu.null<-166.3
sample.mean<-172.55
sample.stddeviation<-26.33
t.stat_2<-(sample.mean-mu.null)/(sample.stddeviation/sqrt(n))
#### If data is available then we can use t.test ####
#t.test_2<-t.test(x=data,mu=mu.null,alternative="greater")

### Since the alternative is in the right tail so t_stat upper tail value will be used
t_criticalval_2<-(qt(p=0.05,df=39,lower.tail = FALSE))
### Here t_stat does not lie in the rejection region thus we fail to reject H0 ###

p.val_2<-pt(q=t.stat_2,df=39,lower.tail = FALSE)
### Here p value>0.05 thus we fail to reject H0 ###


#### Example 3 #####
#### H0: sigma>=0.0230 versus H1: sigma<0.0230

### 1 sample test of sigma (population std. deviation) with chi-square statistic will be applied

n<-37
sigma.null<-0.0230
sample.stddeviation<-0.01648
chi.stat_3<-((n-1)*(sample.stddeviation)^2)/(sigma.null^2)
### Since the alternative is less than so left side value will be used
chi_criticalval_3<-qchisq(p=0.05,df=36,lower.tail=TRUE)
### Here chi square_stat lies in the rejection region thus we reject H0 ###

p.val_3<-pchisq(q=chi.stat_3,df=(n-1),lower.tail = TRUE)
### Here p value<0.05 thus we  reject H0 ###

#### If data is available then we can consider using chi.test ####
#chi.test_3<-chi.test(x=data,mu=mu.null,alternative="greater")



#### Example  4 #####
#### H0: mu_1= mu_2 versus H1: mu_1 not equal to mu_2

n1<-186; n2<-210
mu.null<-0 ### H0: mu_1-mu_2=0
sample1.mean<-15668.5; sample2.mean<-16215
sample1.stddeviation<-8632.5; sample2.stddeviation<-7301.2

t.stat_5<-((sample1.mean-sample2.mean)-mu.null)/sqrt(((sample1.stddeviation^2)/n1)+((sample2.stddeviation^2)/n2))

#### If data is available then we can use t.test ####
#t.test_2<-t.test(x=data1,y=data2,paired=FALSE, var.equal=FALSE,alternative="two.sided") 

### Since the alternative is both sided so we can use either of the tail. The CV will lie on both sides of the curve.
t_criticalval_5<-(qt(p=0.025,df=185,lower.tail = TRUE))

### Here t_stat does not lie in the rejection region thus we fail to reject H0 ###

p.val_5<-2*pt(q=t.stat_5,df=185,lower.tail = TRUE)
### Here p value>0.05 thus we fail to reject H0 ###



#### Example  5 #####
#### H0: mu<=0 versus H1: mu > zero
#### 2 sampled data (paired data/dependent data), testing of mean difference

with_breakfast<-c(8, 7, 9, 5, 9, 8, 10, 7, 6, 9)
without_breakfast<-c(6, 5, 5, 4, 7, 7, 7, 5, 6, 5)
test.ans2<-t.test(x=with_breakfast, y=without_breakfast, paired = TRUE, alternative = "greater")
t.stat_2<-test.ans2$statistic
t_criticalval_q2<-(qt(0.05,df=9,lower.tail = FALSE)) ### df=n-1
### Here t_stat  lies in the rejection region thus H0 is  rejected ###


