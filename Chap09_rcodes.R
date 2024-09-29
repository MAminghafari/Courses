
install.packages("BSDA")
library(BSDA)
library(nortest)
#large sample
mean.x=3.27
sigma.x = 1.3
n.x = 121
mean.y = 2.53
sigma.y = 1.16
n.y = 125L
level = 0.05
z_calc= (mean.x-mean.y)/(sigma.x^2/n.x+sigma.y^2/n.y)^.5

z_calc

zsum.test(
  mean.x=3.27,
  sigma.x = 1.3,
  n.x = 121,
  mean.y = 2.53,
  sigma.y = 1.16,
  n.y = 125,
  alternative = "two.sided",
  mu = 0,
  conf.level = 0.95
)
################
zsum.test(
  mean.x= 6012,
  sigma.x = 602,
  n.x = 35,
  mean.y = 5832,
  sigma.y = 497,
  n.y = 35,
  alternative = "two.sided",
  mu = 0,
  conf.level = 0.90
)
############
#small sample

#test for normality
shapiro.test(rnorm(100))
shapiro.test(rt(10,1))
###################
#test for equality of variance

x <- rnorm(50, mean = 0, sd = 2)
y <- rnorm(30, mean = 1, sd = 1)
var.test(x, y)
###############

#mean, sd, small sample:
n.x =	 21
n.y = 25
mean.x= 3.27
mean.y = 2.53
sigma.x=1.30
sigma.y = 1.16
S2p = ((n.x-1)*sigma.x^2+(n.y-1)*sigma.y^2)/(n.x+n.y-2)
t_calc= (mean.x-mean.y)/(S2p*(1/n.x+1/n.y))^0.5
t_calc


tsum.test(mean.x=mean.x,s.x=sigma.x,n.x=n.x, mean.y=mean.y, s.y=sigma.y,n.y=n.y, 
          alternative = "two.sided", var.equal = TRUE, conf.level = 0.95)

##############################
tsum.test(mean.x = 78,s.x = 10,n.x = 30,mean.y = 85,s.y = 15,n.y = 25,alternative = "two.sided",mu = 0,var.equal =FALSE,conf.level =.9  )



################## Small sample
#paired samples
#human resource example
B=c(85, 94,78,87)
A=c(94,87,79,88)
t.test(B,A,alternative = "less",conf.level = 0.1, paired = TRUE,mu=0)
t.test(A,B,alternative = "greater",conf.level = 0.1, paired = TRUE)



#paired sample with diff:
D = A-B
t.test(D, alternative = "less", conf.level = 0.1)

#Normality test_set
shapiro.test(A)
#######
#example2 
Client=c(10,8,7,9,11,10,9,8)
Competitor=c(11,11,10,12,11,13,12,10)
#test normality
#test 1
shapiro.test(Client)
#test2
ad.test(Client)

shapiro.test(Competitor)
ad.test(Competitor)
t.test(Client,Competitor,alternative = "less",conf.level = 0.01, paired = TRUE)
####################
#proportions

phat=(63+49)/(78+82)
s2=phat*(1-phat)
zsum.test(mu=0,mean.x=63/78, mean.y=49/82,sigma.x= s2^.5, sigma.y=s2^.5,n.x=78,n.y=82,conf.level=0.99, alternative = "two.sided")

#MA VS CA

phat=(74+129)/(1500+1500)
s2=phat*(1-phat)
zsum.test(mu=0,
          mean.x=74/1500,
          mean.y=129/1500,
          sigma.x= s2^.5, 
          sigma.y=s2^.5,
          n.x=1500,
          n.y=1500,
          conf.level=0.95, 
          alternative = "two.sided")

####################
# variance test

x=c(10,8,7,9,11,10,9,8)
y=c(11,11,10,12,11,13,12,10)
ad.test(x)
ad.test(y)
var.test(x,y)

#Second example
x = c(4.23, 4.35, 4.05, 3.75, 4.41, 4.37, 4.01, 4.06, 4.15, 4.19, 4.52, 4.21, 4.29)
y = c(4.14, 4.26, 4.05, 4.11, 4.31, 4.12, 4.17, 4.35, 4.25, 4.21, 4.05, 4.28, 4.15, 4.20, 4.32, 4.25, 4.02, 4.14)

var.test(x,y)
#####################
#non parametric test:
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wilcox.test(x, y, paired = TRUE, alternative = "greater")
