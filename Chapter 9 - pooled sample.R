# Pooled sample variance 

# Problem 1

#Nine dogs and ten cats were tested to determine if there is a difference in the average number of days that 
#the animal can survive without food. The dogs averaged 11 days with a standard deviation of 2 days while 
#the cats averaged 12 days with a standard deviation of 3 days. What can be concluded? (Use alpha = .05)
#Assume that the population variances are equal

x.bar1 = 11
x.bar2 = 12
s1 = 2
s2 = 3
n1 = 9
n2 = 10

# Pooled sd Sp
sp2 = ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
# Test statistic
tcalc = (x.bar1-x.bar2)/(sqrt(sp2*(1/n1+1/n2)))
# Degrees of freedom
df_pool = n1+n2-2
# Critical values
qt(0.025,df_pool)
qt(0.975,df_pool)
# p-value
2*pt(-abs(tcalc),df_pool)

# Using BSDA package
library(BSDA)
tsum.test(mean.x = 11,s.x = 2,n.x = 9,mean.y = 12,s.y = 3,n.y = 10,alternative = "two.sided",mu = 0,var.equal = T,conf.level = .95)

#######################################

# Problem 2

#We wish to know if we may conclude, at the 95% confidence level, that smokers, in general, 
#have greater lung damage than do non-smokers.
#Data: 
# Smokers: mean= 17.5 n= 16 s= 4.4752
# Non-Smokers: mean= 12.4 n= 9 s= 4.8492
#Assume that the population variances are equal

x.bar1 = 17.5
x.bar2 = 12.4
s1 = 4.4752
s2 = 4.8492
n1 = 16
n2 = 9

# Pooled sd Sp
sp2 = ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
# Test statistic
tcalc = (x.bar1-x.bar2)/(sqrt(sp2*(1/n1+1/n2)))
# Degrees of freedom
df_pool = n1+n2-2
# Critical value
qt(0.95,df_pool)
# p-value
1-pt(tcalc,df_pool)

tsum.test(mean.x = 17.5,s.x = 4.4752,n.x = 16,mean.y = 12.4,s.y = 4.8492,n.y = 9,alternative = "greater",mu = 0,var.equal = T,conf.level = .95)



