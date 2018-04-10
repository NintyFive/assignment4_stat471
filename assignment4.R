# 1
# install.packages('alr3')
# install.packages('matlib')
library('alr3')
library('matlib')
growth <- read.table("growth.txt", header=T,colClasses = c("numeric","numeric","numeric","numeric"))
attach(growth)
names(growth)

h<-lm(Yield~x1+x2+x3+I(x1^2)+I(x2^2)+I(x3^2)+x1*x2+x2*x3+x1*x3,data = growth)
summary(h)
# b = c(1.271,1.361,-1.494)
# B = matrix(c(-3.767,2.875/2,-2.625/2,2.875/2,-12.430,-4.625/2,-2.625/2,-4.625/2,-9.601),nrow = 3,ncol = 3)
# t(B)
# ev = eigen(B)
# ev$values
h<-lm(Yield~x2+x3+I(x2^2)+I(x3^2)+x2*x3,data = growth)
summary(h)
pure.error.anova(h)
b = c(1.361,-1.494)
B = matrix(c(-12.055,-4.625/2,-4.625/2,-9.227),nrow = 2,ncol = 2)
t(B)
-1/2*solve(B)%*%b
ev = eigen(B)
ev$values
#2
ch = c(rep(9,13),rep(10,35),rep(11,44),rep(12,69),rep(13,36),rep(14,24),
       rep(15,7),rep(16,3),rep(18,5),rep(17,2),19,20)
sum((ch-mean(ch))^2)/(length(ch)-1)/240

#3
acres = c(222.81,49.61,50.25,197.81)
tows = c(5704,1270,1287,5064)
average_num = c(0.44,1.17,3.92,1.80)

sum(average_num*tows)/sum(tows)

W_h = tows/sum(tows)

s_h = c(0.068,0.042,2.146,0.794)
n_h = c(4,6,3,5)
f_h = n_h/tows
var_yst = 0
for ( k in c(1:4)){
  var_yst = var_yst + W_h[k]^2*(1-f_h[k])*s_h[k]^2/n_h[k]
}


#4
install.packages('SDaA')
require('SDaA')
attach(agsrs)

r = mean(agsrs$acres92)/mean(agsrs$farms87)
r*2087759

#b
fit = lm(acres92~farms87)
summary(fit)
fit$coefficients
y = 267029.81 + 47.65* mean(farms87)
y*3078

#c
r = mean(acres92)/mean(acres87)
N = 3078
n = 300
sum = 0
for (k in c(1:n))
{
  sum = sum + (acres92[k]-r*acres87[k])^2
}
sum = sum/(n-1)
se = N*sqrt((1-n/N)*(sum/n))
se

r = mean(acres92)/mean(farms87)
N = 3078
n = 300
sum = 0
for (k in c(1:n))
{
  sum = sum + (acres92[k]-r*farms87[k])^2
}
sum = sum/(n-1)
se = N*sqrt((1-n/N)*(sum/n))
se

fit = lm(acres92~farms87)

b0 = fit$coefficients[1]
b1 = fit$coefficients[2]
N = 3078
n = 300
sum = 0
for (k in c(1:n))
{
  sum = sum + (acres92[k]-b0-b1*farms87[k])^2
}
sum = sum/(n-1)
se = N*sqrt((1-n/N)*(sum/n))
se
# fit = lm(counties$physicia~counties$totpop)
# summary(fit)
# fit$coefficients
