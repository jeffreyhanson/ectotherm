curve(exp(-(x/10)^10), from = 0, to = 1000)
curve(1/(exp(x)**(1/5)), from = 0, to = 1)


library('ggplot2')
# load data from Gregg 1983
survival <- read.table('Chortoicetes/survival.txt', header=T)


# test weibull fit (if linear, then weibull is apppropriate (wiki)) 
ggplot(survival, aes(x=log(-log(time.d/max(time.d))), y=log(-log(survival)), colour=as.factor(temp.C))) + geom_line()
# conclude weibull model not appropriate


# try to fit linear regression model
lm1<-lm(survival~time.d+temp.C + I(temp.C^(2)) + I(temp.C^(3)) , data = survival)
lm2<-lm(survival~time.d+temp.C + I(temp.C^(2))  , data = survival)
lm3<-lm(survival~time.d*temp.C + I(temp.C^(2))  , data = survival)
lm3<-lm(survival~time.d*temp.C , data = survival)
AIC(lm1, lm2,lm3)

summary(lm1)
temp.C<-survival$temp.C
# plot model against data
cl = rainbow(length(unique(temp.C)))
count = 1
leg = {}
plot(c(),c(),xlim=c(0,60),ylim=c(0,1), ylab = 'survival', xlab = 'time')
for(temp in unique(temp.C)){
subdata <- subset(survival, temp.C == temp)
data = as.data.frame(cbind(seq(0,max(subdata$time.d),length.out=100),
                           rep(temp,100),rep(temp,100)**2,rep(temp,100)**3  ))
names(data)<- c('time.d', 'temp.C', 'I(temp.C^(2))', 'I(temp.C^(3))')
pred <-predict.lm(lm1, data)
lines(data$time.d, pred, col = cl[count])
points(subdata$time.d, subdata$survival, col = cl[count])
count = count + 1
leg = c(leg, temp)
}

legend("topright",as.character(leg), col = cl, lty = 1)

#########################  apply logistic regression
survival$timetemp  <- survival$time.d*survival$temp.C
survival$timetemp2 <- survival$time.d*survival$temp.C**2# add power terms
survival$timetemp3 <- survival$time.d*survival$temp.C**3
survival$timetemp4 <- survival$time.d*survival$temp.C**4
survival$timetemp5 <- survival$time.d*survival$temp.C**5

lme1<-lm(I(log(1/survival))~time.d + timetemp -1, data = survival)
lme2<-lm(I(log(1/survival))~time.d + timetemp + timetemp2 - 1, data = survival)
lme3<-lm(I(log(1/survival))~time.d + timetemp + timetemp2 + timetemp3- 1, data = survival)
lme4<-lm(I(log(1/survival))~time.d + timetemp + timetemp2 + timetemp3 + timetemp4 - 1, data = survival)
lme5<-lm(I(log(1/survival))~time.d + timetemp + timetemp2 + timetemp3 + timetemp4 + timetemp5 -1, data = survival)
AIC(lme1, lme2,lme3, lme4, lme5)
# best model is model 4
summary(lme4)

cl = rainbow(length(unique(survival$temp.C)))
count = 1
leg = {}
plot(c(),c(),xlim=c(0,75),ylim=c(0,1), ylab = 'surviving proportion', xlab = 'time (d)')
for(temp in unique(survival$temp.C)){
  subdata <- subset(survival, temp.C == temp)
  t <- seq(0,max(subdata$time.d),length.out=100)
  data = as.data.frame(cbind(t, t*temp, t*temp**2, t*temp**3,  t*temp**4    ))
  names(data)<- c('time.d', 'timetemp', 'timetemp2', 'timetemp3', 'timetemp4')
  pred <-predict.lm(lme4, data)
  lines(data$time.d, 1/exp(pred), col = cl[count])
  points(subdata$time.d, subdata$survival, col = cl[count])
  count = count + 1
  leg = c(leg, temp)
}

legend("topright",as.character(leg), col = cl, lty = 1, bg = NULL, bty = 'n', title = "temp (C)")

lme4$coefficients

dsurvival<-function(pars,survival, temp){
  # change in proportion surviving as a function of fitted pars, proportion surviving, and temp C  
  dy <- -survival*(      pars[2]*temp**1+
                         pars[3]*temp**2+
                         pars[4]*temp**3+
                         pars[5]*temp**4)
  return(dy)}
  
  