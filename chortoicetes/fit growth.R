
# load data from Clissold thesis and Gregg 1983
growth <- read.table('Chortoicetes/growth vs temp.txt', header=T)


#########################  fit exponential function
growth$timetemp  <- growth$time.d*growth$temp.C
growth$timetemp2 <- growth$time.d*growth$temp.C**2 # add power terms
growth$timetemp3 <- growth$time.d*growth$temp.C**3
growth$timetemp4 <- growth$time.d*growth$temp.C**4
growth$timetemp5 <- growth$time.d*growth$temp.C**5

lme1<-lm(log(dwt.mg)~time.d + timetemp , data = growth)
lme2<-lm(log(dwt.mg)~time.d + timetemp + timetemp2 , data = growth)
lme3<-lm(log(dwt.mg)~time.d + timetemp + timetemp2 + timetemp3, data = growth)
lme4<-lm(log(dwt.mg)~time.d + timetemp + timetemp2 + timetemp3 + timetemp4, data = growth)
lme5<-lm(log(dwt.mg)~time.d + timetemp + timetemp2 + timetemp3 + timetemp4 + timetemp5, data = growth)
AIC(lme1, lme2,lme3, lme4, lme5)
# best model is model 5
summary(lme5)

cl = rainbow(length(unique(growth$temp.C)))
count = 1
leg = {}
plot(c(),c(),xlim=c(0,70),ylim=c(0,5), ylab = 'ln(mass (mg))', xlab = 'time (d)')
for(temp in unique(growth$temp.C)){
  subdata <- subset(growth, temp.C == temp)
  t <- seq(0,max(subdata$time.d),length.out=100)
  data = as.data.frame(cbind(t, t*temp, t*temp**2, t*temp**3,  t*temp**4 ,t*temp**5    ))
  names(data)<- c('time.d', 'timetemp', 'timetemp2', 'timetemp3', 'timetemp4', 'timetemp5')
  pred <-predict.lm(lme5, data)
  lines(data$time.d, (pred), col = cl[count])
  points(subdata$time.d, log(subdata$dwt.mg), col = cl[count])
  count = count + 1
  leg = c(leg, temp)
}

legend("topright",as.character(leg), col = cl, lty = 1, bg = NULL, bty = 'n', title = "temp (C)")

write.csv(lme5$coefficients,'Chortoicetes/growth_coeff.csv')
