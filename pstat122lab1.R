##2
#a
summary(BMD.data)
#b 
#c
BMD.data <- read.table("/Users/Jasmine/desktop/BMDdata.txt",header=TRUE)
BMD.data$treat <- factor(BMD.data$treat)
model1 <- aov(BMD ~ treat, data = BMD.data)
summary(model1)
#d
#e
library(plyr)
mean.table <- ddply(BMD.data, .(treat), summarize, mean = mean(BMD))
mean.table
c((0.2159333 - qt(p=0.05/2,df = 45-3, lower.tail = F)*sqrt(0.0002064/15)), 
  (0.2159333 + qt(p=0.05/2, df = 45-3, lower.tail = F)*sqrt(0.0002064/15)))
#h
trtavg <- (0.2350667+0.2159333)/2
sampledif <- 0.2188667-trtavg
c((sampledif - qt(p = 0.01/2, df = 45-3, lower.tail = F)*sqrt(0.0002064*(1/10))),
  (sampledif + qt(p = 0.01/2, df = 45-3, lower.tail = F)*sqrt(0.0002064*(1/10))))
w.B <- qt(p = 1-(0.05/(2*3)), df = 45-3)
w.B
c((0.2350667 - 0.2188667) - (w.B*sqrt(0.0002064*(2/15))),
  (0.2350667 - 0.2188667) + (w.B*sqrt(0.0002064*(2/15))))
#f
lmmodel <- lm(BMD ~ treat, data = BMD.data)
lmmodel
plot(lmmodel)
BMD.anova = anova(model1)
SSE = BMD.anova$`Sum Sq`[2]
n = 45
z = model1$residuals/sqrt(SSE/(n-1))
var(z);mean(z)
plot(BMD.data$treat,z)
plot(BMD.data$g,z)
plot(BMD.data$BMD,z)
abline(h=0)
plot(model1$fitted,z)
abline(h=0)
nscore = qnorm((rank(z)-0.375)/(17+0.25))
plot(nscore,z)
qqnorm(z)
qqline(z)
#j
c(0,0.0086676/qchisq(p = 0.05, df = 45 - 3))
#3
#b
reaction.time.data <- read.table("/Users/Jasmine/Desktop/reaction.time.txt", header = TRUE)
plot(reaction.time.data$Trtmt, reaction.time.data$y, xlab = "Treatment Combination", ylab = "Reaction Time")
#c
reaction.time.data$Trtmt <- factor(reaction.time.data$Trtmt)
model2 <- aov(y ~ Trtmt, data = reaction.time.data)
anova(model2)
#d
library(plyr)
mean.table <- ddply(reaction.time.data, .(Trtmt), summarize, mean = mean(y))
mean.table
mean = mean(reaction.time.data$y)
c(0.1850000-mean,0.1786667-mean,0.2120000-mean,
  0.2683333-mean,0.2593333-mean,0.2650000-mean)
#e
w.B <- qt(p=1-(0.1/4),df=18-6)
c((((0.2683333+0.2593333+0.2650000)/3) - ((0.1850000+0.1786667+0.2120000)/3)) - (w.B*sqrt(0.0002893*(2/9))),
  (((0.2683333+0.2593333+0.2650000)/3) - ((0.1850000+0.1786667+0.2120000)/3)) + (w.B*sqrt(0.0002893*(2/9))))
c(((0.2683333 - 0.1850000) - (w.B*sqrt(0.0002893*(2/3)))),
  ((0.2683333 - 0.1850000) + (w.B*sqrt(0.0002893*(2/3)))))
#f
lmmodel2 <- lm(y ~ Trtmt, data = reaction.time.data)
plot(lmmodel2)

#g
reaction.time.data$A <- factor(reaction.time.data$A)
reaction.time.data$B <- factor(reaction.time.data$B)
attach(reaction.time.data)
twofit <- aov(y ~ A * B, data = reaction.time.data)
summary(twofit)
#h
lmmodel3 <- lm(y ~ A + B + A*B, data = reaction.time.data)
summary(lmmodel3, maxsum = 6)
options(contr)
lm.fit <- lm(y ~ A*B, data = reaction.time.data)
summary(lm.fit)
#i 
c(0,0.003472/qchisq(p=0.05,df=(2*3*17)))
bestfitmodel <- aov(y ~ A, data = reaction.time.data)
anova(bestfitmodel)
eyedata <- read.table("/Users/Jasmine/Desktop/Eyedata.txt",header = TRUE)
eyedata$Distance <- factor(eyedata$Distance)
eyedata$Subject <- factor(eyedata$Subject)
attach(eyedata)
anova.fit <- aov(Focus ~ Distance + Subject, data = eyedata)
anova(anova.fit)
#e
library(plyr)
mean.table <- ddply(eyedata, .(Distance), summarize, mean = mean(Focus))
mean.table
c(((6.8-5.2) - qt(p=0.05/2,df=20-4, lower.tail = F)*sqrt(1.275*(2/5))), 
  ((6.8-5.2) + qt(p=0.05/2,df=20-4, lower.tail = F)*sqrt(1.275*(2/5))))
#f
estimator <- ((0.5*(6.8+5.2)) - (0.5*(3.6+3.8)))
estimator
s.e <- sqrt(1.275*(4/16))
s.e
c((estimator - qt(p=0.05/2,df=20-4, lower.tail = F)*s.e),
  (estimator + qt(p=0.05/2,df=20-4, lower.tail = F)*s.e))
#5
#a
latin.square <- data.frame(Batch = factor(rep(c("1","2","3","4","5"), each = 5)), Day = factor(rep(paste0("Day",1:5),5)), 
                           Trt = factor(c(1,2,4,3,5,3,5,1,4,2,2,1,3,5,4,4,3,5,2,1,5,4,2,1,3)), React = c(8,7,1,7,3,11,2,7,3,8,4,9,10,1,5,6,8,6,6,10,4,2,3,8,8))
attach(latin.square)
fit.latin <- aov(React ~ Day + Trt + Batch, data = latin.square)
anova(fit.latin)
mean(React)
library(plyr)
mean.table <- ddply(latin.square, .(Trt), summarize, mean = mean(React))
mean.table
c(8.4-5.88,5.6-5.88,8.8-5.88,3.4-5.88,3.2-5.88)
