library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(lmtest)
library(gridExtra)
library(arm)
library(knitr)

load('~/Desktop/STAT8100/Master Paper/mean_data.RData')
load('~/Desktop/STAT8100/Master Paper/median_data.RData')

#### Section 2 ####

meanplot1 <- ggplot(data = meandat[meandat$commonname=='BLUEGILL',],aes(x = temp,y = count)) +
  geom_point() + labs(title = 'BLUEGILL', x='Temperature',y='Count')
meanplot2 <- ggplot(data = meandat[meandat$commonname=='CENTRAL STONEROLLER',],aes(x = temp,y = count)) +
  geom_point() + labs(title = 'CENTRAL STONEROLLER', x='Temperature',y='Count')
meanplot3 <- ggplot(data = meandat[meandat$commonname=='CREEK CHUB',],aes(x = temp,y = count)) +
  geom_point() + labs(title = 'CREEK CHUB', x='Temperature',y='Count')
meanplot4 <- ggplot(data = meandat[meandat$commonname=='GREEN SUNFISH',],aes(x = temp,y = count)) +
  geom_point() + labs(title = 'GREEN SUNFISH', x='Temperature',y='Count')
grid.arrange(meanplot1, meanplot2, meanplot3, meanplot4, ncol=2, nrow = 2) # Figure 2.1.1



medianplot1 <- ggplot(data = medidat[medidat$commonname=='BLUEGILL',],aes(x = temp,y = logc)) +
  geom_point() + labs(title = 'BLUEGILL', x='Temperature',y='logc')
medianplot2 <- ggplot(data = medidat[medidat$commonname=='CENTRAL STONEROLLER',],aes(x = temp,y = logc)) +
  geom_point() + labs(title = 'CENTRAL STONEROLLER', x='Temperature',y='logc')
medianplot3 <- ggplot(data = medidat[medidat$commonname=='CREEK CHUB',],aes(x = temp,y = logc)) +
  geom_point() + labs(title = 'CREEK CHUB', x='Temperature',y='logc')
medianplot4 <- ggplot(data = medidat[medidat$commonname=='GREEN SUNFISH',],aes(x = temp,y = logc)) +
  geom_point() + labs(title = 'GREEN SUNFISH', x='Temperature',y='logc')
grid.arrange(medianplot1, medianplot2, medianplot3, medianplot4, ncol=2, nrow = 2) # Figure 2.1.2


#### Section 3.1 ####

## Linear model
mean.lm <- lm(logc~temp, data = meandat)
summary(mean.lm)
kable(coef(summary(mean.lm)),digits = 4, caption = 'Linear Model Estimates of mean data') # Table 3.1.1

## Random intercept model
int.mean <- lmer(logc ~ temp + (1|commonname),data = meandat,REML = F)
summary(int.mean)
kable(VarCorr(int.mean),digits = 4,
      caption = 'Random effects Estimates of mean data (random intercept only)') # Table 3.1.2
rand(int.mean) # Test for random intercept

int.mean.all <- lmer(logc ~ temp + Turb + TN + TP + (1|commonname),data = meandat,REML = F)
summary(int.mean.all)
rand(int.mean.all)

## Random intercept and slope model
result.mean <- lmer(logc ~ temp + (temp|commonname),data = meandat,REML = F)
summary(result.mean)
kable(VarCorr(result.mean),digits = 4,
      caption = 'Random effects Estimates of mean data (random intercept and slope)') # Table 3.1.3

result.mean.all <- lmer(logc ~ temp + Turb + TP + TN + 
                          (temp+Turb+TP+TN|commonname),data = meandat,REML = F)
summary(result.mean.all)

#### Section 3.2 ####

## Random intercept model
int.median <- lmer(logc ~ temp + (1|commonname),data = medidat,REML = F)
summary(int.median)
kable(VarCorr(int.median),digits = 4,
      caption = 'Random effects Estimates of median data (random intercept only)') # Table 3.2.1
rand(int.median) # Test for random intercept

int.median.all <- lmer(logc ~ temp + Turb + TN + TP + (1|commonname),data = medidat,REML = F)
summary(int.median.all)
rand(int.median.all)

## Random intercept and slope model
result.median <- lmer(logc ~ temp + (temp|commonname),data = medidat,REML = F)
summary(result.median)
kable(VarCorr(result.median),digits = 4,
      caption = 'Random effects Estimates of median data (random intercept and slope)') # Table 3.2.2

result.median.all <- lmer(logc ~ temp + Turb + TP + TN + 
                          (temp+Turb+TP+TN|commonname),data = medidat,REML = F)
summary(result.median.all)


#### Appendix ####

## A.1
residuals.lm(mean.lm) %>% qqnorm(,main='Normal Q-Q Plot for Linear regression model',cex.main=0.8)
residuals.lm(mean.lm) %>% qqline(,lwd=1.5)

residuals.lm(mean.lm) %>% hist(,main='Histogram of Residuals for Linear regression model',
                               xlab = 'Residuals',cex.main=0.8)
## A.2
resid(int.mean) %>% qqnorm(,main='Normal Q-Q Plot for Random intercept model (mean data)',cex.main=0.8) 
resid(int.mean) %>% qqline(,lwd=1.5) 

resid(int.mean) %>% hist(,main='Histogram of Residuals for Random intercept model (mean data)',
                         xlab = 'Residuals',cex.main=0.8)

resid(result.mean) %>% qqnorm(,main='Normal Q-Q Plot for Random intercept and slope model (mean data)',cex.main=0.8) 
resid(result.mean) %>% qqline(,lwd=1.5) 

resid(result.mean) %>% hist(,main='Histogram of Residuals for Random intercept and slope model (mean data)',
                            xlab = 'Residuals',cex.main=0.8)
## B
resid(int.median) %>% qqnorm(,main='Normal Q-Q Plot for Random intercept model (median data)',cex.main=0.8) 
resid(int.median) %>% qqline(,lwd=1.5) 

resid(int.median) %>% hist(,main='Histogram of Residuals for Random intercept model (median data)',
                           xlab = 'Residuals',cex.main=0.8)

resid(result.median) %>% qqnorm(,main='Normal Q-Q Plot for Random intercept and slope model (median data)',cex.main=0.8) 
resid(result.median) %>% qqline(,lwd=1.5) 

resid(result.median) %>% hist(,main='Histogram of Residuals for Random intercept and slope model (median data)',
                              xlab = 'Residuals',cex.main=0.8)
## C

meanf <- fitted.values(int.mean)
meanfit <- meandat
meanfit$fitted <- meanf
ggplot(meanfit, aes(x = mon,y=fitted,group=commonname)) + 
  geom_line(aes(color = commonname, linetype = commonname)) + 
  geom_point(aes(color=commonname)) +
  scale_color_manual(values = c("darkred", "steelblue","springgreen","orange")) +
  theme(axis.text.x = element_text(angle=45)) +
  labs(title = 'Fitted lines from the random intercept model for mean data')


medif <- fitted.values(int.median)
medifit <- medidat
medifit$fitted <- medif
ggplot(medifit, aes(x = mon,y=fitted,group=commonname)) + 
  geom_line(aes(color = commonname, linetype = commonname)) + 
  geom_point(aes(color=commonname)) +
  scale_color_manual(values = c("darkred", "steelblue","springgreen","orange")) +
  theme(axis.text.x = element_text(angle=45)) +
  labs(title = 'Fitted lines from the random intercept model for median data')







