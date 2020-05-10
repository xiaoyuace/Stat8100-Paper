library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(lmtest)
library(gridExtra)
library(arm)
library(knitr)

save(meandat, file = "Desktop/mean_data.RData")
save(medidat, file = "Desktop/median_data.RData")

#### Raw Data ####
raw <- read.csv('Desktop/2019summer/com.csv')

blue <- raw[raw$commonname=='BLUEGILL',] 
central <- raw[raw$commonname=='CENTRAL STONEROLLER',] 
chub <- raw[raw$commonname=='CREEK CHUB',] 
sunfish <- raw[raw$commonname=='GREEN SUNFISH',] 
mod <- rbind(blue,central,chub,sunfish)

mod <- mod[mod$Turbidity__NTU_ < 55900,]
new <- mod[,c(4,7,10,11,12,15,17,21,23,24,34)]
new$mon <- paste(new$Year,new$Month,sep='-') %>% as.factor()
new <- na.omit(new)
new$commonname <- factor(new$commonname)
new$mon <- factor(new$mon)

#### mean data ####
new %>% 
  group_by(mon, commonname) %>%
  dplyr::summarize(count = mean(CPUE), temp = mean(Temp_C), 
                   Turb = mean(Turbidity__NTU_), TP = mean(TP),
                   TN = mean(TN)) -> dat 
dat$logc <- log(dat$count)
meandat <- dat[-c(17,18,24:27,31,35,106,109,127,141,148,151),]

meanplot1 <- ggplot(data = meandat[meandat$commonname=='BLUEGILL',],aes(x = temp,y = count)) +
  geom_point() + labs(title = 'BLUEGILL', x='Temperature',y='Count')
meanplot2 <- ggplot(data = meandat[meandat$commonname=='CENTRAL STONEROLLER',],aes(x = temp,y = count)) +
  geom_point() + labs(title = 'CENTRAL STONEROLLER', x='Temperature',y='Count')
meanplot3 <- ggplot(data = meandat[meandat$commonname=='CREEK CHUB',],aes(x = temp,y = count)) +
  geom_point() + labs(title = 'CREEK CHUB', x='Temperature',y='Count')
meanplot4 <- ggplot(data = meandat[meandat$commonname=='GREEN SUNFISH',],aes(x = temp,y = count)) +
  geom_point() + labs(title = 'GREEN SUNFISH', x='Temperature',y='Count')

grid.arrange(meanplot1, meanplot2, meanplot3, meanplot4, ncol=2, nrow = 2)

#### median data ####

new %>% 
  group_by(mon, commonname) %>%
  dplyr::summarize(count = median(CPUE), temp = median(Temp_C),
                   Turb = mean(Turbidity__NTU_), TP = mean(TP),
                   TN = mean(TN)) -> medi 
medi$logc <- log(medi$count)
medidat <- medi[-c(20:27,35,141,17,100,127,151,6,1:4,69,18,106),]


medianplot1 <- ggplot(data = medidat[medidat$commonname=='BLUEGILL',],aes(x = temp,y = logc)) +
  geom_point() + labs(title = 'BLUEGILL', x='Temperature',y='logc')
medianplot2 <- ggplot(data = medidat[medidat$commonname=='CENTRAL STONEROLLER',],aes(x = temp,y = logc)) +
  geom_point() + labs(title = 'CENTRAL STONEROLLER', x='Temperature',y='logc')
medianplot3 <- ggplot(data = medidat[medidat$commonname=='CREEK CHUB',],aes(x = temp,y = logc)) +
  geom_point() + labs(title = 'CREEK CHUB', x='Temperature',y='logc')
medianplot4 <- ggplot(data = medidat[medidat$commonname=='GREEN SUNFISH',],aes(x = temp,y = logc)) +
  geom_point() + labs(title = 'GREEN SUNFISH', x='Temperature',y='logc')

grid.arrange(medianplot1, medianplot2, medianplot3, medianplot4, ncol=2, nrow = 2)

#### linear model ####

mean.lm <- lm(count~temp, data = dat)
summary(mean.lm)
residuals.lm(mean.lm) %>% plot()
plot(dat$temp,dat$count,pch=16,cex=0.5,main = 'Linear model fit for mean data',
     xlab = 'Temperature', ylab = 'Count')
abline(mean.lm,lwd=1.5)

median.lm <- lm(count~temp, data = medi)
summary(median.lm)
plot(medi$temp,medi$count,pch=16,cex=0.5,main = 'Linear model fit for median data',
                xlab = 'Temperature', ylab = 'Count')
abline(median.lm,lwd=1.5)

#### Mixed model ####

int.mean <- lmer(logc ~ temp + (1|commonname),data = meandat,REML = F)
VarCorr(int.mean)
display(int.mean)
summary(int.mean)


result.mean <- lmer(logc ~ temp + (temp|commonname),data = meandat,REML = F)
display(result.mean)
summary(result.mean)
VarCorr(result.mean)

int.median <- lmer(logc ~ temp + Turb + (1|commonname),data = medidat)
VarCorr(int.median)
result.median <- lmer(logc ~ temp + Turb + TP + TN + (TN|commonname),data = medidat)
summary(result.median)
VarCorr(result.median) %>% as.data.frame()

fixef(int.median)
ranef(int.median)
resid(int.median) %>% hist(,main='Histogram of Residuals fit by random intercept model',
                           xlab = 'Residuals')
resid(int.median) %>% qqnorm() 
resid(int.median) %>% qqline(,lwd=2) 


plot(mod$TN,mod$CPUE)
ggplot(mod,aes(CPUE,TN)) +
  geom_point()
ggplot(mod,aes(CPUE,TP)) +
  geom_point()
ggplot(mod,aes(CPUE,Turbidity__NTU_)) +
  geom_point()



result1 <- lmer(CPUE ~ Turbidity__NTU_ + TP + TN + (1|commonname),data = mod)
summary(result1)

