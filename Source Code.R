library(readxl)
hosp <- read.csv('C:/Users/pavan/Desktop/python project/Project/Projects for Submission/Healthcare/Healthcare/HospitalCosts.csv')
print(hosp)
library(ggplot2)
r = range(hosp$AGE)
print(r)
min(hosp$AGE)
max(hosp$AGE)
median(hosp$AGE)
quantile(hosp$AGE)
summary(hosp$AGE)
hist(hosp$AGE,xlab = "Age",col = "skyblue",xlim = c(0,20), ylim = c(0,400),breaks = 5,main = "Histogram of AGE")

r2 <- range(hosp$TOTCHG)
print(r2)
min(hosp$TOTCHG)
max(hosp$TOTCHG)
median(hosp$TOTCHG)
quantile(hosp$TOTCHG)
hist(hosp$TOTCHG,xlab = "Total Charge",col = "skyblue",ylim = c(0,500),main = "Histogram of Total Charge")

plot(hosp$AGE,hosp$TOTCHG, xlab = "Age", ylab = "Total Charge",xlim = c(0,20), ylim = c(0,50000))

hist(hosp$APRDRG,main = "Histogram of All Patient Refined Diagnosis Related Groups")

plot(hosp$AGE,hosp$APRDRG,xlab = "Age", ylab = "APRDRG")

hist(hosp$RACE,xlab = "Patient Race",main = "Histogram of Patient Race")

summary(hosp$RACE)
RACE <- as.factor(hosp$RACE)
summary(RACE)

plot(hosp$RACE,hosp$TOTCHG,main = "Total charge per Race", xlab = "Patient RACE", ylab = "Total Charge" )

plot <- ggplot(hosp,aes(AGE,TOTCHG((,fill=FEMALE))
plot <- plot + geom_bar(stat = "identity", position = 'dodge')
plot

gfg <- data.frame(x=hosp$TOTCHG,grp=rep(hosp$AGE),each=2,subgrp=LETTERS[0:2])
ggplot(gfg,aes(x = grp, y =x, fill = subgrp)) +
  geom_bar(stat = "identity", position = "dodge")

install.packages("tidyverse")
library(tidyverse)

sum(is.na(hosp))
hosp <- na.omit(hosp)
sum(is.na(hosp))
attach(hosp)

boxplot(hosp)
check <- hosp[,-5]
boxplot(check)
check1 <- check[,-5]
boxplot(check1)
check2 <- check1[,-3]
boxplot(check2)

summary(TOTCHG)
IQR_totchg<-2530-1218    
upfen_totchg=2530+1.5*IQR_totchg
upfen_totchg

summary(APRDRG)
IQR_aprgrg <- 751-640
upfen_aprdrg=751+1.5*IQR_aprgrg
upfen_aprdrg

summary(LOS)
IQR_los <- 3-2
upfen_los <- 3+1.5*IQR_los
upfen_los

summary(RACE)
IQR_race <- 484
upfen_race <- 484+1.5*IQR_race
upfen_race
hosp <- na.omit(hosp)
mydata <- subset(hosp,AGE<=17 & FEMALE<=1 & LOS<=4.5 & RACE<=1 & TOTCHG<=2530 & APRDRG>=640)
mydata <- na.omit(mydata)

boxplot(mydata)

model <- lm(mydata$LOS~mydata$AGE+mydata$FEMALE+mydata$RACE)
summary(model)
