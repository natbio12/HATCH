## HATCH Respirometry age trial
library(ggplot2)
setwd("~/Documents/AUBURN/Projects/Metabolic_Rate")

## Load data
resp<-read.csv("~/Documents/AUBURN/Projects/Metabolic_Rate/RQ_HATCH_Age_DPSE.csv")

#Check variables
str(resp)

pdf("Respirometry_Age.pdf")

#Plot for both species grouping by day
Age_1=ggplot(resp, aes(x=Age, y=RQ)) + 
  ggtitle("RQ by age DPSE") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.75,1.3) +
  stat_summary(aes(y = RQ), fun=mean, colour="red", geom="line")+
  #scale_x_discrete(limits=c("2","5","7","10","20"))+
  geom_point()
Age_1

resp_female<-subset(resp, resp$Sex=="F", na.rm=TRUE, select = c(Age, Sex, RQ))
Age_1=ggplot(resp_female, aes(x=Age, y=RQ)) + 
  ggtitle("RQ by age DPSE in females") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0.75,1.3) +
  stat_summary(aes(y = RQ), fun=mean, colour="red", geom="line")+
  #scale_x_discrete(limits=c("2","5","7","10","20"))+
  geom_point()
Age_1
dev.off()

