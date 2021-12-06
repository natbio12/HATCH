## HATCH Respirometry early round summer 2021
library(ggplot2)
setwd("~/Documents/AUBURN/Projects/Metabolic_Rate/Late_Summer")

## Load data
resp<-read.csv("~/Documents/AUBURN/Projects/Metabolic_Rate/Late_Summer/RQ_HATCH_LateSummer.csv")

#Check variables
str(resp)

#pdf("Respirometry_Late_Summer.pdf")
#Function to add N to the boxplot
stat_box_data <- function(y, upper_limit = max(resp$RQ) * 1.3) {
  return( 
    data.frame(
      y = 0.8 * upper_limit,
      label = paste('n =', length(y))
    )
  )
}

#Boxplot for both species grouping by treatment
RQBoth=ggplot(resp, aes(x=Species, y=RQ, fill=Treatment)) + 
  ggtitle("Both Species \n RQ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(breaks = c("Control ", "HighTemp"), values=c("blue", "red")) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  geom_boxplot()
RQBoth

#Subset by species 
respDPSE<-subset(resp, resp$Species=="DPSE", na.rm=TRUE, select = c(Species, Sex, Treatment, CompleteTreatment, Line, RQ))
respDMEL<-subset(resp, resp$Species=="DMEL", na.rm=TRUE, select = c(Species, Sex, Treatment, CompleteTreatment, Line, RQ))

#Boxplot for DMEL by complete treatment
RQTreatmentDMEL=ggplot(respDMEL, aes(x=Treatment, y=RQ, fill=CompleteTreatment)) + 
  ggtitle("Drosophila melanogaster RQ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(breaks = c("Control_Kept", "HighTempt_Kept", "Control_Switched",  "HighTempt_Switched"), 
                    values=c("darkblue", "darkred", "blue", "red")) +
  geom_boxplot()
RQTreatmentDMEL

#DMEL by treatment and sex
RQSexDMEL=ggplot(respDMEL, aes(x=Sex, y=RQ, fill=Treatment)) + 
  ggtitle("Drosophila melanogaster RQ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(breaks = c("Control ", "HighTemp"), 
                    values=c("blue", "red")) +
  geom_boxplot()
RQSexDMEL

#Boxplot for DPSE by complete treatment
RQTreatmentDPSE=ggplot(respDPSE, aes(x=Treatment, y=RQ, fill=CompleteTreatment)) + 
  ggtitle("Drosophila pseudoobscura RQ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(breaks = c("Control_Kept", "HighTempt_Kept", "Control_Switched",  "HighTempt_Switched"), 
                    values=c("darkblue", "darkred", "blue", "red")) +
  geom_boxplot()
RQTreatmentDPSE

#DPSE by treatment and sex
RQSexDPSE=ggplot(respDPSE, aes(x=Sex, y=RQ, fill=Treatment)) + 
  ggtitle("Drosophila pseudoobscura RQ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(breaks = c("Control ", "HighTemp"), 
                    values=c("blue", "red")) +
  geom_boxplot()
RQSexDPSE
#dev.off()
####ANOVA to analyze the variance of the means 
# Compute the analysis of variance for species 
res.aov <- aov(RQ ~ Species, data = resp)
# Summary of the analysis
summary(res.aov)

# Compute the analysis of variance for species and treatments
res.aov2 <- aov(RQ ~ Species+Treatment, data = resp)
# Summary of the analysis
summary(res.aov2)

###Analyze data by species individually
##DPSE
# Compute the analysis of variance for DPSE for treatment
res.aov3 <- aov(RQ ~ Treatment+ CompleteTreatment, data = respDPSE)
# Summary of the analysis
summary(res.aov3)

#Compute the analysis of variance for DPSE for treatment and sex
res.aov4 <- aov(RQ ~ Treatment+ Sex, data = respDPSE)
# Summary of the analysis
summary(res.aov4)

##DMEL
# Compute the analysis of variance for DPSE for treatment
res.aov5 <- aov(RQ ~ Treatment+ CompleteTreatment, data = respDMEL)
# Summary of the analysis
summary(res.aov5)

#Compute the analysis of variance for DMEL for treatment, complete treatment and sex
res.aov6 <- aov(RQ ~ Treatment+ CompleteTreatment + Sex, data = respDMEL)
# Summary of the analysis
summary(res.aov6)

