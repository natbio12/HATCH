## HATCH CTirometry early round summer 2021
library(ggplot2)
library(tidyr)
library(tidyverse)

setwd("~/Documents/AUBURN/Projects/Metabolic_Rate/Late_Summer")

## Load data
CT<-read.csv("~/Documents/AUBURN/Projects/Metabolic_Rate/Late_Summer/CT_Max_LatteSummer.csv", stringsAsFactors = FALSE)

#Check variables
str(CT)

#pdf("CTMAX_LateHATCH.pdf")
#Function to add N to the boxplot
stat_box_data <- function(y, upper_limit = max(CT$CT.max.at.) * 1.3) {
  return( 
    data.frame(
      y = 0.8 * upper_limit,
      label = paste('n =', length(y))
    )
  )
}

#Boxplot for both species grouping by treatment
CTBoth=ggplot(CT, aes(x=Species, y=CT.max.at., fill=Treatment)) + 
  ggtitle("Both Species CT") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Species") + ylab("Temperature °C") +
  ylim(33,45) +
  scale_fill_manual(breaks = c("Control", "HighTemp"), values=c("blue", "red")) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  geom_boxplot()
CTBoth

#Subset by species 
CTDPSE<-subset(CT, CT$Species=="DPSE", na.rm=TRUE, select = c(Species, Sex., Treatment, CompleteTreatment, CT.max.at.))
CTDMEL<-subset(CT, CT$Species=="DMEL", na.rm=TRUE, select = c(Species, Sex., Treatment, CompleteTreatment, CT.max.at.))

#Boxplot for DMEL by complete treatment
CT.max.at.TreatmentDMEL=ggplot(CTDMEL, aes(x=Treatment, y=CT.max.at., fill=CompleteTreatment)) + 
  ggtitle("Drosophila melanogaster CT Max") +
  xlab("Species") + ylab("Temperature °C") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(35,42.5) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(breaks = c("Control_Kept", "HighTemp_Kept", "Control_Switched",  "HighTemp_Switched"), 
                    values=c("darkblue", "darkred", "blue", "red")) +
  geom_boxplot()
CT.max.at.TreatmentDMEL

#DMEL by treatment and sex
CT.max.at.SexDMEL=ggplot(CTDMEL, aes(x=Sex., y=CT.max.at., fill=Treatment)) + 
  ggtitle("Drosophila melanogaster CT Max") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Species") + ylab("Temperature °C") +
  ylim(35,42) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(breaks = c("Control", "HighTemp"), 
                    values=c("blue", "red")) +
  geom_boxplot()
CT.max.at.SexDMEL

#Boxplot for DPSE by complete treatment
CT.max.at.TreatmentDPSE=ggplot(CTDPSE, aes(x=Treatment, y=CT.max.at., fill=CompleteTreatment)) + 
  ggtitle("Drosophila pseudoobscura CT.max.at.") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Species") + ylab("Temperature °C") +
  ylim(33,43) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(breaks = c("Control_Kept", "HighTemp_Kept", "Control_Switched",  "HighTemp_Switched"), 
                    values=c("darkblue", "darkred", "blue", "red")) +
  geom_boxplot()
CT.max.at.TreatmentDPSE

#DPSE by treatment and sex
CT.max.at.SexDPSE=ggplot(CTDPSE, aes(x=Sex., y=CT.max.at., fill=Treatment)) + 
  ggtitle("Drosophila pseudoobscura CT.max.at.") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Species") + ylab("Temperature °C") +
  ylim(33,42.5) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(breaks = c("Control", "HighTemp"), 
                    values=c("blue", "red")) +
  geom_boxplot()
CT.max.at.SexDPSE
#dev.off()
####ANOVA to analyze the variance of the means 
# Compute the analysis of variance for species 
res.aov <- aov(CT.max.at. ~ Species, data = CT)
# Summary of the analysis
summary(res.aov)

# Compute the analysis of variance for species and treatments
res.aov2 <- aov(CT.max.at. ~ Species+Treatment, data = CT)
# Summary of the analysis
summary(res.aov2)

###Analyze data by species individually
##DPSE
# Compute the analysis of variance for DPSE for treatment
res.aov3 <- aov(CT.max.at. ~ Treatment+ CompleteTreatment, data = CTDPSE)
# Summary of the analysis
summary(res.aov3)

#Compute the analysis of variance for DPSE for treatment and sex
res.aov4 <- aov(CT.max.at. ~ Treatment+ Sex., data = CTDPSE)
# Summary of the analysis
summary(res.aov4)

##DMEL
# Compute the analysis of variance for DPSE for treatment
res.aov5 <- aov(CT.max.at. ~ Treatment+ CompleteTreatment, data = CTDMEL)
# Summary of the analysis
summary(res.aov5)

#Compute the analysis of variance for DMEL for treatment, complete treatment and sex
res.aov6 <- aov(CT.max.at. ~ Treatment+ CompleteTreatment + Sex., data = CTDMEL)
# Summary of the analysis
summary(res.aov6)
