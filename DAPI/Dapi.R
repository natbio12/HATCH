## HATCH DAPIirometry early round summer 2021
library(ggplot2)
library(tidyr)
library(tidyverse)

setwd("~/Documents/AUBURN/Projects/DAPI")

## Load data
DAPI<-read.csv("~/Documents/AUBURN/Projects/DAPI/LateSummer.csv", stringsAsFactor = FALSE)

#Check variables
str(DAPI)

pdf("DAPIMAX_LateHATCH.pdf")
#Function to add N to the boxplot
stat_box_data <- function(y, upper_limit = max(DAPI$pixels.per.um) * 0.9) {
  return( 
    data.frame(
      y = 0.8 * upper_limit,
      label = paste('n =', length(y))
    )
  )
}

#Boxplot for both species grouping by treatment
DAPIBoth=ggplot(DAPI, aes(x=Species, y=pixels.per.um, fill=Treatment)) + 
  ggtitle("Both Species DAPI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(breaks = c("Control", "HighTemp"), values=c("blue", "red")) +
  xlab("Species") + ylab("Pixels per um") +
  ylim(0,0.25) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  geom_boxplot()
DAPIBoth

#Subset by species 
DAPIDPSE<-subset(DAPI, DAPI$Species=="DPSE", na.rm=TRUE, select = c(Species, Day, Treatment, pixels.per.um))
DAPIDMEL<-subset(DAPI, DAPI$Species=="DMEL", na.rm=TRUE, select = c(Species, Day, Treatment, pixels.per.um))

#Boxplot for DMEL by complete treatment
DAPITreatmentDMEL=ggplot(DAPIDMEL, aes(x=Treatment, y=pixels.per.um, fill=Day)) + 
  ggtitle("Drosophila melanogaster DAPI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Species") + ylab("Pixels per um") +
  ylim(0,0.25) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(breaks = c("7_Control", "7_HighTemp", "2_Control",  "2_HighTemp"), 
                    values=c("darkblue", "darkred", "blue", "red")) +
  geom_boxplot()
DAPITreatmentDMEL


#Boxplot for DPSE by complete treatment
DAPITreatmentDPSE=ggplot(DAPIDPSE, aes(x=Treatment, y=pixels.per.um, fill=Day)) + 
  ggtitle("Drosophila pseudoobscura DAPI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Species") + ylab("Pixels per um") +
  ylim(0,0.25) +
  stat_summary(fun.data = stat_box_data, geom = "text", fun = median,
               position = position_dodge(width = 0.75)) +
  scale_fill_manual(breaks = c("7_Control", "7_HighTemp", "2_Control",  "2_HighTemp"), 
                    values=c("darkblue", "darkred", "blue", "red")) +
  geom_boxplot()
DAPITreatmentDPSE

dev.off()
####ANOVA to analyze the variance of the means 
# Compute the analysis of variance for species 
res.aov <- aov(pixels.per.um ~ Species, data = DAPI)
# Summary of the analysis
summary(res.aov)

# Compute the analysis of variance for species and treatments
res.aov2 <- aov(pixels.per.um ~ Species+Treatment, data = DAPI)
# Summary of the analysis
summary(res.aov2)

###Analyze data by species individually
##DPSE
# Compute the analysis of variance for DPSE for treatment and day
res.aov3 <- aov(pixels.per.um ~ Treatment+ Day, data = DAPIDPSE)
# Summary of the analysis
summary(res.aov3)


##DMEL
# Compute the analysis of variance for DPSE for treatment
res.aov5 <- aov(pixels.per.um ~ Treatment+ Day, data = DAPIDMEL)
# Summary of the analysis
summary(res.aov5)

