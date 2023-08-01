#HAGFISH TEMPERATURE DATA#
library(ggplot2)
library(dplyr)
library(readxl)
library(ggmap)
library(gridExtra)
library(car)
library(extrafont)

#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#IMPORT HAGFISH DATA#
#TEMP DETAILS FORM#
HG_TEMP<-read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/survey/raw_data/2016-2017 Hagfish set and bio data.xlsx", sheet=4)

#AVG HOURLY TEMP FOR HAGFISH#
HG_TEMP1<-HG_TEMP %>% filter (Status==1) %>% group_by(Location) %>% 
  summarise(Avg=mean(Temp), SD=sd(Temp))

HG_TEMP1

limits<-aes(ymax=Avg + SD, ymin=Avg - SD)

HG_TEMPplot<-ggplot(data=HG_TEMP1, aes(x=Location, y=Avg), colour="Location") +
  ylim(0,6.5) + ylab("Avg Hourly Temp (degrees C) w/ SD") + geom_errorbar(limits, width=0.25) +
  geom_point(colour="dodgerblue1", size=4)

HG_TEMPplot
