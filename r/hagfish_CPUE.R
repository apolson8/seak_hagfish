#HAGFISH CPUE DATA#
library(ggplot2)
library(dplyr)
library(readxl)
library(ggmap)
library(gridExtra)
library(car)
library(extrafont)
library(lubridate)

#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#soak time of hagfish pots by year and locaiton#
#SURVEY POT SET FORM#
HG_SET<-read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/survey/raw_data/2016-2017 Hagfish set and bio data.xlsx", sheet=1)

soak_time <- HG_SET %>% group_by(YEAR, Location, STATION_NO) %>%
  mutate(soak_start = ymd_hms(TIME_SET_BEGIN), soak_end = ymd_hms(TIME_LIFT_COMPLETE),
         duration = as.numeric(soak_end - soak_start, units = "hours")) 
  
avg_soak_time <-soak_time %>% group_by(YEAR, Location) %>% n()
  summarise(mean_soak = mean(duration), sd = sd(duration))

avg_soak_time


#SURVEY POT DETAILS FORM#
hg_survey <- read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/survey/raw_data/2016-2017 Hagfish set and bio data.xlsx", sheet = 3)

survey_cpue <- hg_survey %>% filter(!is.na(No_Hagfish)) %>% group_by(Year, Location) %>% mutate(cpue_no_per_trap = No_Hagfish/1) %>%
  summarise('Avg. CPUE (no. of fish per pot)' = mean(cpue_no_per_trap), 'Std. Dev' = sd(cpue_no_per_trap)) 

survey_cpue 

survey_cpue_weight <-hg_survey %>% filter(!is.na(`Sample_Weight(kg)`)) %>% group_by(Year, Location) %>% 
  mutate(`cpue(kg/pot)`= `Sample_Weight(kg)`/1) %>% 
  summarise('Avg. CPUE (kg/pot)' = mean(`cpue(kg/pot)`),'Std. Dev' = sd(`cpue(kg/pot)`))

survey_cpue_weight



