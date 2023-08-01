#HAGFISH SURVEY AND FISHERY MAPS#
library(ggplot2)
library(dplyr)
library(readxl)
library(ggmap)
library(gridExtra)
library(car)
library(extrafont)
library(rgeos)
library(maptools)
library(rgdal)

#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#IMPORT HAGFISH DATA#
#SURVEY POT SET FORM#
HG_SET<-read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/survey/raw_data/2016-2017 Hagfish set and bio data.xlsx", sheet=1)

#SURVEY POT DETAILS FORM#
HG_POT<-read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/survey/raw_data/2016-2017 Hagfish set and bio data.xlsx", sheet=2)

Hag_Location<-c(lon=-131.7279, lat=55.7674833)
Hag_Location<-c(-132.5,55.0,-131.5,56.5)

Hag_Map<-ggmap(get_map(location=Hag_Location, zoom = 9, source="google")) + 
  geom_point(aes(x=long, y=lat, size=No_Hagfish), data=HG_SET, colour="blue") +
  ylab("DD Latitude") + xlab("DD Longitude")

Hag_Map

#ADD STAT AREA & SEAK MAP SHAPEFILES#
groundfish_stat <- readOGR("H:/Groundfish/ArcGIS/base_maps/pvg_stat_2001.shp")

groundfish_stat_df <-fortify(groundfish_stat)

se_map <-readOGR("H:/Groundfish/ArcGIS/base_maps/p4_se_alaska.shp")

se_map_df <-fortify(se_map)

#FISHERY MAPS#
HG_logbook <-read_excel("h:/Groundfish/Hagfish/seak_hagfish/data/fishery/raw_data/hagfish logbook data.xlsx")

Hag_Location<-c(-132.25,55.0,-130.5,56.0)


Hag_fishery_map<-ggmap(get_map(location=Hag_Location, zoom = 10, source="stamen", maptype = "toner-lite")) + 
  geom_point(aes(x=long_start, y=lat_start, size=fishticket_lbs), data=HG_logbook, colour="dodgerblue1") +
  geom_path(data = groundfish_stat_df, aes(long, lat, group = group), 
            color = 'black', size = 0.5) +
  ylab("DD Latitude") + xlab("DD Longitude") 
 

tiff(filename = "H:/Groundfish/Hagfish/seak_hagfish/figures/hagfish_fishery_harvest_map.tiff",
     width = 7, height = 5, units = "in", res = 600, compression = "lzw")


Hag_fishery_map

dev.off()

#Survey catch/distribution
#SURVEY POT SET FORM#
HG_SET<-read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/survey/raw_data/2016-2017 Hagfish set and bio data.xlsx", sheet=1)

#SURVEY POT DETAILS FORM#
HG_POT<-read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/survey/raw_data/2016-2017 Hagfish set and bio data.xlsx", sheet=2)

Hag_Location<-c(-132.5,55.0,-130.5,56.5)

Hag_Map<-ggmap(get_map(location=Hag_Location, zoom = 9, source="stamen", maptype = "toner-lite")) + 
  geom_point(aes(x=long, y=lat, size=No_Hagfish), data=HG_SET, colour="dodgerblue1") + 
  geom_path(data = groundfish_stat_df, aes(long, lat, group = group), 
            color = 'black', size = 0.5) +
  ylab("DD Latitude") + xlab("DD Longitude")

tiff(filename = "H:/Groundfish/Hagfish/seak_hagfish/figures/hagfish_survey_catch_map.tiff",
     width = 7, height = 5, units = "in", res = 600, compression = "lzw")

Hag_Map

dev.off()

#SSEI SABLEFISH LL SURVEY#
#EVIDENCE OF HAGFISH IN OTHER AREAS FROM SLIME ON HOOKS AND BYCATCH#

#IMPORT SET, HOOK, AND CATCH ACCOUNTING FORMS#
ssei_set <-read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/survey/raw_data/ssei LL survey set info.xlsx", sheet = 1)

ssei_catch <-read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/survey/raw_data/ssei LL survey catch.xlsx", sheet = 1)

ssei_hook <-read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/survey/raw_data/ssei LL survey hook accounting.xlsx", sheet = 1)

#SUMMARSIE HOOK AND CATCH FORMS THEN JOIN WITH SET FORM#
#FILTER FOR HAGFISH SPECIES CODES 202 & 212#
set_summary <-ssei_set %>% 
  select(Year, `Trip No`, `Set No`, `G Stat Area`, `Start Latitude Decimal Degrees`, `Start Longitude Decimal Degree`)

set_summary


catch_summary <- ssei_catch %>% group_by(Year, `Trip No`, `Set No`) %>% filter(`Species Code` == 212) %>%
  summarise(total_hagfish = sum(`Fish - Number Caught`))

catch_summary


hook_summary <- ssei_hook %>% group_by(Year, `Trip No`, `Set No`) %>% filter(Year >= 2016) %>%
  summarise(slimed_hooks = sum(`Hooks - Hagfish Slime`))

hook_summary


set_catch_summary <- inner_join(set_summary, catch_summary, by = c("Year", "Trip No", "Set No"))

write.csv(set_catch_summary, "H:/Groundfish/Hagfish/seak_hagfish/output/ssei_ll_survey_hagfish_evidence.csv")

set_catch_hook_summary <- inner_join(set_catch_summary, hook_summary, by = c("Year", "Trip No", "Set No")) 


Hag_Location<-c(-133.0,54.3,-131.0,56.2)


ssei_ll_survey<-ggmap(get_map(location=Hag_Location, zoom = 8, source = "stamen", maptype = "toner-lines",
                              color = "bw", messaging = FALSE)) +
  geom_point(aes(x=`Start Longitude Decimal Degree`, y=`Start Latitude Decimal Degrees`), 
             data=set_catch_summary, colour="dodgerblue1") + 
  geom_path(data = groundfish_stat_df, aes(long, lat, group = group), 
            color = 'black', size = 0.5) +
  coord_map() +
  ylab("DD Latitude") + xlab("DD Longitude")

tiff(filename = "H:/Groundfish/Hagfish/seak_hagfish/figures/ssei_llsurvey_hagfish_evidence.tiff",
     width = 7, height = 10, units = "in", res = 600, compression = "lzw")

ssei_ll_survey

dev.off()
