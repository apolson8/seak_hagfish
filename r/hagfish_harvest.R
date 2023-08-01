#HAGFISH HARVEST DATA#
library(ggplot2)
library(dplyr)
library(readxl)
library(ggmap)
library(gridExtra)
library(car)
library(extrafont)
library(lubridate)
library(scales)

#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#COLOR BLIND PALETTE#
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#NORTHERN PACIFIC HARVEST (BC, WA, OR, & CA)#
#Data obtained from agency websites and PACFIN database#
pacific_harvest <- read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/fishery/raw_data/hagfish landings WA, OR, CA.xlsx")

pacific_harvest$area <- factor(pacific_harvest$area, levels = c("BC", "WA", "OR", "CA"))

tiff(filename = "H:/Groundfish/Hagfish/seak_hagfish/figures/pacific coast harvest.tiff",
     width = 7, height = 5, units = "in", res = 600, compression = "lzw")

ggplot(pacific_harvest, aes(year, lbs, colour = factor(area))) + geom_point(size = 3) + geom_line(size = 1) +
  xlab("Year") + ylab("Harvest (lbs)") + scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = pretty(pacific_harvest$year , n =5)) +
  scale_fill_manual(values= cbPalette) + scale_color_manual(values = cbPalette) +
  theme(legend.title = element_blank(), legend.position = c(0.75, 0.75))

dev.off()

#SOUTHEAST HARVEST#
hag_harvest <- read_excel("h:/Groundfish/Hagfish/seak_hagfish/data/fishery/raw_data/hagfish fishticket data.xlsx")

hag_harvest$G_STAT_AREA <-as.factor(hag_harvest$G_STAT_AREA)

harvest <- hag_harvest %>% filter(G_CFEC_FISHERY_GROUP == "Misc. Finfish") %>% 
  group_by(YEAR) %>%
  summarise(total_harvest = sum(ROUND_POUNDS))


tiff(filename = "H:/Groundfish/Hagfish/seak_hagfish/figures/seak harvest.tiff",
     width = 7, height = 5, units = "in", res = 600, compression = "lzw")


ggplot(harvest, aes(YEAR, total_harvest)) + geom_bar(stat = "identity") +
  xlab("Year") + ylab("Total Harvest (lbs)") +
  scale_y_continuous(label = comma) +
  scale_x_continuous(breaks = pretty(harvest$YEAR, n = 5))

dev.off()

