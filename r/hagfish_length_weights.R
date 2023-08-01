#HAGFISH SURVEY DATA#
library(ggplot2)
library(dplyr)
library(readxl)
library(ggmap)
library(gridExtra)
library(car)
library(extrafont)
library(ggridges)
library(janitor)

#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#IMPORT HAGFISH DATA#
#SPECIMEN FORM#
read_excel("data/survey/raw_data/2016-2017 Hagfish Set and Bio Data Complete .xlsx", sheet = 1) %>%
  clean_names() -> hag_bio

hag_bio %>% 
  select(year, location, species_code, length_mm, sex) %>% 
  filter(species_code==202 & !is.na(sex)) %>% 
  group_by(location) -> lgth

lgth

lgth %>%
  ggplot(aes(x = sex, y= length_mm)) + 
  geom_col(aes(fill = factor(sex), stat = "bin")) +
  facet_wrap(~location)

#LENGTH/WEIGHT RELATIONSHIP#
hag_bio %>%
  filter(species_code == 202) -> hg_lw


hg_lw %>%
  ggplot(aes(length_mm, weight_g)) +
  geom_point(aes(color = factor(sex))) +
  stat_smooth()


#LOG TRANSFORMED X & Y AXES#
hg_lw %>%
  ggplot(aes(log(length_mm),
             log(weight_g),
             color = (factor(sex)))) +
  geom_point(aes(color = factor(sex))) +
  ylab("log weight (g)") +
  xlab("log length (mm)") +
  geom_smooth(method = 'lm')



wlhag <- function(dat,a,b){
  return(list(p=ggplot(dat,aes(log(length_mm),log(weight_g))) +
                geom_point() +
                geom_abline(intercept=log(a),slope=b)+
                ylim(c(0,10)) + xlim(c(0,10)),
              ss=sum((log(dat$weight_g) - log(a) -
                        b*log(dat$length_mm))^2)))
}
wlhag

#L vs W RELATIONSHIP#
##NOT SURE IF STAT SMOOTH AUTO DOES THE CORRECT FIT FOR THIS#
wlHag_plot<-ggplot(data=hg_lw, aes(x=length_mm, y=log(weight_g), colour=(factor(sex)))) + 
  geom_point(size=2) + ylab("log Weight (g)") + xlab("Length (mm)") + 
  ggtitle("E. deani Length/Weight Relationship") + 
  stat_smooth(aes(fill=factor(sex)))

wlHag_plot

#Survey Length Frequency#
hg_length <- read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/survey/raw_data/2016-2017 Hagfish set and bio data.xlsx", sheet = 3)

lendat<- bind_rows(hg_length %>% filter(!is.na(Length_cm) & (!is.na(Sex)) & Species_Code == 202) %>%
            mutate(Sex, sex_name = ifelse(Sex == 0, "Unknown",
                   ifelse(Sex == 1, "Male", "Female"))) %>%
            select(Year, sex_name, Length_cm) %>% mutate(Source = "Survey") %>%
            filter(!c(Length_cm < 5 | Length_cm > 100)) %>%
            mutate(length2 = ifelse(Length_cm < 6, 6,
                                    ifelse(Length_cm > 99, 99, Length_cm)),
                   length_bin = cut(length2, breaks = seq(4, 99, 2),
                                    labels = paste(seq(6, 99, 2))))) %>% select(-length2)

lencomps<- lendat %>% count(Source, Year, sex_name, length_bin) %>% 
  group_by(Source, Year, sex_name) %>% mutate(proportion = round(n / sum(n), 4))

lencomps %>% group_by(Source, Year, sex_name) %>% summarise(sum(proportion))


survey_prop <- hg_length %>% group_by(Year, Sex, Length_cm) %>% 
  summarise(n = n()) %>% mutate(freq = n / sum(n))

survey_prop

tiff(filename = "H:/Groundfish/Hagfish/seak_hagfish/figures/Hagfish Survey Lengths.tiff",
     width = 8, height = 5, units = "in", res = 600, compression = "lzw")

ggplot(lencomps) + geom_bar(aes(x = length_bin, y = proportion),
                            stat = "identity") + 
  facet_wrap(~Year + sex_name, ncol = 3) + xlab("Length (cm)") + ylab("Proportion") +
  scale_x_discrete(breaks = seq(6, 99, 6),
                   labels = seq(6, 99, 6)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dev.off()

#Fishery Length Frequencies#
fishery_length <- read_excel("H:/Groundfish/Hagfish/seak_hagfish/data/fishery/raw_data/hagfish port sampling data.xlsx")

fishery_length$YEAR <- as.factor(fishery_length$YEAR)

hg_length <- fishery_length %>% mutate(length_cm = LENGTH_MILLIMETERS / 10) %>% 
  filter(!is.na(length_cm))


ggplot(hg_length, aes(x = length_cm, y = YEAR, group = YEAR, fill = YEAR)) + 
  geom_density_ridges(aes(point_fill = YEAR, point_color = YEAR),
                      jittered_points = TRUE, scale = 3.0, alpha = 0.3, point_alpha = 1, lwd = 0.75) +
  geom_vline(xintercept = 51.6, linetype = "dashed", lwd = 1.25) +
  geom_vline(xintercept = 44.4, linetype = "dashed", lwd = 1.25, color = "red") +
  xlim(20, 90) +  
  xlab("Length (cm)") + ylab("Year") +
  theme(legend.position = "none")


