#Aaron Baldwin, Andrew Olson, & Rhea Ehresmann

#Objective: summarise survey and fishery information for hagfish speceis in Southeast Alaska.

#SEAK black hagfish survey, fishery, and biological information


#load -----
library(ggplot2)
library(dplyr)
library(readxl)
library(ggmap)
library(gridExtra)
library(car)
library(extrafont)
library(janitor)
library(sizeMat)
library(lubridate)
library(cowplot)
library(ggpubr)

# global ---------
cur_yr = 2022 # most recent year of data
fig_path <- paste0('figures/', cur_yr) # folder to hold all figs for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('output/', cur_yr) # output and results
dir.create(output_path) 


#COLOR BLIND PALETTE----
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# load data ------------
# Import survey and fishery port sampling data
read_excel("data/survey/raw_data/2016-2017 Hagfish Survey Bio Data Clean 01-17-23.xlsx", sheet = 1) %>%
  clean_names() %>%
  mutate(length_cm = (length_mm/10))-> hag_bio

read.csv("data/fishery/raw_data/hagfish port sampling data.csv") %>%
  clean_names() %>%
  mutate(length_cm = (length_millimeters/10))-> hag_port


#FECUNDITY OF FEMALES----

#need to determine best model fit the data
hag_bio %>%
  filter(species_code == 202,
         sex == 2,
         maturity_stage == 3) %>%
  group_by(maturity_stage) -> fecund_hag

hist(fecund_hag$no_eggs)

#Histogram
fecund_hag %>%
  ggplot(aes(no_eggs)) +
  geom_histogram(binwidth = 5,
                 alpha = 0.2,
                 color = "black") +
  ylab("Count") +
  xlab("Number of Mature Eggs (Stage 3)") -> fecund_plot

fecund_plot

ggsave(paste0(fig_path, '/hagfish fecundity.png'), width = 6, height = 5, units = "in", dpi = 400)


#fit linear regression model to dataset and view model summary
model <- lm(no_eggs~length_cm, data = fecund_hag)

summary(model)


#linear regression plot
fecund_hag %>%
  ggplot(aes(length_cm, no_eggs)) + 
  geom_point() +
  geom_smooth(method = 'lm') + 
  stat_regline_equation(label.y = 50, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 48, aes(label = ..rr.label..)) +
  ylab("Number of Mature Eggs (Stage 3)") + 
  scale_x_continuous(breaks = seq(0,200, by = 5),
                     name = "Total Length (cm)") -> fecund_model_plot

fecund_model_plot
  
ggsave(paste0(fig_path, '/hagfish fecundity model.png'), width = 6, height = 5, units = "in", dpi = 400)


plot_grid(fecund_plot, fecund_model_plot,
          labels = c('A', 'B'),
          label_size = 12)

ggsave(paste0(fig_path, '/hagfish fecundity hist and model.png'), width = 8, height = 5, units = "in", dpi = 400)

#REPROD. STAGES -----
#Need to convert male stages to negative values so they can be on the same graph as females
hag_bio %>% 
  filter(sex != 0,
         !is.na(sex),
         maturity_stage != 0,
         !is.na(maturity_stage)) %>% 
  mutate(stage = ifelse(sex == 1 & maturity_stage == 1, -1,
                        ifelse(sex == 1 & maturity_stage == 2, -2,
                               ifelse(sex == 1 & maturity_stage == 3, -3,
                                      ifelse(sex == 1 & maturity_stage == 4, -3,
                                             ifelse(sex == 2 & maturity_stage == 1, 1,
                                                    ifelse(sex == 2 & maturity_stage == 2, 2,
                                                           ifelse(sex == 2 & maturity_stage == 3, 3,
                                                                  ifelse(sex == 2 & maturity_stage == 4, 3,
                                                                         ifelse(sex == 2 & maturity_stage == 5, 3, "NA")))))))))) %>%
  mutate(sex_name = ifelse(sex == 1, "Male",
                           ifelse(sex == 2, "Female", "Unknown"))) %>% mutate(length_cm = length_mm / 10) -> reprod


ggplot(reprod, 
       aes(stage, length_cm, fill = sex_name)) + 
  geom_boxplot() + 
  coord_flip() + 
  geom_jitter() +
  geom_vline(xintercept = 3.5,
             linetype = "dashed") +
  scale_fill_manual(values= cbPalette) + 
  scale_color_manual(values = cbPalette) +
  xlab("Maturity Stage") + 
  ylab("Total Length (cm)") + 
  scale_y_continuous(breaks = pretty(reprod$length_cm, n = 10)) +
  theme(legend.position = c(0.25, 0.75), legend.title = element_blank())

ggsave(paste0(fig_path, '/hagfish_maturity_stages_condensed.png'), width = 6, height = 7, units = "in", dpi = 400)






#SIZE AT MATURITY----
hag_bio %>%
  filter(species_code == 202) %>%
  mutate(length_cm = length_mm/10) -> hag_bio

hag_sam = sizeMat::gonad_mature(data = hag_bio,
                                varNames = c("length_cm", "mature"),
                                inmName = 0,
                                matName = 1,
                                method = "fq",
                                niter = 1000)

hag_sam %>% print()

## formula: Y = 1/1+exp-(A + B*X)

hag_sam$out %>%
  as_tibble() %>%
  ggplot(aes(x = x))+
  geom_point(aes(y = mature, x = x)) + 
  geom_line(aes(y = fitted), linetype = "solid", size = 1.2) +
  geom_ribbon(aes(ymin = CIlower, ymax = CIupper), alpha = 0.3) +
  #geom_line(aes(y = CIlower), col = "blue", linetype = 6, size = .8)+
  #geom_line(aes(y = CIupper), col = "blue", linetype = 6, size = .8)+
  # modelr::geom_ref_line(h = 0.5, colour = "red", size = 1)+
  labs(x = "Total length (cm)", y = "Proportion Mature")+
  geom_segment(aes(x = 10, xend = 49.3377 , y = 0.5, yend = .5), 
               #color ="red", 
               size =1.2, 
               linetype = "dashed")+
  geom_segment(aes(x = 49.337, xend = 49.337 , y = 0, yend = .5), 
               #color ="red", 
               size =1.2, 
               linetype = "dashed") +
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(0,100,10))+
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0.25,1,.25)) +
  geom_text(x = 20, y = 0.85, label = expression(L[50]~48.5))


#Male SAM----
hag_bio %>%
  filter(sex == 1) -> hag_bio_m

hag_sam_m = sizeMat::gonad_mature(data = hag_bio_m, 
                                  varNames = c("length_cm", "mature"), 
                                  inmName = 0, 
                                  matName = 1, 
                                  method = "fq",
                                  niter = 1000)

hag_sam_m %>% print()

#Female SAM----
hag_bio %>%
  filter(sex == 2) -> hag_bio_f

hag_sam_f = sizeMat::gonad_mature(data = hag_bio_f, 
                                  varNames = c("length_cm", "mature"), 
                                  inmName = 0, 
                                  matName = 1, 
                                  method = "fq",
                                  niter = 1000)

hag_sam_f %>% print()


#Combine Male and Female SAM----
hag_sam_f$out %>%
  as_tibble() %>%
  mutate(sex = "Female") %>%
  bind_rows(hag_sam_m$out %>%
              as_tibble() %>%
              mutate(sex = "Male")) %>%
  ggplot(aes(x = x, y = fitted, 
             color = sex, 
             fill = sex,
             linetype = sex,
             name = "E. deani")) +
  geom_point(aes(x, mature, color = sex),
             size = 4) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = CIlower, ymax = CIupper), 
              alpha = 0.3) +
  geom_segment(aes(x = 10, xend = 51.7198 , y = 0.5, yend = .5), 
               color ="#999999", size =.75, linetype = "dashed", show.legend = FALSE)+
  geom_segment(aes(x = 51.798, xend = 51.798 , y = 0, yend = .5), 
               color ="#999999", size =.75, linetype = "dashed", show.legend = FALSE) +
  geom_segment(aes(x = 10, xend = 44.551 , y = 0.5, yend = .5), 
               color ="#E69F00", size =.75, linetype = "dashed", show.legend = FALSE)+
  geom_segment(aes(x = 44.551, xend = 44.551 , y = 0, yend = .5), 
               color ="#E69F00", size =.75, linetype = "dashed", show.legend = FALSE) +
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(0,100,5))+
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0.25,1,.25)) +
  #scale_fill_viridis_d() +
  #scale_colour_viridis_d() +
  scale_color_manual(values = cbPalette) +
  scale_fill_manual(values = cbPalette) +
  labs(x = "Total Length (cm)",
       y = "Proprition Mature") +
  annotate(geom = "text", 
           x = 32,
           y = 0.8,
           label = expression(L[50]~51.7), color = "#999999") +
  annotate(geom = "text", 
           x = 32,
           y = 0.75,
           label = expression(L[50]~44.5), color = "#E69F00") +
  
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.785))

ggsave(paste0(fig_path, '/hagfish maturity.png'), width = 6, height = 5, units = "in", dpi = 400)


#LENGTH FREQUENCIES----
#Escape hole size (survey vs fishery)----
hag_bio %>%
  filter(species_code == 202,
         !is.na(length_cm)) %>%
  select(year, length_cm) %>%
  mutate(escape_hole_size_cm = 0.95,
         data_source = "survey") -> length_survey
  
hag_port %>%
  filter(species_code == 202,
         !is.na(length_cm),
         year <= 2020) %>%
  select(year, length_cm) %>%
  mutate(escape_hole_size_cm = ifelse(year < 2018, 1.6,
                                      ifelse(year >= 2018, 1.9, "NA")),
         data_source = "fishery") -> length_fishery

#library(plyr)
all_data<- rbind.fill(length_survey, length_fishery)

escape_hole_labs <-c("0.95 cm", "1.6 cm", "1.9 cm")
names(escape_hole_labs) <-c(0.95, 1.6, 1.9)

all_data %>%
  ggplot(aes(length_cm,
         fill = escape_hole_size_cm,
         color = escape_hole_size_cm)) +
  geom_density(alpha = 0.3,
                 bins = 30) +
  scale_color_manual(values = cbPalette,
                     name = "Escape Hole Size (cm)") +
  scale_fill_manual(values = cbPalette,
                    name = "Escape Hole Size (cm)") + 
  labs(x = "Total Length (cm)",
       y = "Density") +
  scale_x_continuous(breaks = seq(0,200, by = 5)) +
  geom_vline(xintercept = 50.4,
             linetype = "dotted",
             size = 1.5,
             color = "black") +
  geom_vline(xintercept = 44.5,
             linetype = "dashed",
             size = 1.5,
             color = "black") + 
  facet_wrap(~escape_hole_size_cm,
             ncol = 1,
             labeller = labeller(escape_hole_size_cm = escape_hole_labs)) +
  theme(legend.position = "bottom",
        strip.background = element_blank())
 
ggsave(paste0(fig_path, '/hagfish escape hole length comp.png'), width = 6, height = 8, units = "in", dpi = 400)


