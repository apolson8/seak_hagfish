#HAGFISH MATURITY AND REPRODUCTION DATA#
library(ggplot2)
library(dplyr)
library(readxl)
library(ggmap)
library(gridExtra)
library(car)
library(extrafont)
library(janitor)
library(sizeMat)

# global ---------
cur_yr = 2017 # most recent year of data
fig_path <- paste0('figures/', cur_yr) # folder to hold all figs for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('output/', cur_yr) # output and results
dir.create(output_path) 

#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')
          +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

#COLOR BLIND PALETTE#
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#IMPORT HAGFISH DATA#
#SPECIMEN FORM#
read_excel("data/survey/raw_data/2016-2017 Hagfish Set and Bio Data Complete .xlsx", sheet = 1) %>%
             clean_names() -> hag_bio

#FECUNDITY OF FEMALES#
hag_bio %>%
  filter(sex == 2) %>%
  group_by (location, maturity_stage) ->fecund_hag

ggplot(fecund_hag, aes(length_mm, no_eggs, colour = location)) + geom_line() +
  geom_point()

#SIZE AT MATURITY#
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
  geom_line(aes(y = fitted), linetype = "solid", size = 1.2) +
  geom_ribbon(aes(ymin = CIlower, ymax = CIupper), alpha = 0.3) +
  #geom_line(aes(y = CIlower), col = "blue", linetype = 6, size = .8)+
  #geom_line(aes(y = CIupper), col = "blue", linetype = 6, size = .8)+
  # modelr::geom_ref_line(h = 0.5, colour = "red", size = 1)+
  labs(x = "Total length (cm)", y = "Proportion Mature")+
  geom_segment(aes(x = 10, xend = 48.4749 , y = 0.5, yend = .5), 
               #color ="red", 
               size =1.2, 
               linetype = "dashed")+
  geom_segment(aes(x = 48.4749, xend = 48.4749 , y = 0, yend = .5), 
               #color ="red", 
               size =1.2, 
               linetype = "dashed") +
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(0,100,10))+
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0.25,1,.25)) +
  geom_text(x = 20, y = 0.85, label = expression(L[50]~48.5))


#Male SAM
hag_bio %>%
  filter(sex == 1) -> hag_bio_m

hag_sam_m = sizeMat::gonad_mature(data = hag_bio_m, 
                                  varNames = c("length_cm", "mature"), 
                                  inmName = 0, 
                                  matName = 1, 
                                  method = "fq",
                                  niter = 1000)

hag_sam_m %>% print()

#Female SAM
hag_bio %>%
  filter(sex == 2) -> hag_bio_f

hag_sam_f = sizeMat::gonad_mature(data = hag_bio_f, 
                                  varNames = c("length_cm", "mature"), 
                                  inmName = 0, 
                                  matName = 1, 
                                  method = "fq",
                                  niter = 1000)

hag_sam_f %>% print()


#Combine Male and Female SAM
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
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = CIlower, ymax = CIupper), 
              alpha = 0.3) +
  geom_segment(aes(x = 10, xend = 50.4586 , y = 0.5, yend = .5), 
               color ="#999999", size =.75, linetype = "dashed", show.legend = FALSE)+
  geom_segment(aes(x = 50.4586, xend = 50.4586 , y = 0, yend = .5), 
               color ="#999999", size =.75, linetype = "dashed", show.legend = FALSE) +
  geom_segment(aes(x = 10, xend = 44.4918 , y = 0.5, yend = .5), 
               color ="#E69F00", size =.75, linetype = "dashed", show.legend = FALSE)+
  geom_segment(aes(x = 44.4918, xend = 44.4918 , y = 0, yend = .5), 
               color ="#E69F00", size =.75, linetype = "dashed", show.legend = FALSE) +
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(0,100,5))+
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0.25,1,.25)) +
  scale_color_manual(values = cbPalette) +
  scale_fill_manual(values = cbPalette) +
  labs(x = "Length (cm)",
       y = "Proprition Mature") +
  annotate(geom = "text", 
           x = 32,
           y = 0.8,
           label = expression(L[50]~50.5), color = "#999999") +
  annotate(geom = "text", 
           x = 32,
           y = 0.75,
           label = expression(L[50]~44.5), color = "#E69F00") +
  
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.785))

ggsave(paste0(fig_path, '/hagfish maturity.png'), width = 6, height = 5, units = "in", dpi = 400)

  

#ERROR AROUND L50#
lvalue <- function(coef,p) (log(p/(1-p))-coef[1])/coef[2]
fit<-glm(Mature~Length_cm,family=binomial(link=logit),data=hag_fmat)





#VALUE @ 50% MATURITY#
bs<-Boot(fit,B=1000)
(l50<-lvalue(coef(fit),.5))

#BOOTSTRAP CIs#
l50boots<-apply(bs,1,lvalue,p=0.5)
(l50ci<-quantile(l50boots,c(0.025,0.975)))


#MALE SAM#
Hag_Mmat<-HG_BIO %>% filter(Sex==1, Species_Code==202) %>% mutate(Length_cm = Length_mm / 10)

Hag_MSAM<-ggplot(Hag_Mmat,aes(Length_cm, Mature)) + 
  stat_smooth(method="glm", method.args=list(family="binomial")) +
  geom_point() + ylab('Ratio Mature')+xlab('Length (cm)') + ggtitle("E. deani male SAM") +
  geom_segment(aes(x=126.4345, y=.5, xend=918.3873, yend=.5))


#ERROR AROUND L50#
lvalue <- function(coef,p) (log(p/(1-p))-coef[1])/coef[2]
fit<-glm(Mature~Length_cm,family=binomial(link=logit),data=Hag_Mmat)

#VALUE @ 50% MATURITY#
bs<-bootCase(fit,B=1000)
(l50<-lvalue(coef(fit),.5))

#BOOTSTRAP CIs#
l50boots<-apply(bs,1,lvalue,p=0.5)
(l50ci<-quantile(l50boots,c(0.025,0.975)))

#FEMALE & MALE SAM ON SAME GRAPH#
Hag_mat<-HG_BIO %>% filter(Sex!=0, Species_Code==202) %>% mutate(Sex_Def = ifelse(Sex %in% 1, "Male", "Female")) %>%
  mutate(Length_cm = Length_mm /10)

Hag_SAM<-ggplot(Hag_mat, aes(Length_cm, Mature, colour=Sex_Def)) + 
  stat_smooth(aes(fill=Sex_Def), method="glm", method.args=list(family="binomial")) + 
  geom_point() + geom_hline(yintercept = 0.5, linetype = 2, size = 1) + ylab('Ratio Mature') + xlab('Length (cm)') + 
  scale_fill_manual(values= cbPalette) + scale_color_manual(values = cbPalette) +
  theme(legend.position = c(0.25, 0.75), legend.title = element_blank())
  
        
tiff(filename = "H:/Groundfish/Hagfish/seak_hagfish/figures/hagfish_maturity.tiff",
     width = 5, height = 5, units = "in", res = 600, compression = "lzw")

Hag_SAM

dev.off()

#Box Plots of Male and Female Reproductive Stages#
#Need to convert male stages to negative values so they can be on the same graph as females
hag_bio %>% 
  filter(sex != 0,
         !is.na(sex),
         maturity_stage != 0,
         !is.na(maturity_stage)) %>% 
  mutate(stage = ifelse(sex == 1 & maturity_stage == 1, -1,
                            ifelse(sex == 1 & maturity_stage == 2, -2,
                            ifelse(sex == 1 & maturity_stage == 3, -3,
                            ifelse(sex == 1 & maturity_stage == 4, -4,
                            ifelse(sex == 2 & maturity_stage == 1, 1,
                            ifelse(sex == 2 & maturity_stage == 2, 2,
                            ifelse(sex == 2 & maturity_stage == 3, 3,
                            ifelse(sex == 2 & maturity_stage == 4, 4,
                            ifelse(sex == 2 & maturity_stage == 5, 5, "NA")))))))))) %>%
  mutate(sex_name = ifelse(sex == 1, "Male",
                    ifelse(sex == 2, "Female", "Unknown"))) %>% mutate(length_cm = length_mm / 10) -> reprod

        
ggplot(reprod, 
       aes(stage, length_cm, fill = sex_name)) + 
  geom_boxplot() + 
  coord_flip() + 
  geom_jitter() +
  geom_vline(xintercept = 4.5,
             linetype = "dashed") +
  scale_fill_manual(values= cbPalette) + 
  scale_color_manual(values = cbPalette) +
  xlab("Maturity Stage") + 
  ylab("Length (cm)") + 
  scale_y_continuous(breaks = pretty(reprod$length_cm, n = 10)) +
  theme(legend.position = c(0.25, 0.75), legend.title = element_blank())

ggsave(paste0(fig_path, '/hagfish_maturity_stages.png'), width = 6, height = 7, units = "in", dpi = 400)
