setwd("/Users/dougt/GitHub/K_axis_midoc/fish_habitat_modelling")
library(tidyverse)

bathy_grp_bm_epi<- readRDS("bathy_grp_bm_epi.RDS")
bathy_grp_bm_meso<- readRDS("bathy_grp_bm_meso.RDS")
bathy_grp_bm_ubathy<- readRDS("bathy_grp_bm_ubathy.RDS")
bathy_grp_bm_epi$strat<- "epi"
bathy_grp_bm_meso$strat<-"meso"
bathy_grp_bm_ubathy$strat<-"ubathy"
bathy_bm<- bind_rows(bathy_grp_bm_epi,bathy_grp_bm_meso,bathy_grp_bm_ubathy)
bathy_bm$taxon <- "Bathylagus"

ele_gymno_grp_bm_epi<- readRDS("ele_gymno_grp_bm_epi.RDS")
ele_gymno_grp_bm_meso<- readRDS("ele_gymno_grp_bm_meso.RDS")
ele_gymno_grp_bm_ubathy<- readRDS("ele_gymno_grp_bm_ubathy.RDS")
ele_gymno_grp_bm_epi$strat<- "epi"
ele_gymno_grp_bm_meso$strat<- "meso"
ele_gymno_grp_bm_ubathy$strat<- "ubathy"
eg_bm<- bind_rows(ele_gymno_grp_bm_epi,ele_gymno_grp_bm_meso,ele_gymno_grp_bm_ubathy)
eg_bm$taxon <- "Electrona and Gymnoscopelus"

kreff_grp_bm_epi<- readRDS("kreff_grp_bm_epi.RDS")
kreff_grp_bm_meso<- readRDS("kreff_grp_bm_meso.RDS")
kreff_grp_bm_ubathy<- readRDS("kreff_grp_bm_ubathy.RDS")
kreff_grp_bm_epi$strat<- "epi"
kreff_grp_bm_meso$strat<- "meso"
kreff_grp_bm_ubathy$strat<- "ubathy"
kreff_bm<- bind_rows(kreff_grp_bm_epi,kreff_grp_bm_meso,kreff_grp_bm_ubathy)
kreff_bm$taxon <- "Krefftichthys"

all_bm <- bind_rows(kreff_bm, eg_bm, bathy_bm)

all_bm$strat_m<-  NA_character_
all_bm$strat_m[all_bm$strat=="epi"]<- "0--200"
all_bm$strat_m[all_bm$strat=="meso"]<- "200--600"
all_bm$strat_m[all_bm$strat=="ubathy"]<- "600--1000"

ggplot(all_bm, aes(y=bm_m3, strat_m)) + 
		geom_boxplot() +
		facet_grid(.~taxon) +
		scale_y_log10() +
		theme_light() +
		theme(strip.text = element_text(face = "italic")) +
		labs(x="depth (m)",y= expression("biomass (g."~m^-3*")"))

ggsave("biomass_boxplots.pdf", height=2.6, width=7)

# Figure 2
# Day-night vertical distributions (means and standard deviations) of biomass-dominant fish of the genera (a) Krefftichthys, (b) Protomyctophum, (c) Electrona, (d) Gymnoscopelus and (e) Bathylagus for the three depth strata during the summer period (January-February) of Kerguelen-Axis program


# Figure 4
# Fig. 4. Percentage occurrence of midwater fish taxa in all net hauls (gray line) and in Myctophid and Bathylagid samples kept for GAMs (blackline) for the southern Kerguelen Plateau in the Indian sector of the Southern Ocean.
