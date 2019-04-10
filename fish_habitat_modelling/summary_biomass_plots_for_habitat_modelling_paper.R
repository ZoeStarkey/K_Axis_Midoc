usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/fish_habitat_modelling")
setwd(d)
library(tidyverse)

# notes on the list of stations included:
# - even though stations 2 and 12 had problems with depth strata, they can be included in these analyses because of the way strata are grouped 

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

# ggsave("biomass_boxplots.pdf", height=2.6, width=7)

# Figure 2
# Day-night vertical distributions (means and standard deviations) of biomass-dominant fish of the genera (a) Krefftichthys, (b) Protomyctophum, (c) Electrona, (d) Gymnoscopelus and (e) Bathylagus for the three depth strata during the summer period (January-February) of Kerguelen-Axis program
# version aggregated to major groups
	# read in and bind with environmental/station data 
	stn <- readRDS("../derived data/midoc_stations_envdata.rda")
	all_bm$DNC <- stn$DNC.visual[match(all_bm$midoc.stn, stn$midoc.stn)]
	all_bm$DNC2<- factor(gsub("\\wC","N", all_bm$DNC))
	all_bm <- all_bm %>% mutate(taxon=fct_rev(taxon))

	depth_boxplots<- function(x){
		ggplot(x, aes(y=log(bm_m3,10), x=fct_rev(factor(strat_m)), fill=DNC2)) + 
		scale_fill_manual(name="", values=c("grey90","grey40")) +
		geom_boxplot(position=position_dodge(width=.9)) +
		facet_grid(taxon~.) +
		coord_flip() +
		theme_light() +
		theme(strip.text = element_text(face = "italic")) +
		labs(x="depth (m)",y= expression("biomass (g."~m^-3*")")) +
		scale_y_continuous(breaks=c(-6:-2), labels=c(expression(10^-6), expression(10^-5),expression(10^-4),expression(10^-3),expression(10^-2))) + 
		theme(legend.position = "none")
	} 
	depth_boxplots(all_bm)
	ggsave("fig2_tax_grps_daynight_depth_boxplots.pdf", height=6,width=2.5)
  
# version disaggregated to individual species
	fbm <- readRDS("../derived data/codend_fish_biomass.rds")
	ce <- readRDS("../derived data/midoc_cod_ends_checked.rds")
	swept <- ce %>% filter(CE>1) %>% group_by(midoc.stn) %>% summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))

	# swept volume of epi
	swept.epi <- ce %>% filter(CE==6) %>% group_by(midoc.stn) %>%  summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))
	# note that a hard cut-off of 200m max depth limits this to only 2 stations
	# the mean for max depth of CE6 is 207.5, and max is 216 - ok to just retain all.

	# biomass epi
	bm.epi <- fbm %>% filter(fish.grp%in%c("Kreffichthys andersonii","Protomyctophum sp","Electrona sp.", "Gymnoscopelus sp.","unidientified/other myctophid","Bathylagiids"), cod.end == "6") %>%
				   group_by(midoc.stn, fish.grp) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.epi) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3) %>% arrange(midoc.stn)
	bm.epi[is.na(bm.epi$bm_m3),]$bm_m3<- 0
	bm.epi[is.na(bm.epi$n_m3),]$n_m3<- 0
	bm.epi$strat<- "0--200"


	# swept meso
	swept.meso <- ce %>% filter(CE%in%c(4:5)) %>% filter(max.dep<650) %>% group_by(midoc.stn) %>%  summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))
	# code end classifications for depth are OK, apart from MIDOC02, where CE4 went to 948m

	# biomass meso
	bm.meso <- fbm %>% filter(fish.grp%in%c("Kreffichthys andersonii","Protomyctophum sp","Electrona sp.", "Gymnoscopelus sp.","unidientified/other myctophid","Bathylagiids"), cod.end %in% c(as.character(2:5))) %>% filter((midoc.stn=="MIDOC02" & cod.end=="4")==F) %>%
				   group_by(midoc.stn, fish.grp) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.meso) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
	bm.meso[is.na(bm.meso$bm_m3),]$bm_m3<- 0
	bm.meso[is.na(bm.meso$n_m3),]$n_m3<- 0
	bm.meso$strat<- "200--600"

	# swept ubathy
	swept.ubathy <- ce %>% filter(CE%in%c(2:3)) %>% group_by(midoc.stn) %>%  summarise(swept_stn_m3 = sum(swept_m3, na.rm=T))

	# biomass ubathy
	bm.ubathy <- fbm %>% filter(fish.grp%in%c("Kreffichthys andersonii","Protomyctophum sp","Electrona sp.", "Gymnoscopelus sp.","unidientified/other myctophid","Bathylagiids"), cod.end %in% c(as.character(2:3))) %>% filter((midoc.stn=="MIDOC02" & cod.end=="4")==F) %>%
				   group_by(midoc.stn, fish.grp) %>% mutate(n.individuals=as.numeric(n.individuals)*include.in.total) %>% summarise(bm.g=sum(wt.g, na.rm=T), n=sum(as.numeric(n.individuals), na.rm=T)) %>% right_join(swept.ubathy) %>% mutate(n_m3=n/swept_stn_m3, bm_m3=bm.g/swept_stn_m3)
	bm.ubathy[is.na(bm.ubathy$bm_m3),]$bm_m3<- 0
	bm.ubathy[is.na(bm.ubathy$n_m3),]$n_m3<- 0
	bm.ubathy$strat<- "600--1000"


	# join together epi, meso, ubathy then add DN classification and rename for plotting
	txbm <- bind_rows(bm.epi, bm.meso, bm.ubathy) %>% rename(taxon=fish.grp, strat_m=strat) %>% filter(!is.na(taxon))

	txbm$DNC <- stn$DNC.visual[match(txbm$midoc.stn, stn$midoc.stn)]
	txbm$DNC2<- factor(gsub("\\wC","N", txbm$DNC))
	txbm$taxon <- gsub("Protomyctophum sp", "Protomyctophum sp.", txbm$taxon)
	txbm$taxon <- gsub("andersonii", "anderssoni", txbm$taxon)

	txbm$taxon<- fct_relevel(txbm$taxon, "Protomyctophum sp.","Kreffichthys anderssoni","Electrona sp.","Gymnoscopelus sp.", "Bathylagiids")

	

	depth_boxplots(txbm)
	ggsave("suppl_fig_tax_ungrouped_daynight_depth_boxplots.pdf", height=7.5,width=2.5)

# Figure 4
# Fig. 4. Percentage occurrence of midwater fish taxa in all net hauls (gray line) and in Myctophid and Bathylagid samples kept for GAMs (blackline) for the southern Kerguelen Plateau in the Indian sector of the Southern Ocean.
	fpa <- readRDS("../derived data/midoc_fish_presence_absence.rds")
	fpa <- fpa[fpa$midoc.stn%in%unique(txbm$midoc.stn),]
	fpa$fgroup <- gsub("Kreffichthys andersonii", "Krefftichthys anderssoni",fpa$fgroup)
	fpa$fgroup <- gsub("Other myctophid", "unidientified/other myctophids",fpa$fgroup)
	fpa$fgroup[fpa$fgroup%in%c("Melanonidae", "Stomiidae","Gempylidae", "Melamphidae")] <- "mixed/other fishes"
	fpa<- fpa %>% group_by(midoc.stn, fgroup) %>% distinct()

	st.prop<- fpa %>% group_by(fgroup) %>% mutate(nst=sum(PA)) %>% select(fgroup, nst) %>% distinct() %>% arrange(-nst) %>% mutate(pst=round(nst/36*100,0))

	st.prop$fgroup<- gsub("Electrona" ,"Electrona sp.",st.prop$fgroup)
	st.prop$fgroup<- gsub("Gymnoscopelus","Gymnoscopelus sp.",st.prop$fgroup)
	st.prop$fgroup<- gsub("Gonostomatidae","Cyclothone sp.",st.prop$fgroup)
	st.prop$fgroup<- gsub("Protomyctophum","Protomyctophum sp.",st.prop$fgroup)
	

# instead potentially a plot for proportions
	fbm <- readRDS("../derived data/codend_fish_biomass.rds")
	fbm$fish.grp <- gsub("cyclothone", "Cyclothone",fbm$fish.grp)
	fbm$fish.grp <- gsub("Kreffichthys", "Krefftichthys",fbm$fish.grp)
	fbm$fish.grp <- gsub("Bathylagiids", "Bathylagidae",fbm$fish.grp)
	fbm <- fbm[!is.na(fbm$fish.grp),]
# total biomass and proportional biomass (tbm and pbm) columns in this df are across all taxa. We want totals for fish, then proportional contributions
# this will be the proportion of the total catch, across cod-ends 2:6 (ascending cod ends)
# proportions for individual cod-ends could be generated by modifying the group_by groupings

f.bmp <- fbm %>% filter(cod.end%in%as.character(c(2:6))) %>% select(-TL.mm, -FL.mm,-SL.mm, -photo, -photo.label, -notes, -to.check) %>%
		  group_by(midoc.stn) %>%
		  mutate(tbm = sum(wt.g, na.rm=T)) %>% ungroup() %>%
		  group_by(midoc.stn, fish.grp) %>%
		  mutate(bm.grp = sum(wt.g, na.rm=T)) %>% ungroup() %>%
		  mutate(pbm.grp = bm.grp/tbm) %>%
		  select(midoc.stn,date, fish.grp, tbm, bm.grp, pbm.grp) %>%
		  distinct()  

# Fix this: Melanonidae, Stomiidae,Gempylidae, Melamphidae <- other fish

f.bmp$fish.grp <- gsub("Protomyctophum sp", "Protomyctophum sp.", f.bmp$fish.grp)
f.bmp$fish.grp <- gsub("Cyclothone", "Cyclothone sp.", f.bmp$fish.grp)
f.bmp$fish.grp <- gsub("Macrurids", "Macrouridae", f.bmp$fish.grp)
f.bmp$fish.grp <- gsub("andersonii", "anderssoni", f.bmp$fish.grp)
f.bmp$fish.grp <- gsub("mixed/other fish", "mixed/other fishes", f.bmp$fish.grp)

f.bmp$fish.grp<- as.factor(f.bmp$fish.grp)

f.bmp$fish.grp<- fct_relevel(f.bmp$fish.grp, "mixed/other fishes", "Cyclothone sp.","unidientified/other myctophids", "Macrouridae", "Paralepididae", "Protomyctophum sp.","Krefftichthys anderssoni","Electrona sp.","Gymnoscopelus sp.", "Bathylagidae")

st.prop<- st.prop %>% rename("fish.grp"=fgroup)


mylabels <- c("Bathylagidae", 
			  expression(italic("Gymnoscopelus sp.")),
			  expression(italic("Electrona sp.")),
			  expression(italic("Krefftichthys anderssoni")),
			  expression(italic("Protomyctophum sp.")),
			  "Paralepididae",
			  "Macrouridae",
			  "unidientified/other myctophids",
			  expression(italic("Cyclothone sp.")),
			  "mixed/other fishes"
             )

ggplot(f.bmp, aes(x=fish.grp, y=pbm.grp)) +
	coord_flip() + 
	geom_boxplot() + 
	geom_text(data=st.prop, aes(x=fish.grp, y=1, label=(pst)), col="dark grey") +
	theme_light() +
	scale_x_discrete(labels = rev(mylabels)) +
	annotate(geom="text", x=10.4, y=.925, label="% of sites", col="dark grey") +
	labs(y="proportion of total fish biomass",x="") 

ggsave("fig4_taxa_propotional_biomass_contributions.pdf", height=4, width=5)