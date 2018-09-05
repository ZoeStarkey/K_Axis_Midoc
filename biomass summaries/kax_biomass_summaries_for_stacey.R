# kax_biomass_summaries_for_stacey.R
# 5 Sep 2018
# script to generate summaries of biomass of major taxa caught in MIDOC trawls on K-axis voyage
# for use in development of Kerguelen Plateau/Prydz Bay Ecopath model


library(tidyverse)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/")
setwd(d)

bm <-  readRDS("./derived data/codend_taxa_biomass.rds")

# problem stations
naf <- c("MIDOC10","MIDOC13")

bm <- bm %>% filter(cod.end %in% as.character(c(2:6)), (midoc.stn%in%naf)==F)

# total volume swept for each midoc
md.swept <- bm %>% group_by(midoc.stn, cod.end) %>% distinct(swept_m3) %>% ungroup() %>% group_by(midoc.stn) %>% summarise(swept_m3=sum(swept_m3, na.rm=T))

bm.stn <- bm %>% select(-fish.grp) %>%
		  group_by(midoc.stn, tax.grp) %>%
		  summarise(bm_sum = sum(bm,na.rm=T)) %>%
		  inner_join(md.swept)	%>%
		  mutate(bm_m3 = bm_sum/swept_m3) %>% filter(tax.grp%in%"NA"==F)

## TODO: 
		  # before sending add
		  # lat and lon
		  # more detailed fish groupings

saveRDS(bm.stn, "midoc_station_biomass_sums.RDS")
write_csv(bm.stn, "midoc_station_biomass_sums.csv")