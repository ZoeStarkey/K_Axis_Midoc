# first post-voyage maps
# for inclusion in presentation at CLS
# 9-Jun-2016

## "Max chl-a since Nov 2015, ice contours for 15 Jan (solid) and 15 Nov (dashed), and Orsifronts from north "saf", "pf", "saccf", "sbdy""
## '/home/shared/data/chlmisc/chl_max_kerg2016/v3_overall.png'

library(dplyr)
library(tidyr)
library(ggplot2)


crep <- readr::read_csv(("./source data/midoc_crepuscular.csv"))

bm.tax <- readRDS("./derived data/codend_taxa_biomass.rds")
bm.wide <- bm.tax %>% spread(key=tax.grp, value=bm, fill=0)

km <- readRDS("./source data/midoc_stations_locations_times.rds")
# bm.wide <- bm.wide %>% inner_join(km)
# bm.tax <- bm.tax %>% inner_join(km)

ktr <- readr::read_csv("./source data/v3_201516030_waypoints_dec.csv")
ktr <- ktr[-1,]
colnames(ktr) <- c("wp","lat","lon","wp.grp")

naf <- c("TRIAL","MIDOC02","MIDOC08","MIDOC10","MIDOC12","MIDOC13","MIDOC33")

bm.fish  <- bm.tax %>% filter(tax.grp=="fish", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(km) %>% inner_join(crep)
bm.krill <- bm.tax %>% filter(tax.grp=="krill", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(km) %>% inner_join(crep)
bm.ceph <- bm.tax %>% filter(tax.grp=="cephalopods", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(km) %>% inner_join(crep)
bm.jelly <- bm.tax %>% filter(tax.grp=="cnidarians", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(km) %>% inner_join(crep)
bm.salps <- bm.tax %>% filter(tax.grp=="salps", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(km) %>% inner_join(crep)


plotbm2<- function(bm.dat, file, title){
   ggplot(ktr, aes(x=lon, y=lat), main=title) + 
   geom_path(col="grey") +
   geom_point(data=bm.dat, aes(y=LATITUDE, x=LONGITUDE, color=DNC.visual, size=bm.tot))+
   theme_bw() +
   guides(fill=guide_legend(title="biomass (g)"))
   ggsave(file, width=6, height=4)
}

plotbm2(bm.fish, "fish.pdf", "fish")
plotbm2(bm.ceph, "ceph.pdf", "cephalopods")
plotbm2(bm.krill, "krill.pdf", "krill")
plotbm2(bm.salps, "salps.pdf", "salps")
plotbm2(bm.jelly, "jelly.pdf","cnidarians")


bm.tax2<- bm.tax %>% filter(tax.grp%in%c("fish", "cephalopods","krill","salps","cnidarians"), midoc.stn%in%naf==F) %>%
 ungroup() %>% group_by(midoc.stn, tax.grp) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(km) %>% inner_join(crep) 

p<- ggplot(bm.tax2, aes(y=LATITUDE, x=LONGITUDE, color=DNC.visual, size=bm.tot)) +
  geom_point() +
  geom_text(data=bm.tax2, aes(x=LONGITUDE-1, y=LATITUDE, label=substr(midoc.stn,6,7)), cex=4, color="dark grey")+
  facet_wrap(~tax.grp) +
  theme_bw()