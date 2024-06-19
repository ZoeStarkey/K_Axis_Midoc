# KPS_EA_catch_bubbles.R
# bubble plots of catch, for inclusion in extended abstract for Kerguelen Plateau symposium
install.packages("ggpubr")

library(tidyverse)
library(ggpubr)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/derived data")
setwd(d)

mds   <- readRDS("midoc_stations_checked.rds")
md.ce <- readRDS("midoc_cod_ends_checked.rds")
ktr <- readRDS("nav_reduced.rds")
ced <- tibble(cod.end=as.character(c(2:6)), depth=seq(900,100,by=-200))
mde  <- readRDS("midoc_stations_envdata.rda")

md.crep<- read_csv("../source data/midoc_crepuscular.csv")

bm.tax <- readRDS("codend_taxa_biomass.rds")
bm.wide <- bm.tax %>% spread(key=tax.grp, value=bm, fill=0)

naf <- c("TRIAL","MIDOC02","MIDOC08","MIDOC10","MIDOC12","MIDOC13","MIDOC33")

bm.fish  <- bm.tax %>% filter(tax.grp=="fish", midoc.stn%in%naf==F, as.numeric(cod.end)>1) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T), bm_g_m3_total = sum(bm_g_m3, na.rm=T)) %>% inner_join(md.crep) %>% inner_join(mds)

bm.krill <- bm.tax %>% filter(tax.grp=="krill", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(mds) %>% inner_join(md.crep)
bm.ceph <- bm.tax %>% filter(tax.grp=="cephalopods", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(mds) %>% inner_join(md.crep)
bm.jelly <- bm.tax %>% filter(tax.grp=="cnidarians", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(mds) %>% inner_join(md.crep)
bm.salps <- bm.tax %>% filter(tax.grp=="salps", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(mds) %>% inner_join(md.crep)


plotbm2<- function(bm.dat, file, title){
ggplot(ktr, aes(x=LONGITUDE, y=LATITUDE), main=title) + 
   geom_path(col="grey") +
   geom_point(data=bm.dat, aes(y=lat_start, x=lon_start, colour=as.factor(DNC.visual), size=bm_g_m3_total))+
   geom_text(data=bm.dat, aes(y=lat_start, x=lon_start, label=substr(midoc.stn, 6,7), vjust=-.4), cex=3)+
   theme_bw() +
   scale_size(name=expression(paste("biomass (",g/m^3,")"))) + scale_colour_manual(name="", values=c("yellow","violet","dark blue","orange"), label=c("day","sunrise","night","sunset"))
   ggsave(file, width=4.5, height=3)
}

# plotbm2(bm.fish, "../KPS_symposium_extended_abstract/fish.pdf", "fish")
# plotbm2(bm.ceph, "ceph.pdf", "cephalopods")
# plotbm2(bm.krill, "krill.pdf", "krill")
# plotbm2(bm.salps, "salps.pdf", "salps")
# plotbm2(bm.jelly, "jelly.pdf","cnidarians")

# this was incorrect; need to do density of sums, not sums of densities
# bm.tax2<- bm.tax %>% filter(tax.grp%in%c("fish", "cephalopods","krill","salps","cnidarians"), midoc.stn%in%naf==F, cod.end %in% as.character(c(2:6))) %>% 
#  ungroup() %>% group_by(midoc.stn, tax.grp) %>% summarize(bm_g_m3_total = sum(bm_g_m3, na.rm=T)) %>% inner_join(mds) %>% inner_join(md.crep) 

# sanity check: is sum of density different to density of sum
md.swept <-  bm.tax %>% filter(midoc.stn%in%naf==F, cod.end %in% as.character(c(2:6))) %>%  select(midoc.stn, cod.end, swept_m3) %>% distinct() %>% group_by(midoc.stn) %>% summarise(swept_total = sum(swept_m3))

bm.tax2 <-  bm.tax %>% filter(tax.grp%in%c("fish", "cephalopods","krill","salps","cnidarians"), midoc.stn%in%naf==F, cod.end %in% as.character(c(2:6))) %>%
 ungroup() %>% group_by(midoc.stn, tax.grp) %>% summarize(bm_total = sum(bm, na.rm=T)) %>% inner_join(mds) %>% inner_join(md.crep) %>% inner_join(md.swept) %>% mutate(bm_dens = bm_total/swept_total)

#ZS: frontal zone 
tmp <- read_rds("../derived data/midoc_stations_envdata.rda")
bm.tax2 <-  bm.tax %>% filter(tax.grp%in%c("fish", "cephalopods","krill","salps","cnidarians"), midoc.stn%in%naf==F, cod.end %in% as.character(c(2:6))) %>%
  ungroup() %>% group_by(midoc.stn, tax.grp) %>% summarize(bm_total = sum(bm, na.rm=T)) %>% inner_join(mds) %>% inner_join(md.crep) %>% inner_join(md.swept) %>% inner_join(tmp) %>% mutate(bm_dens = bm_total/swept_total)
bm.tax2$zone.col <- NA
bm.tax2$zone.col <- ifelse(bm.tax2$zone == "Southern", "blue", 
                      ifelse(bm.tax2$zone == "Subpolar", "red",
                             ifelse(bm.tax2$zone == "Antarctic ACC", "yellow", 
                                    ifelse(bm.tax2$zone == "Subpolar - ASF", "green", 
                                           ifelse(bm.tax2$zone == "Antarctic KP", "darkblue", "orange")))))
pdir <- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/KPS_symposium_extended_abstract")

ggplot(ktr, aes(x=LONGITUDE, y=LATITUDE)) + geom_path(col="grey") +
  geom_point(data=bm.tax2, aes(y=lat_start, x=lon_start, color=zone.col, size=bm_dens), alpha=.7) +
  # geom_text(data=bm.tax2, aes(x=lon_start-1, y=lat_start, label=substr(midoc.stn,6,7),hjust=-0.5),cex=3, colour="dark grey")+
  scale_size(name=expression(paste("biomass (",g/m^3,")"))) + scale_colour_manual(name="", values=c("blue","red","yellow","green","darkblue", "orange"), label=c("Southern","Subpolar","Antarctic ACC","Subpolar ASF", "Antarctic KP", "Antarctic FTJ")) +
  facet_wrap(~tax.grp) +
  theme_bw() + xlab("longitude") + ylab("latitude")+
  ggsave(paste0(pdir,"/K4S_catch_biomass_bubbles_FZ.pdf"), height=4, width=7)





# this is the plot for the KPSII extended abstract
ggplot(ktr, aes(x=LONGITUDE, y=LATITUDE)) + geom_path(col="grey") +
  geom_point(data=bm.tax2, aes(y=lat_start, x=lon_start, color=DNC.visual, size=bm_dens), alpha=.7) +
  # geom_text(data=bm.tax2, aes(x=lon_start-1, y=lat_start, label=substr(midoc.stn,6,7),hjust=-0.5),cex=3, colour="dark grey")+
  scale_size(name=expression(paste("biomass (",g/m^3,")"))) + scale_colour_manual(name="", values=c("yellow","violet","dark blue","orange"), label=c("day","sunrise","night","sunset")) +
  facet_wrap(~tax.grp) +
  theme_bw() + xlab("longitude") + ylab("latitude") +
  ggsave(paste0(pdir,"/KPS_EA_catch_biomass_bubbles.pdf"), height=4, width=7)


# proportions
bm.tax %>% 
  filter(tax.grp%in%c("fish", "cephalopods","krill","salps","cnidarians","mixed krill and salps","mixed/other gelatinous"), midoc.stn%in%naf==F, cod.end %in% as.character(c(2:6))) %>%
  mutate(midoc.stn = substr(midoc.stn, 6,8)) %>% inner_join(ced) %>%
ggplot(aes(weight=pbm, x=reorder(depth, desc(depth)),fill=tax.grp), xlab = "cod end", ylab = "proportion of total biomass") +geom_bar() + facet_wrap(~midoc.stn, ncol = 4) +theme_bw() +coord_flip()  + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill=guide_legend(title="group")) +
  xlab("cod end") + ylab("proportion of total catch (by weight)") +
  ggsave(paste0(pdir,"/KPS_EA_catch_weight_proportions.pdf"), height=5, width=8)


# plots of average proportion biomass (box)

cols<- c("yellow","violet","dark blue","orange")
dcols <- c("goldenrod","violetred","navyblue","darkorange2")

pd <- bm.tax %>% 
   filter(tax.grp%in%c("fish", "cephalopods","krill","salps","cnidarians"), midoc.stn%in%naf==F, cod.end %in% as.character(c(2:6))) %>% inner_join(ced) %>% inner_join(select(md.crep, midoc.stn, DNC.visual)) %>% group_by(tax.grp, cod.end, DNC.visual) 

ce2p <- pd %>% filter(cod.end==2) %>%  ggplot(aes(x=tax.grp, y=pbm, fill=DNC.visual, col=DNC.visual)) + geom_violin() + scale_fill_manual(name="", values=cols) +scale_colour_manual(name="", values=dcols) + theme_bw() +xlab("") + ylab("") + ggtitle("900 m")+ theme(plot.title = element_text(size=10))

ce3p <- pd %>% filter(cod.end==3) %>%  ggplot(aes(x=tax.grp, y=pbm, fill=DNC.visual, col=DNC.visual)) + geom_boxplot() + scale_fill_manual(name="", values=cols) +scale_colour_manual(name="", values=dcols) + theme_bw() +xlab("") + ylab("") + ggtitle("700 m")+ theme(plot.title = element_text(size=10))

ce4p <- pd %>% filter(cod.end==4) %>%  ggplot(aes(x=tax.grp, y=pbm, fill=DNC.visual, col=DNC.visual)) + geom_boxplot() + scale_fill_manual(name="", values=cols) +scale_colour_manual(name="", values=dcols) + theme_bw() +xlab("") + ylab("proportion of biomass") + ggtitle("500 m")+ theme(plot.title = element_text(size=10))

ce5p <- pd %>% filter(cod.end==5) %>%  ggplot(aes(x=tax.grp, y=pbm, fill=DNC.visual, col=DNC.visual)) + geom_boxplot() + scale_fill_manual(name="", values=cols) +scale_colour_manual(name="", values=dcols) + theme_bw() +xlab("") + ylab("") + ggtitle("300 m") + theme(plot.title = element_text(size=10))

ce6p <- pd %>% filter(cod.end==6) %>%  ggplot(aes(x=tax.grp, y=pbm, fill=DNC.visual, col=DNC.visual)) + geom_boxplot() + scale_fill_manual(name="", values=cols) +scale_colour_manual(name="", values=dcols) + theme_bw() +xlab("") + ylab("") + ggtitle("100 m") +  theme(plot.title = element_text(size=10))   

ggarrange(ce6p+rremove("x.text"), ce5p+rremove("x.text"), ce4p+rremove("x.text"), ce3p+rremove("x.text"), ce2p, common.legend=T, ncol=1, nrow=5)

ggsave(paste0(pdir,"/KPS_EA_catch_proportions_dnc_depth.pdf"), height=8.25, width=4.75)


# could also do this wigh ggboxplot...
pd <- bm.tax %>% 
  filter(tax.grp%in%c("fish", "cephalopods","krill","salps","cnidarians"), midoc.stn%in%naf==F, cod.end %in% as.character(c(2:6))) %>% inner_join(ced) %>% inner_join(select(md.crep, midoc.stn, DNC.visual)) %>%
    group_by(tax.grp, cod.end, DNC.visual) %>% filter(cod.end==2) %>%  ggboxplot(x="tax.grp", y="pbm", fill="DNC.visual")
plot(pd)



# plot of biomass per cod-end and TOD
bm.tax %>% filter(tax.grp=="fish", midoc.stn%in%naf==F, cod.end%in%as.character(c(2:6))) %>%
  inner_join(ced) %>% inner_join(select(mde, midoc.stn, DNC.visual)) %>% 
  group_by(midoc.stn, cod.end, DNC.visual) 

