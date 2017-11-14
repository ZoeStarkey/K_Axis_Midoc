# first post-voyage maps
# for inclusion in presentation at CLS
# 9-Jun-2016

## "Max chl-a since Nov 2015, ice contours for 15 Jan (solid) and 15 Nov (dashed), and Orsifronts from north "saf", "pf", "saccf", "sbdy""
## '/home/shared/data/chlmisc/chl_max_kerg2016/v3_overall.png'

library(raster)
library(orsifronts)
library(rworldxtra)
data(countriesHigh)
library(dplyr)
library(tidyr)

# define plot extent
ext <- extent(60, 100, -70, -50)
ext2 <- extent(62, 98, -68, -52)

# # read in chlorophyll
# chl <- brick("/home/shared/data/chlmisc/chl_max_kerg2016/chlmax_2002_2015.grd")
# ## subset to 15/16: last one is 2015-2016
# chl <- chl[[nlayers(chl)]]
# chl <- crop(chl, ext) # crop to extent
# pal <- chlPal(palette = TRUE) # define pallette

# # read ice data
# ex <- projectExtent(chl, projection(readice()))
# icejan <- readice("2016-01-15", xylim = ex)
# icenov <- readice("2015-11-15", xylim = ex)

# read in MIDOC data
# generic function for reading biomass csvs, and coding DNC plot colour

crep <- readr::read_csv(("./source data/midoc_crepuscular.csv"))

bm.tax <- readRDS("./derived data/codend_taxa_biomass.rds")
bm.wide <- bm.tax %>% spread(key=tax.grp, value=bm, fill=0)

km <- readRDS("./source data/midoc_stations_locations_times.rds")
# bm.wide <- bm.wide %>% inner_join(km)
# bm.tax <- bm.tax %>% inner_join(km)

ktr <- readr::read_csv("./source data/v3_201516030_waypoints_dec.csv")
colnames(ktr) <- c("wp","lat","lon","wp.grp")
ktr <- ktr[-1,]
coordinates(ktr) <- ~lon+lat

naf <- c("TRIAL","MIDOC02","MIDOC08","MIDOC10","MIDOC12","MIDOC13","MIDOC33")

bm.fish  <- bm.tax %>% filter(tax.grp=="fish", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(km) %>% inner_join(crep)
bm.krill <- bm.tax %>% filter(tax.grp=="krill", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(km) %>% inner_join(crep)
bm.ceph <- bm.tax %>% filter(tax.grp=="cephalopods", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(km) %>% inner_join(crep)
bm.jelly <- bm.tax %>% filter(tax.grp=="cnidarians", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(km) %>% inner_join(crep)
bm.salps <- bm.tax %>% filter(tax.grp=="salps", midoc.stn%in%naf==F) %>% ungroup() %>% group_by(midoc.stn) %>% summarize(bm.tot = sum(bm, na.rm=T)) %>% inner_join(km) %>% inner_join(crep)


# function to scale point size consistently for bubble plots
key.values<- c(0.25, 0.5, 1,2,4,8,16) # breaks for scale
scale.bubbles<- function(bmdat, maxsize = 4, key = key.values){	
  abm = bmdat$bm.tot
  # scale to square root
  abm = sqrt(abm)
  cex = as.vector(maxsize * abm/max(abm, sqrt(key)))    
  bmdat$cex <- cex
  bmdat
}

# plotting
plotbm <- function(pdf.name, bm.dat, the.main){

  bm.dat$col<- ifelse(bm.dat$DNC.visual=="N", "dark blue", 
                     ifelse(bm.dat$DNC.visual=="D", "yellow",
                            ifelse(bm.dat$DNC.visual=="MC", "orange", "violet")))
  coordinates(bm.dat) <- ~LONGITUDE+LATITUDE

  pdf(height=6, width=8, pdf.name)
  par(las=1)
  plot(ext2, ylab="", xlab="", type="n", main=the.main)
  #plot(chl, col = pal$cols, breaks = pal$breaks, legend = FALSE, add=T)
  #plot(crop(orsifronts,ext), add = TRUE, col="#053061", lty=1, lwd=1)
  #plot(spTransform(rasterToContour(icejan[[1]], lev = 15), projection(countriesHigh)), add = TRUE, lty=1, col="#00FFFF", lwd=1.5)
  #plot(spTransform(rasterToContour(icenov[[1]], lev = 15), projection(countriesHigh)), add = TRUE, lty = 1, lwd=0.5)
  plot(countriesHigh, add=T, col="gray90", border="gray70")
  
  lines(coordinates(ktr), col="gray40", lwd=2)
  
  
  # old plotting, using automatic scaling of bubbles with bubble()
  # the.cex<- bubble(bm.dat, zcol="bm.grand.tot", plot=F)$panel.args.common$cex
  # points(bm.dat, cex=the.cex, bg=bm.dat$col, pch=21, col="gray")
  
  # new plotting to force consistent scale for bubbles
  bm.dat<- scale.bubbles(bm.dat)
  points(bm.dat, cex=bm.dat$cex, bg=bm.dat$col, pch=21, col="gray")
  # text(bm.dat, labels=as.character(bm.dat$midoc.stn), cex=0.3)
  
  # make a key 
  keydat<- data.frame(value=key.values)
  keydat$cex = as.vector(sqrt(16)*sqrt(keydat$value)/max(sqrt(keydat$value)))
  keydat$lon <- 65 
  keydat$lat <- -55 -(seq(0, .5*6, .5))
  points(rev(keydat$lon), rev(keydat$lat), pch=21, bg="gray", cex=rev(keydat$cex))
  lab<- as.character(keydat$value)
  lab[length(lab)]<- paste0(">= ",lab[length(lab)] )
  text(keydat$lon+.5, keydat$lat, lab, cex=0.5, pos=4)
  text(keydat$lon[1], keydat$lat[1]+1, "biomass (kg)", cex=0.6)
  dev.off()
}

# bubble plots for
  # total fish
    plotbm("fish.pdf", bm.fish, "fish")
  # total squid
    plotbm("squid.pdf", bm.ceph, "squid")
  # total krill
    plotbm("krill.pdf", bm.krill, "krill")
  # total salps 
    plotbm("salps.pdf", bm.salps, "salps")    