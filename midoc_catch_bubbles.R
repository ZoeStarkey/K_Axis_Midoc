# first post-voyage maps
# for inclusion in presentation at CLS
# 9-Jun-2016

## "Max chl-a since Nov 2015, ice contours for 15 Jan (solid) and 15 Nov (dashed), and Orsifronts from north "saf", "pf", "saccf", "sbdy""
## '/home/shared/data/chlmisc/chl_max_kerg2016/v3_overall.png'

library(raster)
library(palr)
library(orsifronts)
library(rworldxtra)
data(countriesHigh)
library(raadtools)
library(RColorBrewer)


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
read.bm<- function(bm.rds){ 
  bmdat<- readRDS(bm.rds)
  bmdat$col<- ifelse(bmdat$DNC=="N", "dark blue", 
                     ifelse(bmdat$DNC=="D", "yellow",
                            ifelse(bmdat$DNC=="MC", "orange", "violet")))
  coordinates(bmdat) <- ~lon+lat
  bmdat
}

# total biomass for fish
stn.bm<- read.bm("f.tots.csv")

# combined fish biomass split by codend
CE.bm<- read.bm("f.CE.tots.csv")

# biomass of higher taxonomic groups - total
grps.bm <- read.bm("stn.tax.bm.tots.csv")

# total fish biomass split by group - total
f.grps.bm <- read.bm("stn.bm.tots.csv")

# voyage data
voyage <- readRDS("/home/shared/data/chlmisc/chl_max_kerg2016/v3points.rds")

# function to scale point size consistently for bubble plots
key.values<- c(0.25, 0.5, 1,2,4,8,16) # breaks for scale
scale.bubbles<- function(bmdat, maxsize = 4, key = key.values){	
  abm = bmdat$bm.grand.tot
  # scale to square root
  abm = sqrt(abm)
  cex = as.vector(maxsize * abm/max(abm, sqrt(key)))    
  bmdat$cex <- cex
  bmdat
}

# plotting
plotbm <- function(pdf.name, bm.dat, the.main){

  pdf(height=6, width=8, pdf.name)
  par(las=1)
  plot(ext2, ylab="", xlab="", type="n", main=the.main)
  plot(chl, col = pal$cols, breaks = pal$breaks, legend = FALSE, add=T)
  plot(crop(orsifronts,ext), add = TRUE, col="#053061", lty=1, lwd=1)
  plot(spTransform(rasterToContour(icejan[[1]], lev = 15), projection(countriesHigh)), add = TRUE, lty=1, col="#00FFFF", lwd=1.5)
  plot(spTransform(rasterToContour(icenov[[1]], lev = 15), projection(countriesHigh)), add = TRUE, lty = 1, lwd=0.5)
  plot(countriesHigh, add=T, col="gray90", border="gray70")
  
  lines(coordinates(voyage), col="gray40", lwd=2)
  
  
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

plotbm(pdf.name="k_axis_fish_biomass_total.pdf", bm.dat=stn.bm, the.main="all fish")
plotbm(pdf.name="k_axis_fish_biomass_CE2.pdf", bm.dat=CE.bm[CE.bm$cod.end==2,], the.main="800-1000m")
plotbm(pdf.name="k_axis_fish_biomass_CE3.pdf", bm.dat=CE.bm[CE.bm$cod.end==3,], the.main="600-800m")
plotbm(pdf.name="k_axis_fish_biomass_CE4.pdf", bm.dat=CE.bm[CE.bm$cod.end==4,], the.main="400-600m")
plotbm(pdf.name="k_axis_fish_biomass_CE5.pdf", bm.dat=CE.bm[CE.bm$cod.end==5,], the.main="200-400m")
plotbm(pdf.name="k_axis_fish_biomass_CE6.pdf", bm.dat=CE.bm[CE.bm$cod.end==6,], the.main="0-200m")

# fish tax groups - subset from f.grps.bm
# Bathylagus sp.
plotbm(pdf.name="k_axis_bathylagus_biomass_total.pdf", bm.dat=subset(f.grps.bm, f.grps.bm$fish.grp=="Bathylagiids"), the.main="Bathylagus sp.")
# Electrona sp.
plotbm(pdf.name="k_axis_electrona_biomass_total.pdf", bm.dat=subset(f.grps.bm, f.grps.bm$fish.grp=="Electrona sp."), the.main="Electrona sp.")

# Krefftichthys sp.
plotbm(pdf.name="k_axis_krefftichthys_biomass_total.pdf", bm.dat=subset(f.grps.bm, f.grps.bm$fish.grp=="Kreffichthys andersonii"), the.main="Krefftichthys sp.")

# Gymnoscopelus sp. 
plotbm(pdf.name="k_axis_gymnoscopelus_biomass_total.pdf", bm.dat=subset(f.grps.bm, f.grps.bm$fish.grp=="Gymnoscopelus sp."), the.main="Gymnoscopelus sp.")

# Protomyctophum sp. 
plotbm(pdf.name="k_axis_Protomyctophum_biomass_total.pdf", bm.dat=subset(f.grps.bm, f.grps.bm$fish.grp=="Protomyctophum sp"), the.main="Protomyctophum sp.")

# non-fish groups - subset from grps.bm 
# cephalopods
plotbm(pdf.name="k_axis_cephalopod_biomass_total.pdf", bm.dat=subset(grps.bm, grps.bm$tax.grp=="cephalopods"), the.main="Cephalopods")

# cnidarians
plotbm(pdf.name="k_axis_cnidarian_biomass_total.pdf", bm.dat=subset(grps.bm, grps.bm$tax.grp=="cnidarians"), the.main="Cnidarians")

# krill
plotbm(pdf.name="k_axis_krill_biomass_total.pdf", bm.dat=subset(grps.bm, grps.bm$tax.grp=="krill"), the.main="Krill")

# salps 
plotbm(pdf.name="k_axis_salps_biomass_total.pdf", bm.dat=subset(grps.bm, grps.bm$tax.grp=="salps"), the.main="Salps")

# mixed krill and salps
plotbm(pdf.name="k_axis_mixed_krill_salps_biomass_total.pdf", bm.dat=subset(grps.bm, grps.bm$tax.grp=="mixed krill and salps"), the.main="Mixed krill and salps")
