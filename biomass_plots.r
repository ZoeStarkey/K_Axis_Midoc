##run preiminasy GAMs on non-depth structured prey data
##uses "prey_env"
rm(list=ls(all=TRUE))
library(raster)
library(mgcv)
library(MuMIn)
library(corrplot)
library(viridis)
library(mapview)

##load data
load("~/kaxis/fish_habitat_modelling/gam_fit_kref_all_strata.Rdata")
load("~/kaxis/fish_habitat_modelling/gam_fit_gymno_all_strata.Rdata")
load("~/kaxis/fish_habitat_modelling/gam_fit_bath_all_strata.Rdata")
prenv_epi <- data.frame(readRDS("~/kaxis/fish_habitat_modelling/prey_env_epi.rds"))
p <- prenv_epi

env_data <- readRDS("~/kaxis/fish_habitat_modelling/env_data.rds")
load("~/kaxis/fish_habitat_modelling/KAXIS_vectors/KAXIS_FEATURESpoly_five.RData")
bc <- rasterToContour(env_data[["bathy"]])

pdf("~/kaxis/fish_habitat_modelling/biomass_preds.pdf", paper="a4r", height=8, width=9.6)
par(mfrow=c(2,2), mar=c(3,4,3,4))

mn <- format(cellStats(kref_tot_pred, mean, na.rm=T), digits=2)
tit <- paste("Kreftichthys: ",mn  )
plot(kref_tot_pred, col=viridis(100), axes=F, main=tit)
plot(f3$finished$geometry[1:5], add=T,col="grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.0, col="red")
degAxis(1)
degAxis(2, las=2)

mn <- format(cellStats(gymno_tot_pred, mean, na.rm=T), digits=2)
tit <- paste("Gymnoscopelids: ",mn  )
plot(gymno_tot_pred, col=viridis(100), axes=F, main=tit)
plot(f3$finished$geometry[1:5], add=T,col="grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.0, col="red")
degAxis(1)
degAxis(2, las=2)

mn <- format(cellStats(bath_tot_pred, mean, na.rm=T), digits=2)
tit <- paste("Bathylagids: ",mn  )
plot(bath_tot_pred, col=viridis(100), axes=F, main=tit)
plot(f3$finished$geometry[1:5], add=T,col="grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.0, col="red")
degAxis(1)
degAxis(2, las=2)

biom_tot_pred <- sum(kref_tot_pred, gymno_tot_pred, bath_tot_pred)
mn <- format(cellStats(biom_tot_pred, mean, na.rm=T), digits=2)
tit <- paste("Total Mesopelagic Biomass ",mn  )
plot(biom_tot_pred, col=viridis(100), axes=F, main=tit)
plot(f3$finished$geometry[1:5], add=T,col="grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.0, col="red")
degAxis(1)
degAxis(2, las=2)
dev.off()


save(biom_tot_pred, file="~/kaxis/fish_habitat_modelling/biom_tot_pred.Rdata")

##plot splines

pdf("~/kaxis/fish_habitat_modelling/splines.pdf", paper="a4r", height=8, width=10)
plot(mk1, pages=1, main="Kref")
plot(mg1, pages=1, main="Gymno")
plot(mb1, pages=1, main="Bathy")
dev.off()

##plot GAM checks
pdf("~/kaxis/fish_habitat_modelling/gam_checks.pdf", paper="a4r", height=8, width=10)
par(mfrow=c(2,2))
gam.check(mk1)
par(mfrow=c(2,2))
gam.check(mg1)
par(mfrow=c(2,2))
gam.check(mb1)
dev.off()

