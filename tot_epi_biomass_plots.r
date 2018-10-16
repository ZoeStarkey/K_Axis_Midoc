
rm(list=ls(all=TRUE))
library(raster)
library(mgcv)
library(MuMIn)
library(corrplot)
library(viridis)
library(mapview)
load("~/kaxis/fish_habitat_modelling/kref_epi_gam.Rdata")
load("~/kaxis/fish_habitat_modelling/gymno_epi_gam.Rdata")
load("~/kaxis/fish_habitat_modelling/bathy_epi_gam.Rdata")

prenv <- readRDS("~/kaxis/fish_habitat_modelling/prey_env_epi.rds")
env_data <- readRDS("~/kaxis/fish_habitat_modelling/env_data.rds")
load("~/kaxis/fish_habitat_modelling/KAXIS_vectors/KAXIS_FEATURESpoly_five.RData")

pdf("tot_epi_biomass.pdf", paper="a4r", height=9, width=8.2)
par(mfrow=c(2,2))
plot(kref_epi_pred, col=viridis(100), main="Kreftichthys- epi")
plot(f3$finished$geometry[1:5], add=T,col="light grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(prenv$lon_start, prenv$lat_start, cex=1.5, col="red")

plot(gymno_epi_pred, col=viridis(100), main="Gymnoscopelids- epi")
plot(f3$finished$geometry[1:5], add=T,col="light grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(prenv$lon_start, prenv$lat_start, cex=1.5, col="red")

plot(bathy_epi_pred, col=viridis(100), main="Bathylagids- epi")
plot(f3$finished$geometry[1:5], add=T,col="light grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(prenv$lon_start, prenv$lat_start, cex=1.5, col="red")

biomass_epi_pred <- kref_epi_pred+ gymno_epi_pred+bathy_epi_pred
plot(biomass_epi_pred, col=viridis(100), main="Total Epipelagic Biomass")
plot(f3$finished$geometry[1:5], add=T,col="light grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(prenv$lon_start, prenv$lat_start, cex=1.5, col="red")

dev.off()

