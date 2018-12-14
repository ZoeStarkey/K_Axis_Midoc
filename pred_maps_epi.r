
##uses top kerff model (not depth structured) to predict distribution
rm(list=ls(all=TRUE))
library(raster)
library(mgcv)
library(MuMIn)
library(corrplot)
library(viridis)
library(orsifronts)

##load data
env_data <- readRDS("~/kaxis/fish_habitat_modelling/env_data.rds")
load("~/kaxis/fish_habitat_modelling/kref_epi_gam.Rdata")

##kreftichthys
summary(m2.5)
##make a data frame for predictions
vnames <- c("days_since_melt", "ssh")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat <- data.frame(days_since_melt_s=scale(vd$days_since_melt, center=T, scale=T), 
                   ssh_s=scale(vd$ssh, center=T, scale=T))
vd$pred <- predict(m2.5, ndat)

kref_epi_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(kref_epi_pred, col=viridis(100))
plot(orsifronts, add=T)

##bathylagids
load("~/kaxis/fish_habitat_modelling/bathy_epi_gam.RData")
summary(m2.5)
##make a data frame for predictions
vnames <- c("days_since_melt",  "ssh")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat <- data.frame(days_since_melt_s=scale(vd$days_since_melt, center=T, scale=T) ,
                   ssh_s=scale(vd$ssh, center=T, scale=T))
vd$pred <- predict(m1.6, ndat)

bathy_epi_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(bathy_epi_pred, col=viridis(100))
plot(orsifronts, add=T)

##gymnos and electrona
load("~/kaxis/fish_habitat_modelling/gymno_epi_gam.Rdata")
summary(m1.11)
##make a data frame for predictions
vnames <- c("dtie")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat <- data.frame(dtie_s=scale(vd$dtie, center=T, scale=T))
vd$pred <- predict(m1.11, ndat)

gyno_epi_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(gymno_epi_pred, col=viridis(100))
plot(orsifronts, add=T)

#########################################################################
##combine them to give overall biomass

biomass_epi <- kref_epi_pred+bathy_epi_pred+gymno_epi_pred
par(mfrow=c(1,1))
plot(biomass_epi, col=viridis(100))
plot(orsifronts, add=T)

