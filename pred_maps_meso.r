
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
load("~/kaxis/fish_habitat_modelling/kref_meso_gam.Rdata")

##kreftichthys
summary(m1.5)
##make a data frame for predictions
vnames <- c("ssh")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat <- data.frame(ssh_s=scale(vd$ssh, center=T, scale=T))
vd$pred <- predict(m1.5, ndat)

kref_meso_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(kref_meso_pred, col=viridis(100))
plot(orsifronts, add=T)

##bathylagids - no significant model, so use the null to give uniform distribution
load("~/kaxis/fish_habitat_modelling/bathy_meso_gam.RData")
summary(m.null)
##make a data frame for predictions
vnames <- c("ssh", "ssha")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
vd$pred <- predict(m.null)[1]

bathy_meso_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(bathy_meso_pred, col=viridis(100))
plot(orsifronts, add=T)

##gymnos and electrona
load("~/kaxis/fish_habitat_modelling/gymno_meso_gam.Rdata")
summary(m1.9)
##make a data frame for predictions
vnames <- c("days_since_melt")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat <- data.frame(days_since_melt_s=scale(vd$days_since_melt, center=T, scale=T))
vd$pred <- predict(m1.9, ndat)

gyno_meso_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(gymno_meso_pred, col=viridis(100))
plot(orsifronts, add=T)

#########################################################################
##combine them to give overall biomass

biomass_meso <- kref_meso_pred+bathy_meso_pred+gymno_meso_pred
par(mfrow=c(1,1))
plot(biomass_meso, col=viridis(100))
plot(orsifronts, add=T)

