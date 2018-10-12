
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
load("~/kaxis/fish_habitat_modelling/kref_ubathy_gam.Rdata")

##kreftichthys
summary(m2.6)
##make a data frame for predictions
vnames <- c("ssh", "vssha")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat <- data.frame(ssh_s=scale(vd$ssh, center=T, scale=T),
                   vssha_s=scale(vd$vssha, center=T, scale=T))
vd$pred <- predict(m2.6, ndat)

kref_ubathy_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(kref_ubathy_pred, col=viridis(100))
plot(orsifronts, add=T)

##bathylagids - no significant model, so use the null to give uniform distribution
load("~/kaxis/fish_habitat_modelling/bathy_ubathy_gam.RData")
summary(m1.6)
##make a data frame for predictions
vnames <- c("ssha")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat <- data.frame(ssha_s=scale(vd$ssha, center=T, scale=T))
vd$pred <- predict(1.6)[1]

bathy_ubathy_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(bathy_ubathy_pred, col=viridis(100))
plot(orsifronts, add=T)

##gymnos and electrona
load("~/kaxis/fish_habitat_modelling/gymno_ubathy_gam.Rdata")
summary(mnull)
##make a data frame for predictions
vnames <- c("days_since_melt")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat <- data.frame(days_since_melt_s=scale(vd$days_since_melt, center=T, scale=T))
vd$pred <- predict(mnull)[1]

gyno_ubathy_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(gymno_ubathy_pred, col=viridis(100))
plot(orsifronts, add=T)

#########################################################################
##combine them to give overall biomass

biomass_ubathy <- kref_ubathy_pred+bathy_ubathy_pred+gymno_ubathy_pred
par(mfrow=c(1,1))
plot(biomass_ubathy, col=viridis(100))
plot(orsifronts, add=T)

