
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
load("~/kaxis/fish_habitat_modelling/kref_gam.Rdata")
load("~/kaxis/fish_habitat_modelling/gymno_gam.Rdata")
load("~/kaxis/fish_habitat_modelling/bathy_gam.RData")

##kreftichthys
summary(m1.5)
##make a data frame for predictions
vnames <- c("ssh")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat <- data.frame(ssh_s=scale(vd$ssh, center=T, scale=T))
vd$pred <- predict(m1.5, ndat)

kref_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(kref_pred, col=viridis(100))
plot(orsifronts, add=T)

##bathylagids
summary(m1.6)
##make a data frame for predictions
vnames <- c("ssha")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat <- data.frame(ssha_s=scale(vd$ssha, center=T, scale=T))
vd$pred <- predict(m1.6, ndat)

bathy_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(bathy_pred, col=viridis(100))
plot(orsifronts, add=T)

##gymnos and electrona
summary(m3.5)
##make a data frame for predictions
vnames <- c("days_since_melt", "sstg", "ssha")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat <- data.frame(days_since_melt_s=scale(vd$days_since_melt, center=T, scale=T), 
                   sstg_s=scale(vd$sstg, center=T, scale=T),
                   ssha_s=scale(vd$ssha, center=T, scale=T))
vd$pred <- predict(m3.5, ndat)

gyno_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(gymno_pred, col=viridis(100))
plot(orsifronts, add=T)

#########################################################################
##combine them to give overall biomass

biomass <- kref_pred+bathy_pred+gymno_pred
par(mfrow=c(1,1))
plot(biomass, col=viridis(100))
plot(orsifronts, add=T)

