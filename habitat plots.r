rm(list=ls(all=TRUE))
library(raster)
library(mgcv)
library(MuMIn)
library(corrplot)
library(viridis)
library(mapview)
load("~/kaxis/fish_habitat_modelling/gam_fit_kref_all_strata.Rdata")
load("~/kaxis/fish_habitat_modelling/gam_fit_gymno_all_strata.Rdata")
load("~/kaxis/fish_habitat_modelling/gam_fit_bath_all_strata.Rdata")
load("~/kaxis/fish_habitat_modelling/KAXIS_vectors/KAXIS_FEATURESpoly_five.RData")
env_data <- readRDS("~/kaxis/fish_habitat_modelling/env_data.rds")

##examine environmental data for inter-correrlations
##all varaibles
p <- as.data.frame(env_data)
m <- cor(p[complete.cases(p),])
m
pdf("~/kaxis/fish_habitat_modelling/corr_mat_all.pdf", paper="a4r", height=10, width=10)
par(mfrow=c(1,1), mar=c(1,1,1,1))
corrplot(m, type = "upper", tl.pos="d", tl.cex=.75)
corrplot(m, add = TRUE, type = "lower", method = "number",
         col = "black", diag = FALSE, tl.pos = "n", cl.pos = "n")
dev.off()

##plot of environmmental variables used in the models
pdf("~/kaxis/fish_habitat_modelling/env_vars.pdf", paper="a4r", height=6.2, width=9.2)
par(mfrow=c(2,2), mar=c(3,3,1,7))
plot(env_data[[1]], col=inferno(100), axes=F,  main=names(env_data)[1])
degAxis(1)
degAxis(2, las=2)
plot(env_data[[6]], col=inferno(100), axes=F, main=names(env_data)[6])
degAxis(1)
degAxis(2, las=2)
plot(env_data[[10]], col=inferno(100), axes=F, main=names(env_data)[10])
degAxis(1)
degAxis(2, las=2)
plot(env_data[[14]], col=inferno(100), axes=F, main=names(env_data)[14])
degAxis(1)
degAxis(2, las=2)
dev.off()



##plot the splines from the GAMS
pdf("~/kaxis/fish_habitat_modelling/splines_v2.pdf", paper="a4r", height=7.4, width=9.8)
#par(mfrow=c(2,2), mar=c(3,4,3,6))
plot(mk1, page=1)
plot(mg1, page=1)
plot(mb1, page=1)
dev.off()

tbm <- sum(kref_tot_pred, gymno_tot_pred, bath_tot_pred)
kn <- format(cellStats(kref_tot_pred, mean, na.rm=T), digits=2)
gn <- format(cellStats(gymno_tot_pred, mean, na.rm=T), digits=2)
bn <- format(cellStats(bath_tot_pred, mean, na.rm=T), digits=2)
tn <- format(cellStats(tbm, mean, na.rm=T), digits=2)

pdf("~/kaxis/fish_habitat_modelling/biomass_preds_v2.pdf", paper="a4r", height=7.4, width=9.8)
par(mfrow=c(2,2), mar=c(3,4,3,6))
tit <- paste("Kreftichthys: Total Biomass ",kn  )
plot(kref_tot_pred, col=viridis(100), axes=F, main=tit)
plot(f3$finished$geometry[1:5], add=T, col="grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.0, col="red")
degAxis(1)
degAxis(2, las=2)

tit <- paste("Gymnoscopelids: Total Biomass ",gn  )
plot(gymno_tot_pred, col=viridis(100), axes=F, main=tit)
plot(f3$finished$geometry[1:5], add=T, col="grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.0, col="red")
degAxis(1)
degAxis(2, las=2)

tit <- paste("Bathylagids: Total Biomass ",bn  )
plot(bath_tot_pred, col=viridis(100), axes=F, main=tit)
plot(f3$finished$geometry[1:5], add=T, col="grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.0, col="red")
degAxis(1)
degAxis(2, las=2)

tit <- paste("Total Mesopelagic Biomass ",tn  )
plot(tbm, col=viridis(100), axes=F, main=tit)
plot(f3$finished$geometry[1:5], add=T, col="grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.0, col="red")
degAxis(1)
degAxis(2, las=2)
dev.off()
