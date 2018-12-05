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
prenv_epi <- data.frame(readRDS("~/kaxis/fish_habitat_modelling/prey_env_epi.rds"))
prenv_meso <- data.frame(readRDS("~/kaxis/fish_habitat_modelling/prey_env_meso.rds"))
prenv_ubathy <- data.frame(readRDS("~/kaxis/fish_habitat_modelling/prey_env_ubathy.rds"))

env_data <- readRDS("~/kaxis/fish_habitat_modelling/env_data.rds")
load("~/kaxis/fish_habitat_modelling/KAXIS_vectors/KAXIS_FEATURESpoly_five.RData")

prenv_epi$strata <- "epi"
prenv_meso$strata <- "meso"
prenv_ubathy$strata <- "ubathy"

prenv <- rbind(prenv_epi, prenv_meso, prenv_ubathy)

##re-scale environmental variables
prenv$bathy_s <- scale(prenv$bathy, center=T, scale=T)
prenv$ssh_s <- scale(prenv$ssh, center=T, scale=T)
prenv$curr_s <- scale(prenv$curr, center=T, scale=T)
prenv$dtie_s<- scale(prenv$dtie, center=T, scale=T)

##examine environmental data for inter-correrlations
p <- data.frame(prenv)
m <- cor(p[,c(33:36)])
m
corrplot(m, method = "circle", cl.pos = "n")

#Examine distribution of response variables
par(mfrow=c(1,1))
hist(p$kref_b, col="grey")

#multiply by 100000 to deal with very low values
p$kref_b2 <- p$kref_b *1000000
p$strat <- as.factor(p$strata)
##preliminary GAM

m.null <- gam(kref_b2~+1, family="nb", data=p)
##single terms
mk1a <- gam(kref_b2~ s(bathy_s, bs="cs"), 
                    family="nb", data=p)

mk1 <- gam(kref_b2~s(sol_pos, bs="cs", by = strat)+
                   s(bathy_s, bs="cs", by = strat)+
                   s(ssh_s, bs="cs", by = strat)+
#                   s(curr_s, bs="cs", by=strat),
                   s(dtie_s, bs="cs", by = strat), 
                   family="nb", select=TRUE, data=p)
summary(mk1)
plot(mk1, page=1)

# Do predictions
bc <- rasterToContour(env_data[["bathy"]])

##make a data frame for predictions
vnames <- c("bathy", "ssh", "dtie")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat_epi <- data.frame(bathy_s=scale(vd$bathy, center=T, scale=T),
                   ssh_s=scale(vd$ssh, center=T, scale=T),
                   dtie_s=scale(vd$dtie, center=T, scale=T),
                   sol_pos=16,
                   strat="epi")
ndat_meso <- data.frame(bathy_s=scale(vd$bathy, center=T, scale=T),
                       ssh_s=scale(vd$ssh, center=T, scale=T),
                       dtie_s=scale(vd$dtie, center=T, scale=T),
                       sol_pos=16,
                       strat="meso")
ndat_ubathy <- data.frame(bathy_s=scale(vd$bathy, center=T, scale=T),
                       ssh_s=scale(vd$ssh, center=T, scale=T),
                       dtie_s=scale(vd$dtie, center=T, scale=T),
                       sol_pos=16,
                       strat="ubathy")
vd$pred_epi <- predict(mk1, ndat_epi)
vd$pred_meso <- predict(mk1, ndat_meso)
vd$pred_ubathy <- predict(mk1, ndat_ubathy)

pdf("~/kaxis/fish_habitat_modelling/kref_preds.pdf", paper="a4r", height=7.4, width=9.8)
par(mfrow=c(2,2), mar=c(3,4,3,6))

kref_epi_pred <- setValues(env_data[[1]],as.vector(vd$pred_epi))
mn <- format(cellStats(kref_epi_pred, mean, na.rm=T), digits=2)
tit <- paste("Kreftichthys: Epi ",mn  )
plot(kref_epi_pred, col=viridis(100), zlim=c(0,6), axes=F, main=tit)
plot(f3$finished$geometry[1:5], add=T,col="grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.0, col="red")
degAxis(1)
degAxis(2, las=2)

kref_meso_pred <- setValues(env_data[[1]],as.vector(vd$pred_meso))
mn <- format(cellStats(kref_meso_pred, mean, na.rm=T), digits=2)
tit <- paste("Kreftichthys: Meso ",mn  )
plot(kref_meso_pred, col=viridis(100), zlim=c(0,6), axes=F, main=tit)
plot(f3$finished$geometry[1:5], add=T,col="grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.0, col="red")
degAxis(1)
degAxis(2, las=2)

kref_ubathy_pred <- setValues(env_data[[1]],as.vector(vd$pred_ubathy))
mn <- format(cellStats(kref_ubathy_pred, mean, na.rm=T), digits=2)
tit <- paste("Kreftichthys: Ubathy ",mn  )
plot(kref_ubathy_pred, col=viridis(100), zlim=c(0,6), axes=F, main=tit)
plot(f3$finished$geometry[1:5], add=T,col="grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.0, col="red")
degAxis(1)
degAxis(2, las=2)

kref_tot_pred <- sum(kref_epi_pred, kref_meso_pred, kref_ubathy_pred)
mn <- format(cellStats(kref_tot_pred, mean, na.rm=T), digits=2)
tit <- paste("Kreftichthys: Total Biomass ",mn  )
plot(kref_tot_pred, col=viridis(100), axes=F, main=tit)
plot(f3$finished$geometry[1:5], add=T,col="grey",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.0, col="red")
degAxis(1)
degAxis(2, las=2)
dev.off()


save(mk1, kref_epi_pred, kref_meso_pred, kref_ubathy_pred, kref_tot_pred, file="~/kaxis/fish_habitat_modelling/gam_fit_kref_all_strata.Rdata")

