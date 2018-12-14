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
prenv <- readRDS("~/kaxis/fish_habitat_modelling/prey_env.rds")
env_data <- readRDS("~/kaxis/fish_habitat_modelling/env_data.rds")
load("~/kaxis/fish_habitat_modelling/KAXIS_vectors/KAXIS_FEATURESpoly_five.RData")
##set NA ice values to 0
prenv$ice[is.na(prenv$ice)] <- 0
prenv$dsm[prenv$dsm >= 1000] <- 150


##re-scale environmental variables
prenv$bathy_s <- scale(prenv$bathy, center=T, scale=T)
prenv$bath_g_s <- scale(prenv$bath_g, center=T, scale=T)
prenv$sst_s <- scale(prenv$sst, center=T, scale=T)
prenv$sstg_s <- scale(prenv$sstg, center=T, scale=T)
prenv$ssh_s <- scale(prenv$ssh, center=T, scale=T)
prenv$ssha_s <- scale(prenv$ssha, center=T, scale=T)
prenv$curr_s <- scale(prenv$curr, center=T, scale=T)
prenv$chl_rs_s <- scale(prenv$chl_rs, center=T, scale=T)
prenv$dsm_s <- scale(prenv$dsm, center=T, scale=T)
prenv$ice_s <- scale(prenv$ice, center=T, scale=T)
prenv$dtie_s<- scale(prenv$dtie, center=T, scale=T)



##examine environmental data for inter-correrlations
p <- data.frame(prenv)
m <- cor(p[,c(30:40)])
plot(m)
corrplot(m, method = "circle", cl.pos = "n")

##drop sst as it is highly correlated with ssha and dtie

#Examine distribution of response variables
par(mfrow=c(3,3))
hist(p$bathy_b, col="grey")

#multiply by 100000 to deal with very low values
p$bathy_b2 <- p$bathy_b *1000000


##preliminary GAM

m.null <- gam(bathy_b2~1, family="nb", data=p)
##single terms
m1.1 <- gam(bathy_b2~s(bathy_s, bs="cs"), family="nb", data=p)
m1.2 <- gam(bathy_b2~s(bath_g_s, bs="cs"), family="nb", data=p)
m1.3 <- gam(bathy_b2~s(sstg_s, bs="cs"), family="nb", data=p)
m1.4 <- gam(bathy_b2~s(ssh_s, bs="cs"), family="nb", data=p)
m1.5 <- gam(bathy_b2~s(ssha_s, bs="cs"), family="nb", data=p)
m1.6 <- gam(bathy_b2~s(chl_rs_s, bs="cs"), family="nb", data=p)
m1.7 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs"), family="nb", data=p)
m1.8 <- gam(bathy_b2~s(ice_s, k=3, bs="cs"), family="nb", data=p)
m1.9 <- gam(bathy_b2~s(dtie_s, bs="cs"), family="nb", data=p)


##calculate AICc
t1 <- data.frame(env=c("null", "bathy", "bathy_g", "sstg", "ssh", "ssha", "chl_rs", "dsm", "ice", "dtie"),
                 AICc=c(AICc(m.null), AICc(m1.1), AICc(m1.1), AICc(m1.3), AICc(m1.4), AICc(m1.5), AICc(m1.6), AICc(m1.7), 
                        AICc(m1.8), AICc(m1.9)))
t1 <- t1[order(t1$AICc),]
t1$delta <- c(0, diff(t1$AICc))
t1
#null model fits best, so no discernable relationship
##########################################################################
##do prediction
bc <- rasterToContour(env_data[["bathy"]])
##gymnotichthys
summary(m.null)
##make a data frame for predictions
vnames <- c("dsm", "sstg", "dtie")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
vd$pred <- predict(m.null)[1]


bathy_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(bathy_pred, col=viridis(100))
plot(f3$finished$geometry[1:5], add=T,col="black",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.5, col="red")

save(m.null, bathy_pred, bc, file="~/kaxis/fish_habitat_modelling/bathy_gam.Rdata")



