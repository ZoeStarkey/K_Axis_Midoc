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
prenv <- readRDS("~/kaxis/fish_habitat_modelling/prey_env_epi.rds")
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
m <- cor(p[,c(11:22)])
m
corrplot(m, method = "circle", cl.pos = "n")

#Examine distribution of response variables
par(mfrow=c(3,3))
hist(p$bathy_b, col="grey")

#multiply by 100000 to deal with very low values
p$bathy_b2 <- p$bathy_b *1000000


##preliminary GAM

m.null <- gam(bathy_b2~+1, family="nb", data=p)
##single terms
m1.1 <- gam(bathy_b2~s(bathy_s, bs="cs") + sol_pos, family="nb", data=p)
m1.1a <- gam(bathy_b2~s(bathy_s, bs="cs") +sol_pos, family="nb", data=p)
m1.2 <- gam(bathy_b2~s(bath_g_s, bs="cs") +sol_pos, family="nb", data=p)
m1.3 <- gam(bathy_b2~s(sstg_s, bs="cs") +sol_pos, family="nb", data=p)
m1.4 <- gam(bathy_b2~s(ssh_s, bs="cs") +sol_pos, family="nb", data=p)
m1.5 <- gam(bathy_b2~s(ssha_s, bs="cs") +sol_pos, family="nb", data=p)
m1.6 <- gam(bathy_b2~s(chl_rs_s, bs="cs") +sol_pos, family="nb", data=p)
m1.7 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs") +sol_pos, family="nb", data=p)
m1.8 <- gam(bathy_b2~s(ice_s, k=4, bs="cs") +sol_pos, family="nb", data=p)
m1.9 <- gam(bathy_b2~s(dtie_s, k=4, bs="cs") +sol_pos, family="nb", data=p)


##calculate AICc
t1 <- data.frame(env=c("null", "bathy", "bathy_g", "sstg", "ssh", "ssha", "chl_rs", "dsm", "ice", "dtie"),
                 AICc=c(AICc(m.null), AICc(m1.1), AICc(m1.1), AICc(m1.3), AICc(m1.4), AICc(m1.5), AICc(m1.6), AICc(m1.7), 
                        AICc(m1.8), AICc(m1.9)))
t1 <- t1[order(t1$AICc),]
t1$delta <- c(0, diff(t1$AICc))
t1
summary(m1.7)
par(mfrow=c(2,2))
gam.check(m1.7)
plot(m1.7, scale=0, page=1)

##two terms
m2.1 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(bathy_s, bs="cs") + sol_pos, family="nb", data=p)
m2.2 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(bath_g_s, bs="cs") + sol_pos, family="nb", data=p)
m2.3 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(sstg_s, bs="cs") + sol_pos, family="nb", data=p)
m2.4 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(ssh_s, bs="cs") + sol_pos, family="nb", data=p)
m2.5 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(ssha_s, bs="cs") + sol_pos, family="nb", data=p)
m2.6 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(chl_rs_s, bs="cs") + sol_pos, family="nb", data=p)
m2.7 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(ice_s, k=4, bs="cs") + sol_pos, family="nb", data=p)
m2.8 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(dtie_s, bs="cs") + sol_pos, family="nb", data=p)

##calculate AICc
t2 <- data.frame(env=c("null", 
                       "dsm+bathy", 
                       "dsm+bathy_g", 
                       "dsm+sstg",
                       "dsm+ssh",
                       "dsm+ssha",
                       "dsm+chl_rs",
                       "dsm+ice",
                       "dsm+dtie"),
                 AICc=c(AICc(m.null), AICc(m2.1), AICc(m2.2), AICc(m2.3), AICc(m2.4), AICc(m2.5), AICc(m2.6), 
                        AICc(m2.7), AICc(m2.8)))
t2 <- t2[order(t2$AICc),]
t2$delta <- c(0, diff(t2$AICc))
t2
summary(m2.1)
par(mfrow=c(2,2))
gam.check(m2.1)
plot(m2.1, scale=0, page=1)

summary(m1.7)$dev.expl
summary(m2.1)$dev.expl

##three terms
m3.1 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(bathy_s, bs="cs") + 
                     s(bath_g_s, bs="cs") + sol_pos, family="nb", data=p)
m3.2 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(bathy_s, bs="cs") +
                     s(sstg_s, bs="cs") + sol_pos, family="nb", data=p)
m3.3 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(bathy_s, bs="cs") + 
                     s(ssh_s, bs="cs") + sol_pos, family="nb", data=p)
m3.4 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(bathy_s, bs="cs") + 
                     s(ssha_s, bs="cs") + sol_pos, family="nb", data=p)
m3.5 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(bathy_s, bs="cs") + 
                     s(chl_rs_s, bs="cs") + sol_pos, family="nb", data=p)
m3.6 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(bathy_s, bs="cs") +
                     s(ice, k=4, bs="cs") + sol_pos, family="nb", data=p)
m3.7 <- gam(bathy_b2~s(dsm_s, k=3, bs="cs")+
                     s(bathy_s, bs="cs") +
                     s(dtie, bs="cs") + sol_pos, family="nb", data=p)


##calculate AICc
t3 <- data.frame(env=c("null", 
                       "dsm+bathy+bath_g", 
                       "dsm+bathy+sstg", 
                       "dsm+bathy+ssh",
                       "dsm+bathy+ssha",
                       "dsm+bathy+chl_rs",
                       "dsm+bathy+ice",
                       "dsm+bathy+dtie"),
                 AICc=c(AICc(m.null), AICc(m2.1), AICc(m2.2), AICc(m2.3), AICc(m2.4), AICc(m2.5), AICc(m2.6), 
                        AICc(m2.7)))
t3 <- t3[order(t3$AICc),]
t3$delta <- c(0, diff(t3$AICc))
t3
summary(m3.1)
par(mfrow=c(2,2))
gam.check(m3.1)
plot(m3.1, scale=0, page=1)

summary(m1.7)$dev.expl
summary(m2.1)$dev.expl
summary(m3.1)$dev.expl

##addtional terms doesn't help stick with two term model
## m2.1 is best (dsm+bathy)


##########################################################################
##do prediction
bc <- rasterToContour(env_data[["bathy"]])
##gymnotichthys
summary(m2.1)

hist(p$sol_pos)
##make a data frame for predictions
vnames <- c("dsm", "bathy")
v <- env_data[[which(names(env_data) %in% vnames)]]
vd <- as.data.frame(v, xy=T)
ndat <- data.frame(dsm_s=scale(vd$dsm, center=T, scale=T),
                   bathy_s=scale(vd$bathy, center=T, scale=T),
                   sol_pos=16)
vd$pred <- predict(m2.1, ndat)

bathy_epi_pred <- setValues(env_data[[1]],as.vector(vd$pred))
par(mfrow=c(1,1))
plot(bathy_epi_pred, col=viridis(100))
plot(f3$finished$geometry[1:5], add=T,col="black",lwd=3,lty=5) #SBdy
plot(bc, add=T)
points(p$lon_start, p$lat_start, cex=1.5, col="red")

save(m2.1, bathy_epi_pred, bc, file="~/kaxis/fish_habitat_modelling/bathy_epi_gam.Rdata")



