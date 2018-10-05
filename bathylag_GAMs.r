##run preiminasy GAMs on non-depth structured prey data
##uses "prey_env"

library(raster)
library(mgcv)
library(MuMIn)
library(corrplot)

##load data
prenv <- readRDS("~/kaxis/fish_habitat_modelling/prey_env.rds")
##set NA ice values to 0
prenv$ice[is.na(prenv$ice)] <- 0
prenv$days_since_melt[prenv$days_since_melt >= 1000] <- 150

##re-scale environmental variables
prenv$bathy_s <- scale(prenv$bathy, center=T, scale=T)
prenv$bath_g_s <- scale(prenv$bath_g, center=T, scale=T)
prenv$sst_s <- scale(prenv$sst, center=T, scale=T)
prenv$sstg_s <- scale(prenv$sstg, center=T, scale=T)
prenv$ssh_s <- scale(prenv$ssh, center=T, scale=T)
prenv$ssha_s <- scale(prenv$ssha, center=T, scale=T)
prenv$vssha_s <- scale(log(prenv$vssha), center=T, scale=T) ##log transform 
prenv$chl_rs_s <- scale(prenv$chl_rs, center=T, scale=T)
prenv$days_since_melt_s <- scale(prenv$days_since_melt, center=T, scale=T)
prenv$ice_s <- scale(prenv$ice, center=T, scale=T)
prenv$dtie_s<- scale(prenv$distance_to_edge_m, center=T, scale=T)



##examine environmental data for inter-correrlations
p <- data.frame(prenv)
m <- cor(p[,c(31:41)])
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
m1.1 <- gam(bathy_b2~s(bathy_s, bs="cs"), family="nb", data=p)
m1.2 <- gam(bathy_b2~s(bath_g_s, bs="cs"), family="nb", data=p)
m1.3<- gam(bathy_b2~s(sst_s, bs="cs"), family="nb", data=p)
m1.4 <- gam(bathy_b2~s(sstg_s, bs="cs"), family="nb", data=p)
m1.5 <- gam(bathy_b2~s(ssh_s, bs="cs"), family="nb", data=p)
m1.6 <- gam(bathy_b2~s(ssha_s, bs="cs"), family="nb", data=p)
m1.7<- gam(bathy_b2~s(vssha_s, k=4, bs="cs"), family="nb", data=p)
m1.8<- gam(bathy_b2~s(chl_rs_s, bs="cs"), family="nb", data=p)
m1.9<- gam(bathy_b2~s(days_since_melt_s, bs="cs"), family="nb", data=p)
m1.10<- gam(bathy_b2~s(ice_s, k=4, bs="cs"), family="nb", data=p)
m1.11<- gam(bathy_b2~s(dtie_s, k=4, bs="cs"), family="nb", data=p)


##calculate AICc
t1 <- data.frame(env=c("null", "bathy", "bathy_g", "sst", "sstg", "ssh", "ssha", "vssha", "chl_rs", "days_since_melt", "ice", "dtie"),
                 AICc=c(AICc(m.null), AICc(m1.1), AICc(m1.1), AICc(m1.3), AICc(m1.4), AICc(m1.5), AICc(m1.6), AICc(m1.7), 
                        AICc(m1.8), AICc(m1.9), AICc(m1.10), AICc(m1.11)))
t1 <- t1[order(t1$AICc),]
t1$delta <- c(0, diff(t1$AICc))
t1
summary(m1.6)
par(mfrow=c(2,2))
gam.check(m1.6)
plot(m1.6, scale=0, page=1)

##two terms
m2.1 <- gam(bathy_b2~s(ssha_s, bs="cs")+
                     s(bathy_s, bs="cs"), family="nb", data=p)
m2.2 <- gam(bathy_b2~s(ssha_s, bs="cs")+
                     s(bath_g_s, bs="cs"), family="nb", data=p)
m2.3 <- gam(bathy_b2~s(ssha_s, bs="cs")+
                     s(sst_s, bs="cs"), family="nb", data=p)
m2.4 <- gam(bathy_b2~s(ssha_s, bs="cs")+
                     s(sstg_s, bs="cs"), family="nb", data=p)
m2.5 <- gam(bathy_b2~s(ssha_s, bs="cs")+
                     s(ssh_s, bs="cs"), family="nb", data=p)
m2.6 <- gam(bathy_b2~s(ssha_s, bs="cs")+
                     s(vssha_s, bs="cs"), family="nb", data=p)
m2.7 <- gam(bathy_b2~s(ssha_s, bs="cs")+
                     s(chl_rs_s, bs="cs"), family="nb", data=p)
m2.8 <- gam(bathy_b2~s(ssha_s, bs="cs")+
                     s(days_since_melt_s, bs="cs"), family="nb", data=p)
m2.9 <- gam(bathy_b2~s(ssha_s, bs="cs")+
                     s(ice_s, k=4, bs="cs"), family="nb", data=p)
m2.10 <- gam(bathy_b2~s(ssha_s, bs="cs")+
                     s(dtie_s, bs="cs"), family="nb", data=p)




##calculate AICc
t2 <- data.frame(env=c("null", 
                       "ssha+bathy", 
                       "ssha+bathy_g", 
                       "ssha+sst", 
                       "ssha+sstg",
                       "ssha+ssh",
                       "ssha+vssha",
                       "ssha+chl_rs",
                       "ssha+dsm",
                       "ssha+ice",
                       "ssha+dtie"),
                 AICc=c(AICc(m.null), AICc(m2.1), AICc(m2.2), AICc(m2.3), AICc(m2.4), AICc(m2.5), AICc(m2.6), 
                        AICc(m2.7), AICc(m2.8), AICc(m2.9), AICc(m2.10)))
t2 <- t2[order(t2$AICc),]
t2$delta <- c(0, diff(t2$AICc))
t2


##addtional terms doesnt help - stick with the single term model

##top model is SSHa
summary(m1.6)
summary(m1.6)$dev.expl
