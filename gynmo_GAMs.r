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
hist(p$gymno_b, col="grey")

#multiply by 100000 to deal with very low values
p$gymno_b2 <- p$gymno_b *1000000


##preliminary GAM

m.null <- gam(gymno_b2~+1, family="nb", data=p)
##single terms
m1.1 <- gam(gymno_b2~s(bathy_s, bs="cs"), family="nb", data=p)
m1.2 <- gam(gymno_b2~s(bath_g_s, bs="cs"), family="nb", data=p)
m1.3<- gam(gymno_b2~s(sst_s, bs="cs"), family="nb", data=p)
m1.4 <- gam(gymno_b2~s(sstg_s, bs="cs"), family="nb", data=p)
m1.5 <- gam(gymno_b2~s(ssh_s, bs="cs"), family="nb", data=p)
m1.6 <- gam(gymno_b2~s(ssha_s, bs="cs"), family="nb", data=p)
m1.7<- gam(gymno_b2~s(vssha_s, k=4, bs="cs"), family="nb", data=p)
m1.8<- gam(gymno_b2~s(chl_rs_s, bs="cs"), family="nb", data=p)
m1.9<- gam(gymno_b2~s(days_since_melt_s, bs="cs"), family="nb", data=p)
m1.10<- gam(gymno_b2~s(ice_s, k=4, bs="cs"), family="nb", data=p)
m1.11<- gam(gymno_b2~s(dtie_s, k=4, bs="cs"), family="nb", data=p)


##calculate AICc
t1 <- data.frame(env=c("null", "bathy", "bathy_g", "sst", "sstg", "ssh", "ssha", "vssha", "chl_rs", "days_since_melt", "ice", "dtie"),
                 AICc=c(AICc(m.null), AICc(m1.1), AICc(m1.1), AICc(m1.3), AICc(m1.4), AICc(m1.5), AICc(m1.6), AICc(m1.7), 
                        AICc(m1.8), AICc(m1.9), AICc(m1.10), AICc(m1.11)))
t1 <- t1[order(t1$AICc),]
t1$delta <- c(0, diff(t1$AICc))
t1
summary(m1.9)
par(mfrow=c(2,2))
gam.check(m1.9)
plot(m1.9, scale=0, page=1)

##two terms
m2.1 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                    s(bathy_s, bs="cs"), family="nb", data=p)
m2.2 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                    s(bath_g_s, bs="cs"), family="nb", data=p)
m2.3 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                    s(sst_s, bs="cs"), family="nb", data=p)
m2.4 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                    s(sstg_s, bs="cs"), family="nb", data=p)
m2.5 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                    s(ssh_s, bs="cs"), family="nb", data=p)
m2.6 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                    s(ssha_s, bs="cs"), family="nb", data=p)
m2.7 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                    s(vssha_s, bs="cs"), family="nb", data=p)
m2.8 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                    s(chl_rs_s, bs="cs"), family="nb", data=p)
m2.9 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                    s(ice_s, k=4, bs="cs"), family="nb", data=p)
m2.10 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                    s(dtie_s, k=4, bs="cs"), family="nb", data=p)

##calculate AICc
t2 <- data.frame(env=c("null", 
                       "dsm+bathy", 
                       "dsm+bathy_g", 
                       "dsm+sst", 
                       "dsm+sstg",
                       "dsm+ssh",
                       "dsm+ssha",
                       "dsm+vssha",
                       "dsm+chl_rs",
                       "dsm+ice",
                       "dsm+dtie"),
                 AICc=c(AICc(m.null), AICc(m2.1), AICc(m2.2), AICc(m2.3), AICc(m2.4), AICc(m2.5), AICc(m2.6), 
                        AICc(m2.7), AICc(m2.8), AICc(m2.9), AICc(m2.10)))
t2 <- t2[order(t2$AICc),]
t2$delta <- c(0, diff(t2$AICc))
t2
summary(m2.4)
par(mfrow=c(2,2))
gam.check(m2.4)
plot(m2.4, scale=0, page=1)

summary(m1.9)$dev.expl
summary(m2.4)$dev.expl

##addtional terms helps so now try a three term model


#####################################################################

##three terms
m3.1 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                   s(sstg_s, bs="cs")+
                   s(bathy_s, bs="cs"), family="nb", data=p)
m3.2 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                   s(sstg_s, bs="cs")+
                   s(bath_g_s), family="nb", data=p)
m3.3 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                   s(sstg_s, bs="cs")+
                   s(sst_s, bs="cs"), family="nb", data=p)
m3.4 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                   s(sstg_s, bs="cs")+
                   s(ssh_s, bs="cs"), family="nb", data=p)
m3.5 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                   s(sstg_s, bs="cs")+
                   s(ssha_s, bs="cs"), family="nb", data=p)
m3.6 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                   s(sstg_s, bs="cs")+
                   s(vssha_s, bs="cs"), family="nb", data=p)
m3.7 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                   s(sstg_s, bs="cs")+
                   s(chl_rs_s, bs="cs"), family="nb", data=p)
m3.8 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                   s(sstg_s, bs="cs")+
                   s(ice_s, k=4, bs="cs"), family="nb", data=p)
m3.9 <- gam(gymno_b2~s(days_since_melt_s, bs="cs")+
                   s(sstg_s, bs="cs")+
                   s(dtie_s, bs="cs"), family="nb", data=p)



##calculate AICc
t3 <- data.frame(env=c("null",
                       "dsm+sstg+bathy", 
                       "dsm+sstg+bath_g", 
                       "dsm+sstg+sst",
                       "dsm+sstg+ssh",
                       "dsm+sstg+ssha",
                       "dsm+sstg+vssah",
                       "dsm+sstg+chl_rs",
                       "dsm+sstg+ice",
                       "dsm+sstg+dtie"), 
                 AICc=c(AICc(m.null), AICc(m3.1), AICc(m3.2), AICc(m3.3), AICc(m3.4), AICc(m3.5), AICc(m3.6),
                        AICc(m3.7), AICc(m3.8), AICc(m3.9)))
t3 <- t3[order(t3$AICc),]
t3$delta <- c(0, diff(t3$AICc))
t3
summary(m3.5)
par(mfrow=c(2,2))
gam.check(m3.5)
plot(m3.5, scale=0, page=1)

summary(m1.9)$dev.expl
summary(m2.4)$dev.expl
summary(m3.5)$dev.expl


####top model is DSM+sstg+ssha

