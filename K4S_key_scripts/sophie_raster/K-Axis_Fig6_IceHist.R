  #bathym_f <- bathym; bathym_f[bathym_f>0] <- NA_real_
library(raster)
library(raster)
library(rgdal)
library(RColorBrewer)
source("C:/Users/bes049/BESTLEY/scripts/scripts_ssf/map_Ant.R") #DONT HAVE 
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts")
setwd(d)
getwd()


  #...................................
# ICE HISTORY


# voyage track waypoints
usr <- Sys.info()["user"]
wfile <- read.csv("source data/v3_201516030_waypoints_dec.csv",header=TRUE)
                  
colnames(wfile) <-c("waypt","lat","lon","grp")
wfile <- wfile[-which(wfile$lon > 96),]
ctdstns <- read.table('source data/stn_loc.txt') #DONT HAVE 

bx <- c(60, 95, -71, -55)  
ice_dts <- as.POSIXlt(as.Date(paste(1997:2016,"-02-16 UTC",sep=""))) # 20y
load(paste("sophie_raster/KAXIS_ssh.RData",sep=""))

tsm <- brick("sophie_raster/KAXIS_tsm.grd") 
#ZS: error in above code, testing out alternative options below 
library(rgdal)


tsm[tsm==32766] <- NA_real_ #land
tsm[tsm==32765] <- NA_real_ # OOZ
tsm[tsm>365] <- NA_real_ # not ice covered this season

last_day <- dim(tsm)[3] #30 # date of last CTD dip feb 16th (survey end)
tsm_e <- crop(tsm,extent(c(2e+06, 4e+06,-1e+06,2e+06))) # crop to E Ant only, smaller to reproject
tsm_e_ll <- projectRaster(tsm_e,ssh) # reproject to LL


####
# FIGURE
cx <- 0.8
ax.lab <- c("(a)","(b)")

save_plot <- 0
if (save_plot) {
pdf(width=15.5/2.54,height=24/2.54,#units="in", res=600, # can't get this to ignore the white background
  file="C:/Users/bes049/BESTLEY/scripts/MATLAB/KAXIS/output/figures/MS/BESTLEY_Fig5_IceHistV2.pdf")
}
if (!save_plot) {
dev.new(height=24/2.54,width=15.5/2.54,units="in")
}
par(new=FALSE,mar=c(1.25,1.5,0.15,5.5), oma=c(1,1,1,1), family="serif")
layout(matrix(1:2,2,1,byrow=FALSE), widths=c(3,3), heights=rep(5,5))

plot(extent(bx),bty="n",col="white",ylab="",xlab="" ,mgp=c(3,0.25,0),cex=0.5,axes=F)
mtext(paste("Days since ice melt at survey end (Feb 16 2016)"),side=3,line=1.5,cex=1.5)

ryb <- colorRampPalette(rev(brewer.pal(11,"RdYlBu")))
cols1 <- ryb(56); cols1 <- cols1[c(1:23, 32:54)] # remove very pale colours in middle?
use <- tsm_e_ll[[last_day]]; 

ctdjul <- as.Date(paste(ctdstns[,3],ctdstns[,4],ctdstns[,5],sep="-"))
ctdmelt <- as.Date("2016-02-16 UTC") - extract(tsm_e_ll[[last_day]], ctdstns[,1:2])
ctdmeltday <- as.POSIXlt(ctdmelt)$yday+1
ctdmelt > ctdjul  # ice not melted yet ...

use[use>150] <- 151 # cap at 150
use[use<0] <- -1
#plot(use,col=cols1,add=TRUE,zlim=c(-1,151)) # feb 16th
plot(use, col=cols1, add=TRUE,zlim=c(-25,185),
     #legend.width=1, legend.shrink=0.75,
     axis.args=list(at=c(0,seq(25,6*28, by=28)),
                    labels=c(0,seq(25,6*28, by=28)), 
                    cex.axis=0.8),
     legend.args=list(text='Days', side=3, font=2, line=0.5, cex=0.8))

add_coast()
add_shelves()
#contour(bathym, levels=c(-2000,-3000),add=TRUE,drawlabels=FALSE,col="gray75",lwd=2)

#contour(use,lev=c(60,90),add=T,col="black",lwd=2)
#contour(use,lev=seq(60,95,by=5),add=T,col="black",drawlabels=FALSE)
contour(use,lev=seq(25,5*28,by=7),add=T,col="black",drawlabels=FALSE) # weeks prior to survey
contour(use,lev= seq(25+28,4*28,by=28),add=T,col="black",lwd=2,drawlabels=FALSE) # 53  81 109 137 #4wk markers
contour(use,lev=c(25),add=T,col="green",lwd=2) # ice covered survey start
contour(use,lev=c(0),add=T,col="red",lwd=2) # ice covered survey end

# voyage track waypoints
lines(wfile$lon,wfile$lat,col="black",lwd=2)
points(ctdstns[,1],ctdstns[,2],col="black",cex=1.5,pch=16)
points(ctdstns[1,1],ctdstns[1,2],col="green",cex=1.5,pch=16)
points(ctdstns[47,1],ctdstns[47,2],col="red",cex=1.5,pch=16)
points(ctdstns[which(ctdmelt > ctdjul),1],ctdstns[which(ctdmelt > ctdjul),2],col="yellow",cex=1.5,pch=1)
points(ctdstns[21,1],ctdstns[21,2],col="yellow",cex=1.5,pch=1)

#mtext('Longitude',side=1,cex=1,line=1.5)
mtext('Latitude',side=2,cex=1,line=1.25)
box()
degAxis(1,at=seq(60,95,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(3,at=seq(60,95,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(2,at=seq(-70,-50,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(4,at=seq(-70,-50,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
mtext(ax.lab[1], 3, 0, cex=1,adj=-0.1,font=2)

###-----------------
# Ice melt as historical anomaly

mn_ice <- calc(tsm, fun = mean, na.rm = TRUE)
sd_ice <- sqrt(calc(tsm, fun = var, na.rm = TRUE))
anom_ice <- (tsm[[last_day]]-mn_ice)/sd_ice
 
plot_ver <- 2
use_mn <- crop(mn_ice,extent(c(2e+06, 4e+06,-1e+06,2e+06))) # crop to E Ant only, smaller to reproject
use_mn[use_mn< 0] <- -1; use_mn[use_mn>150] <- 151
use_mn <- projectRaster(use_mn,ssh) # reproject to LL

plot(extent(bx),bty="n",col="white",ylab="",xlab="" ,mgp=c(3,0.25,0),cex=0.5,axes=F)
cols1 <- ryb(56); #cols1 <- cols1[c(1:23, 32:54)] # don't remove very pale colours in middle?

if (plot_ver ==1){ # plot 20y mean
mtext(paste("20y mean ice melt"),side=3,line=1.5,cex=1.5)
#use[use<0] <- -1
plot(use_mn,col=cols1,add=TRUE,zlim=c(-1,151)) # feb 16th
contour(use_mn,lev=c(26),add=T,col="green",lwd=2) # ice covered survey start
contour(use_mn,lev=c(0),add=T,col="red",lwd=2) # ice covered survey end
contour(use_mn,lev=c(60,90),add=T,col="black",lwd=2)
contour(use_mn,lev=seq(60,95,by=5),add=T,col="black",drawlabels=FALSE)
}
if (plot_ver==2) {# straight difference
#mtext(paste("2016 ice melt - 20y mean"),side=3,line=1.5,cex=1.5)
tmp <- use - use_mn; zz <- c(-35,35);
tmp[tmp>zz[2]] <- zz[2]; tmp[tmp< zz[1]] <- zz[1];  #c(-90,90)

plot(tmp, col=cols1, add=TRUE,zlim=zz,
     #legend.width=1, legend.shrink=0.75,
     axis.args=list(at=seq(zz[1], zz[2], by=7),
                    labels=seq(zz[1], zz[2], by=7), 
                    cex.axis=0.8),
     legend.args=list(text='Days', side=3, font=2, line=0.5, cex=0.8))

#plot(tmp,col=cols1,add=TRUE,zlim=zz) # feb 16th
#contour(tmp,lev=c(-30,30),add=T,col="black",lwd=2)
#contour(tmp,lev=c(0),add=T,col="black",lwd=1)
#contour(bathym, levels=c(-2000,-3000),add=TRUE,drawlabels=FALSE,col="gray75",lwd=2)
contour(tmp,lev=c(-7,-14,-21,14,7,21),add=T,col="black",drawlabels=FALSE) # weeks prior to survey
#contour(tmp,lev=c(-7,7),add=T,col="black",drawlabels=FALSE) # weeks prior to survey

#contour(tmp,lev=seq(-28,28,by=7),add=T,col="gray75",drawlabels=FALSE) # weeks prior to survey
contour(tmp,lev= c(-28,28),add=T,col="black",lwd=2) # 53  81 109 137 #4wk markers
}
if (plot_ver==3) {# std'd anom
#mtext(paste("Standardised anomaly"),side=3,line=1.5,cex=1.5)
use_anom <- crop(anom_ice,extent(c(2e+06, 4e+06,-1e+06,2e+06))) # crop to E Ant only, smaller to reproject
use_anom <- projectRaster(use_anom,ssh) # reproject to LL

plot(use_anom,col=cols1,add=TRUE,zlim=c(-2.5,2.5)) # feb 16th
contour(use_anom,lev=c(-1,1),add=T,col="black",lwd=2)
contour(use_anom,lev=c(0),add=T,col="black",lwd=1)
}

add_coast()
add_shelves()

# voyage track waypoints
lines(wfile$lon,wfile$lat,col="black",lwd=2)
points(ctdstns[,1],ctdstns[,2],col="black",cex=1.5,pch=16)
points(ctdstns[1,1],ctdstns[1,2],col="green",cex=1.5,pch=16)
points(ctdstns[47,1],ctdstns[47,2],col="red",cex=1.5,pch=16)

mtext('Longitude',side=1,cex=1,line=1.25)
mtext('Latitude',side=2,cex=1,line=1.25)
box()
degAxis(1,at=seq(60,95,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(3,at=seq(50,100,by=5),labels=rep("",length(seq(50,100,by=5))),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(2,at=seq(-70,-50,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(4,at=seq(-70,-50,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
mtext(ax.lab[2], 3, 0, cex=1,adj=-0.1,font=2)

if (save_plot){
dev.off()
}

# plot(r, legend.only=TRUE, col=topo.colors(100),
#      legend.width=1, legend.shrink=0.75,
#      axis.args=list(at=seq(r.range[1], r.range[2], 25),
#                     labels=seq(r.range[1], r.range[2], 25), 
#                     cex.axis=0.6),
#      legend.args=list(text='Elevation (m)', side=4, font=2, line=2.5, cex=0.8))
