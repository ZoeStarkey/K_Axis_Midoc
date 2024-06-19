library(raster)
library(ncdf4)
library(dplyr)
library(palr)
library(mapview)
library(sf)
library(RColorBrewer)


usr <- "bes049"
basedir <- paste("C:/Users/",usr,"/BESTLEY/",sep="")

source("C:/Users/bes049/BESTLEY/scripts/scripts_ssf/map_Ant.R")
bx <- c(60, 95, -71, -54)  

ryb <- colorRampPalette(rev(brewer.pal(11,"RdYlBu")))
cols1 <- ryb(11); #cols1 <- cols1[c(1:23, 32:54)] # remove very pale colours in middle?

# voyage track waypoints
wfile <- read.csv("source data/v3_201516030_waypoints_dec.csv",header=TRUE)
colnames(wfile) <-c("waypt","lat","lon","grp")
wfile <- wfile[-which(wfile$lon > 96),]
ctdstns <- read.table('C:/Users/bes049/BESTLEY/DATA/KAXIS/2016_Analysis/stn_loc.txt')

# CHLA re-extracted off VM on August greater domain (Schallenberg paper)
ncol=51; cols <- chlPal(ncol);
q1 <- 0.05; q2 <- 7.5; 
zz <- c(0.05, 0.1, 0.25, 0.5, 1, 2.5, 5,7.5)

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_CHLA_VoyagePeriod.RData")
R <- R_voy_jf; R[R >q2] <- q2; R[R<q1] <- q1; R <- log(R)

load("C:/Users/bes049/BESTLEY/scripts/MATLAB/KAXIS/output/KAXIS_FEATURESpoly_five.RData")
load("C:/Users/bes049/BESTLEY/scripts/MATLAB/KAXIS/output/KAXIS_FEATURESpoly_extras.RData")

#################################
# FIGURE - now exactly same dimensions as Fig 5
cx <- 0.8;
save_plot <- 0
if (save_plot) {
pdf(width=19.5/2.54,height=17.5/2.54,
  file="C:/Users/bes049/BESTLEY/scripts/MATLAB/KAXIS/output/figures/MS/BESTLEY_MS/reviewed/BESTLEY_Fig9_Chla.pdf")
}
if (!save_plot) {
dev.new(width=19.5/2.54,height=17.5/2.54,units="in")
}
par(new=FALSE,mar=c(2,1.25,1.5,5.5), oma=c(1,1,0,0), family="serif")

# Single panel for CHL-a
plot(extent(bx),main="",bty="n",col="white",ylab="",xlab="" ,mgp=c(3,0.25,0),cex=0.5,axes=F)
plot(R,col=chlPal(ncol),add=TRUE, zlim=log(c(q1,q2)),
       axis.args=list(at=log(zz),labels=zz, cex.axis=0.8),
     legend.args=list(text=expression(paste("Chl-a (mg ", m^-3,  ")", sep="")), side=3, font=2, line=0.5, cex=0.8,adj=0))

contour(bathym,add=TRUE,levels=c(-2000,-3000),col="black", drawlabels = FALSE)
add_coast()
add_shelves()
points(ctdstns[,1],ctdstns[,2],col="black",cex=1.5,pch=1,lwd=2)

box()
mtext('Longitude',side=1,cex=1,line=1.25)
mtext('Latitude',side=2,cex=1,line=1.25)
degAxis(1,at=seq(60,95,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(2,at=seq(-70,-50,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(3,at=seq(60,95,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(4,at=seq(-70,-50,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)

plot(f3$finished$geometry, add=T,col="black",lwd=3) # original
#plot(f3$finished$geometry[1], add=T,col="black",lwd=3,lty=5) #SB
# new extras
plot(f1$finished$geometry, add=T,col="black",lwd=3,lty=5) #extras

# arrows
for (i in 1:length(f3$finished$geometry))   {
  if (i!=3 & i!=1) { # not for eddy or SB
cc <- coordinates(as_Spatial(f3$finished$geometry[i]))[[1]][[1]]
xa <- cc[,1]; ya <- cc[,2];
arrows(tail(xa,2)[1],tail(ya,2)[1],tail(xa,2)[2],tail(ya,2)[2],col="black",lwd=2,angle=25,length=0.1)
} }
cc <- coordinates(as_Spatial(f1$finished$geometry[5]))[[1]][[1]]
xa <- cc[,1]; ya <- cc[,2];
arrows(tail(xa,2)[1],tail(ya,2)[1],tail(xa,2)[2],tail(ya,2)[2],col="black",lwd=2,angle=35,length=0.1)

# Naming zones and features
text(82,-65.75,"ASF",font=2, col="black")
text(63.5,-63.75,"SB",font=2, col="black")
text(94.5,-59.5,"SACCF",font=2, col="black")
text(83.25,-55.875,"FTC",font=2, col="black")

text(70,-61.5,"Antarctic",font=4, col="black")
text(77.5,-65.25,"Subpolar",font=4, col="black")
text(90,-61.5,"Southern",font=4, col="black")

if (save_plot) {
  dev.off()
}
########################################
# end figure
