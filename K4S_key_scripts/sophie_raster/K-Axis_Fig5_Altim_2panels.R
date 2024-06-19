library(raster)
usr <- "bes049"
basedir <- paste("C:/Users/",usr,"/BESTLEY/",sep="")

source("C:/Users/bes049/BESTLEY/scripts/scripts_ssf/map_Ant.R")
bathym2 <- bathym
bathym2[bathym2 < -3000] <-NA_real_
bx <- c(60, 95, -71, -54)  

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_curr_big.RData")
mag <- crop(mag,extent(bx+c(-3,3,-3,3)),snap="out")
u <- crop(u,extent(bx+c(-3,3,-3,3)),snap="out")
v <- crop(v,extent(bx+c(-3,3,-3,3)),snap="out")

# Mikes handy function for holding longest contours
keepOnlyMostComplexLine <- function(x) {
  for (iObj in seq_len(nrow(x))) {
    wmax <- which.max(sapply(x[iObj, ]@lines[[1]]@Lines, function(x)
      nrow(x@coords)))
    x@lines[[iObj]]@Lines <- x@lines[[iObj]]@Lines[wmax]
    
  }
  x
}

ryb <- colorRampPalette(rev(brewer.pal(11,"RdYlBu")))
cols1 <- ryb(11); #cols1 <- cols1[c(1:23, 32:54)] # remove very pale colours in middle?

# voyage track waypoints
wfile <- read.csv("C:/Users/bes049/BESTLEY/DATA/KAXIS/2016_Analysis/v3_201516030_waypoints_dec.csv",header=TRUE)
colnames(wfile) <-c("waypt","lat","lon","grp")
wfile <- wfile[-which(wfile$lon > 96),]

# where exactly were the ctd stations??
ctdstns <- read.table('C:/Users/bes049/BESTLEY/DATA/KAXIS/2016_Analysis/stn_loc.txt')


# re-extracted off VM on 23/5/17 greater domain
dts <- as.POSIXlt(seq(as.Date("2016-01-18"), as.Date("2016-02-18"), by = "1 day"), tz = "UTC")
#bx <- c(50, 100, -70, -40)  
#ssh <- readssh(dts,time="daily",ssha=FALSE
load(paste("C:/Users/",usr,"/BESTLEY/scripts/MATLAB/KAXIS/data/KAXIS_ssh.RData",sep=""))

frontsdir <- paste("C:/Users/",usr,"/BESTLEY/DATA/fronts/",sep="")
saccf <- data.frame(read.table(file.path(frontsdir,"orsi","saccf.txt"),header=FALSE,comment.char = "%") )  # see BROKE_adcp.m
del <- which(saccf[,1] > 80 & saccf[,1]<90 & saccf[,2]> -58)
saccf <- saccf[-del ,]; saccf <- rbind(saccf,c(NA, NA)); saccf <- saccf[which(saccf[,1]>0),] # keep only EAnt
sbdy <- data.frame(read.table(file.path(frontsdir,"orsi","sbdy.txt"),header=FALSE,comment.char = "%") )  # see BROKE_adcp.m

sok_saccfN <- data.frame(read.table(file.path(frontsdir,"sokolov","sACCf-N.txt"),header=FALSE) )  # see BROKE_adcp.m
sok_saccfS <- data.frame(read.table(file.path(frontsdir,"sokolov","sACCf-S.txt"),header=FALSE) )  # see BROKE_adcp.m
sok_bdy <- data.frame(read.table(file.path(frontsdir,"sokolov","Bdy.txt"),header=FALSE) )  # see BROKE_adcp.m

######################################
#improve sok_saccfS
library(ncdf4)

sokfile <- 'C://Users/bes049/BESTLEY/DATA/fronts/sokolov/ACCfronts.nc'
nc <- nc_open(sokfile)
print(nc)
sok <- ncvar_get(nc,'front');
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")

nc_close(nc)

sok_mn <- apply(sok,c(1,2),mean,na.rm=TRUE)
#xy <- expand.grid(x = lon, y = lat)
#df <-data.frame(x = xy$x, y = xy$y, sok = as.vector(sok_mn))
#sm <- rasterFromXYZ(df)
dev.new()
image(lon,lat,sok_mn)
contour(lon,lat,sok_mn,lev=c(2.5,2.5),col="blue",add=T,lwd=2)
contour(lon,lat,sok_mn,lev=c(3.5,3.5),col="magenta",add=T,lwd=2)
contour(lon,lat,sok_mn,lev=c(1.5,1.5),col="black",add=T,lwd=2)
contour(lon,lat,sok_mn,lev=c(4.5,4.5),col="magenta",add=T,lwd=2)

######################################
# ADD ICE (leftover from KAXIS_MAPS_2017)
dp <- paste("C:/Users/",usr,"/BESTLEY/DATA/KAXIS/Kerguelen2016_v3/K_2016_Feb_18",sep="")
icefile <- file.path(dp,paste("k-axis_data_ICE_LONGLAT_2016","02","18",".tif",sep="")) # last date
ice<- raster(icefile)
ice2 <- ice; ice2[ice == 0] <- NA_real_

######################################
# bring in GV calc - try with 1500m ref level
gv_bot <- read.table('C:/Users/bes049/BESTLEY/scripts/MATLAB/KAXIS/output/GVbot.txt')
names(gv_bot) <- c("stn","ctdlon","ctdlat","midlon","midlat","gv")
gv_bot$gv2 <- gv_bot$gv/2; bigv <- c(33,38,39)
#gv_bot$gv2[which(gv_bot$stn %in% bigv)] <- gv_bot$gv[which(gv_bot$stn %in% bigv)]/2

gv_1500 <- read.table('C:/Users/bes049/BESTLEY/scripts/MATLAB/KAXIS/output/GV_1500.txt')
names(gv_1500) <- c("stn","ctdlon","ctdlat","midlon","midlat","gv")
gv_1500$gv2 <- gv_1500$gv; 
gv_1500$gv2[gv_1500$stn==33] <- gv_1500$gv[gv_1500$stn==33]/2
gv_1500$gv2[gv_1500$stn==38] <- gv_1500$gv[gv_1500$stn==38]/2
d <- 2000000

library(geosphere)
revdir <- c(1:14,21:30,38:43); #T2,T3,T6,T9
for (ii in 1:2){
if (ii==1) {gv <- gv_1500}
if (ii==2) {gv <- gv_bot}

gv$br <- NA; gv$bradj <- NA; gv$p2lon <- NA; gv$p2lat <- NA;
for (i in 1:(nrow(gv)-1)){
  gv$br[i] <- bearingRhumb(c(gv$ctdlon[i], gv$ctdlat[i]), c(gv$ctdlon[i+1], gv$ctdlat[i+1]))
  if (gv$stn[i] %in% revdir & !is.na(gv$gv[i])){
    if(gv$gv[i]>0) {gv$bradj[i] <- gv$br[i]+90}# positive current
    if(gv$gv[i]<0) {gv$bradj[i] <- gv$br[i]-90}# negative current
  } else {
if (!is.na(gv$gv[i])) {
   if(gv$gv[i]>0) {gv$bradj[i] <- gv$br[i]-90}# positive current
    if(gv$gv[i]<0) {gv$bradj[i] <- gv$br[i]+90}# negative current
}
  }
  if (!is.na(gv$gv[i])) {
gv[i,c("p2lon","p2lat")] <- destPointRhumb(c(gv$midlon[i], gv$midlat[i]), gv$bradj[i], d*abs(gv$gv2[i]))

}
}
if (ii==1) {gv_1500 <- gv}
if (ii==2) {gv_bot <- gv}

}


#################################
# FIGURE - now exactly same dimensions as Fig 6
cx <- 0.8;
save_plot <- 0
ax.lab <- c("(a)","(b)")
if (save_plot) {
pdf(width=15.5/2.54,height=24/2.54,#units="in", res=600, # can't get this to ignore the white background
  file="C:/Users/bes049/BESTLEY/scripts/MATLAB/KAXIS/output/figures/MS/BESTLEY_Fig6_Altim_Rev.pdf")
}
if (!save_plot) {
dev.new(height=24/2.54,width=15.5/2.54,units="in") #dev.new(width=20.5/2.54,height=19/2.54,units="in")
}
layout(matrix(1:2,2,1,byrow=FALSE), widths=c(3,3), heights=rep(5,5))
par(new=FALSE,mar=c(1.25,1.5,0.15,5.5), oma=c(1,1,1,1), family="serif") #par(new=FALSE,mar=c(2,1.25,1.5,5.5), oma=c(1,1,0,0), family="serif")
plot(extent(bx),bty="n",col="white",ylab="",xlab="" ,mgp=c(3,0.25,0),cex=0.5,axes=F)
plot(bathym2,breaks=c(seq(-5500,0,by=500)),col=gray.colors(12)[2:12],add=TRUE,legend=FALSE)
# tidy edges
contour(bathym, levels=c(-3000,-2950,-2900,-2850,-2800,-2750),add=TRUE,drawlabels=FALSE,col=gray.colors(12)[7],lwd=3)
mtext(ax.lab[1], 3, 0, cex=1,adj=-0.1,font=2)

plotcols <- cols1#plotcols <- c(rev(brewer.pal(9,"Blues")))

for (idate in 1:length(dts)){
# background
contour(ssh[[idate]],lev=c(-0.90),add=TRUE,col=plotcols[9],drawlabels=FALSE,lwd=0.5) #plotcols[7]
contour(ssh[[idate]],lev=c(-0.95),add=TRUE,col=plotcols[8],drawlabels=FALSE,lwd=0.5) #plotcols[6]
contour(ssh[[idate]],lev=c(-1.05),add=TRUE,col=plotcols[6],drawlabels=FALSE,lwd=0.5) #plotcols[4]
#contour(ssh[[idate]],lev=c(-1.1),add=TRUE,col=plotcols[4],drawlabels=FALSE,lwd=0.5) #plotcols[3]
contour(ssh[[idate]],lev=c(-1.2),add=TRUE,col="mediumpurple2",drawlabels=FALSE,lwd=0.5) #plotcols[1]
contour(ssh[[idate]],lev=c(-1.25),add=TRUE,col="purple4",drawlabels=FALSE,lwd=0.5)

}
# of interest; over the top - SACCF
for (idate in 1:length(dts)){
contour(ssh[[idate]],lev=c(-1.1),add=TRUE,col=plotcols[4],drawlabels=FALSE,lwd=0.5) #plotcols[3]
}
contour(min(ssh),lev=c(-1.1),add=T,col="steelblue1",lwd=2,drawlabels=FALSE) # under ice
contour(max(ssh),lev=c(-1.1),add=T,col="steelblue1",lwd=2,drawlabels=FALSE) # under ice

# of interest; over the top - SACCF-N and SB
for (idate in 1:length(dts)){
contour(ssh[[idate]],lev=c(-1.0),add=TRUE,col=plotcols[7],drawlabels=FALSE,lwd=1) #plotcols[5]
contour(ssh[[idate]],lev=c(-1.15),add=TRUE,col=plotcols[3],drawlabels=FALSE,lwd=1) #plotcols[2]
}
contour(min(ssh),lev=c(-1.0),add=T,col="orange1",lwd=2,drawlabels=FALSE) # under ice
contour(max(ssh),lev=c(-1.0),add=T,col="orange1",lwd=2,drawlabels=FALSE) # under ice
contour(bathym, levels=c(-3000,-2000),add=TRUE,drawlabels=FALSE,col=gray.colors(12)[5],lwd=2)

# N-S extent of the envelope
contour(min(ssh),lev=c(-1.15),add=T,col="steelblue3",lwd=2,drawlabels=FALSE) # top eddies
contour(max(crop(ssh,extent(c(50,65,-70,-60)))),lev=c(-1.15),add=T,col="steelblue3",lwd=2,drawlabels=FALSE) # one bottom eddy
plot(ice2,col = palr::bathyDeepPal(56),add=TRUE,legend=TRUE,zlim=c(0,100),#alpha=0.8,
  legend.args=list(text='Ice (%)', side=3, font=2, line=1, cex=0.8,adj=0),
     axis.args=list(cex.axis=0.8)  )

plot(keepOnlyMostComplexLine(rasterToContour(min(ssh), lev = -1.15)),col="steelblue3",add=TRUE,lwd=2)
plot(keepOnlyMostComplexLine(rasterToContour(max(ssh), lev = -1.15)),col="steelblue3",add=TRUE,lwd=2)

if (0) { # simpler to leave these until the final new Fig 10 composite
lines(sbdy[,1],sbdy[,2],lwd=3,lty="twodash",col="black")
lines(saccf[,1],saccf[,2],lwd=3,lty="twodash",col="blue")
use <- sok_saccfN[,1] >50 & sok_saccfN[,1] < 100
lines(sok_saccfN[use,1],sok_saccfN[use,2],col="blue",lwd=3)# SACCF-N

contour(lon,lat,sok_mn,lev=c(1.5,1.5),col="blue",add=T,lwd=3,lty=1) # SACCF-S
lines(c(63,63.5, 64.1),c(-65.1,-65.1,-65.075),col="blue",lwd=3,lty=1) # small patch
contour(lon,lat,sok_mn,lev=c(3.5,3.5),col="magenta",add=T,lwd=2,lty=1) # PF-S
contour(lon,lat,sok_mn,lev=c(4.5,4.5),col="magenta",add=T,lwd=2) # PF-M
}

add_coast()
add_shelves()
lines(wfile$lon,wfile$lat,col="white",lwd=2)# voyage track waypoints
points(ctdstns[,1],ctdstns[,2],col="white",cex=1.25,pch=16)
lines(wfile$lon,wfile$lat,col="black",lwd=1.5)# voyage track waypoints
points(ctdstns[,1],ctdstns[,2],col="black",cex=1,pch=16)

box()
mtext('Latitude',side=2,cex=1,line=1.25)
degAxis(1,at=seq(60,95,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(3,at=seq(60,95,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(2,at=seq(-70,-50,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(4,at=seq(-70,-50,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)

# LEGEND
text(85,-67.6,"SSH contours (m)",font=1,adj=0,cex=cx)
legend(81,-66.85, title="",title.adj=1,
  legend=c("-0.90","-0.95","-1.00","-1.05"),
  #col=c(plotcols[5],"paleturquoise1",plotcols[4],plotcols[3],"purple",plotcols[2]),
  col=c(plotcols[c(9,8,7,6)]),
  lwd=2, cex=cx,lty=1,bty="n",pt.cex=0.5)
legend(88,-66.85, title="",title.adj=1,
  legend=c("-1.10","-1.15","-1.20","-1.25"),
  #col=c(plotcols[5],"paleturquoise1",plotcols[4],plotcols[3],"purple",plotcols[2]),
  col=c(plotcols[c(4,3)],"mediumpurple2","purple4"),
  lwd=2, cex=cx,lty=1,bty="n",pt.cex=0.5)

if (0) {
text(59,-68.1,"SR(09)",adj=0,cex=cx)
legend(58,-67.35,title="",title.adj=0.15,
  legend=c("PF-M & -S","SACCF-N & -S"),
  col=c("magenta","blue"),lty=1,lwd=2,cex=cx-0.1,bty="n",pt.cex=0.5)
text(59,-70.1,"Orsi(95)",adj=0,cex=cx)
legend(58,-69.3,title=c(""),title.adj=0.2,
  legend=c("SACCF","SB"),
  col=c("blue","black"),lty="twodash",lwd=2,cex=cx-0.1,bty="n",pt.cex=0.5)
}

########################################
# New panel (b)
ryb2 <- colorRampPalette(brewer.pal(9,"YlOrRd"))
cols2 <- ryb2(56)

scale <- 5
crds <- coordinates(mag)
xx1 <- crds[,1]
yy1 <- crds[,2]
mn_mag <- max(mag); # mean current spds; quantile(max(mag),0.9,na.rm=T) -- 85% of velocities are < 25cm/s
qu <- as.numeric(quantile(values(max(mag)),c(0.1,0.9),na.rm=TRUE))
#mn_mag[mn_mag> qu[2]] <- qu[2]; mn_mag[mn_mag< qu[1]] <- qu[1]
mn_mag[mn_mag> 0.25] <- 0.255; mn_mag[mn_mag< 0.025] <- 0.025

mn_u <- mean(u) # mean directions, overplotting maximum velocities
mn_v <- mean(v)
xx2 <- crds[,1] + values(mn_u) * scale * 1.2 # axis distances are slightly non-equivalent
yy2 <- crds[,2] + values(mn_v) * scale 
# 2113 km lat v 1745 lon

plot(extent(bx),bty="n",col="white",ylab="",xlab="" ,mgp=c(3,0.25,0),cex=0.5,axes=F)
# MEAN 
plot(mn_mag*100, col = cols2, interpolate = FALSE,add=TRUE,legend = TRUE,
    legend.args=list(text=expression(paste("cm ", s^-1, sep="")), side=3, font=2, line=1, cex=0.8,adj=0),
     axis.args=list(cex.axis=0.8)  )
contour(bathym, levels=c(-3000,-2000),add=TRUE,drawlabels=FALSE,col=gray.colors(12)[3],lwd=2)
b<-3;
arrows(xx1[seq(1,length(xx1),by=b)], yy1[seq(1,length(xx1),by=b)], # subsample by 2s
  xx2[seq(1,length(xx1),by=b)], yy2[seq(1,length(xx1),by=b)], length = 0.04,col="black",angle=10,code=2)

mtext(ax.lab[2], 3, 0, cex=1,adj=-0.1,font=2)
add_coast()
add_shelves()
lines(wfile$lon,wfile$lat,col="white",lwd=2)# voyage track waypoints
points(gv_1500$ctdlon, gv_1500$ctdlat,col="white",cex=1.25,pch=16) # jsut those used for GV calc
lines(wfile$lon,wfile$lat,col="black",lwd=1.5)# voyage track waypoints
points(gv_1500$ctdlon, gv_1500$ctdlat,col="black",cex=1,pch=16)

box()
mtext('Longitude',side=1,cex=1,line=1.25)
mtext('Latitude',side=2,cex=1,line=1.25)
degAxis(1,at=seq(60,95,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(2,at=seq(-70,-50,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(3,at=seq(60,95,by=5),labels=rep("",length(seq(60,95,by=5))),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(4,at=seq(-70,-50,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)

if (0){
ff2 <- !is.na(gv_bot$gv)
arrows(gv_bot$midlon[ff], gv_bot$midlat[ff], 
  gv_bot$p2lon[ff], gv_bot$p2lat[ff], length = 0.04,col="blue",angle=45,code=2,lwd=5)
}
ff <- !is.na(gv_1500$gv)
arrows(gv_1500$midlon[ff], gv_1500$midlat[ff], 
  gv_1500$p2lon[ff], gv_1500$p2lat[ff], length = 0.04,col="blue",angle=45,code=2,lwd=3)


# legend
leg_gv <- c(60,-69,0.03, destPointRhumb(c(60,-69), 90, d*0.02)) # bit exaggerated at 70S? cf in map domain
text(60,-68.5,"GV (CTD pairs)",adj=0,cex=cx)
arrows(leg_gv[1], leg_gv[2], 
  leg_gv[4], leg_gv[5], length = 0.04,col="blue",angle=45,code=2,lwd=3)
text(62,-69,expression(paste("3 cm ", s^-1, sep="")),adj=0,cex=cx)

text(60,-70,"GV (satellite)",adj=0,cex=cx)
arrows(61, -70.5, # satellite
  61 + 0.03 * scale * 1.2, -70.5, length = 0.04,col="black",angle=15,code=2)
text(62,-70.5,expression(paste("3 cm ", s^-1, sep="")),adj=0,cex=cx)

if (save_plot) {
  dev.off()
}
########################################
# end figure
