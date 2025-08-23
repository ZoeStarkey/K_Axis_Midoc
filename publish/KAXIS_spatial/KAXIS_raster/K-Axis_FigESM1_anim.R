library(animation)
library(RColorBrewer)
library(raster)

source("C:/Users/bes049/BESTLEY/scripts/scripts_ssf/map_Ant.R")
#bathym2 <- bathym
#bathym2[bathym2 < -3000] <-NA_real_

ryb <- colorRampPalette(rev(brewer.pal(11,"RdYlBu")))
cols1 <- ryb(56); cols1 <- cols1[c(1:22, 24:56)] # remove very pale colours in middle?

#####################################################################
# OK everything set up above ... try doing the regular KAXIS plot now with this inset??
# voyage track waypoints
wfile <- read.csv("C:/Users/bes049/BESTLEY/DATA/KAXIS/2016_Analysis/v3_201516030_waypoints_dec.csv",header=TRUE)
colnames(wfile) <-c("waypt","lat","lon","grp")
wfile <- wfile[-which(wfile$lon > 96),]
# where exactly were the ctd stations??
ctdstns <- read.table('C:/Users/bes049/BESTLEY/DATA/KAXIS/2016_Analysis/stn_loc.txt')
ctddate <- as.POSIXlt(as.Date(ctdstns[,ncol(ctdstns)],origin="1970-01-01") - 719529,tz="UTC")

# KAXIS frontal vectors
dts <- as.POSIXlt(seq(as.Date("2016-01-18"), as.Date("2016-02-18"), by = "1 day"), tz = "UTC")
bx <- c(60, 95, -70, -51)
cx <- 0.8;

# SST, ice, ssh etc. stored for 18 Jan - 18 Feb (download from CLIOTOP VM)
load("C:/Users/bes049/BESTLEY/scripts/MATLAB/KAXIS/data/KAXIS_anim_data.RData")
SST <- sst
SSH <- ssh
SSHa <- ssha

load("C:/Users/bes049/BESTLEY/scripts/MATLAB/KAXIS/data/KAXIS_anim_GHRSST.RData")
sst <- sstGHR
rm(sstGHR)

iceLL <- crop(ice, extent(2e06,3.25e06,-1e06,1.75e06))
iceLL[iceLL == 0] <- NA_real_
rm(ice)


# current info
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/sophie_raster/KAXIS_curr_big.RData")
crds <- coordinates(mag)
xx1 <- crds[,1]
yy1 <- crds[,2]

SSHmin <- -1.3; ssh[ssh<SSHmin] <- SSHmin; SSHmax <- 0; ssh[ssh>SSHmax] <- SSHmax; 

sshmax <- 0.3; ssha[ssha>sshmax] <- sshmax; sshmin <- -0.1; ssha[ssha<sshmin] <- sshmin
tag <- c("SSH","SSHa","SST")
which_plots <- 1:length(tag)

# if using saveVideo
#setwd("C:/Users/bes049/BESTLEY/scripts/MATLAB/KAXIS/output/figures/MS/BESTLEY_MS/ESM3/video/")
#saveVideo({
# the image quality declines badly over the slide sequence??

# if using saveHTML
#setwd("C:/Users/bes049/BESTLEY/scripts/MATLAB/KAXIS/output/figures/MS/BESTLEY_MS/SuppInfo/ESM3")

for (plot_opt in which_plots) { # generate each in turn
setwd(paste("C:/Users/bes049/BESTLEY/scripts/MATLAB/KAXIS/output/figures/MS/BESTLEY_MS/reviewed/Submission/reviews/ESM/ESM",plot_opt,sep=""))

saveHTML({

for (idate in 1:length(dts)) { #length(dts)
  print(paste(idate,"of",length(dts)))
  thisdate <- dts[idate]
  par(new=FALSE,mar=c(2,1.25,1.5,5.75), oma=c(1,1,1,0), family="serif")

  plot(extent(bx),bty="n",col="white",ylab="",xlab="" ,mgp=c(3,0.25,0),cex=0.5,axes=F)
  mtext(paste(tag[plot_opt], as.Date(thisdate)),side=3,line=1.25,cex=1.25,font=2)
  
  # Here choose background
  tmp <- sst[[idate]]; tmp <- tmp-273.15
  #...................................
  if(tag[plot_opt]=="SST") {# SST
    sstmax <- 5; tmp[tmp>sstmax] <- sstmax;  sstmin <- -1.5; tmp[tmp<sstmin] <- sstmin
    plot(tmp,col = cols1,add=TRUE, alpha = 0.8,legend = TRUE,zlim=c(sstmin, sstmax),
      legend.args=list(text=expression(SST~(degree*C)), side=3, font=2, line=1, cex=0.8,adj=0),
     axis.args=list(cex.axis=0.8))  
  }
  if(tag[plot_opt]=="SSHa") {# SSHa
    plot(ssha[[idate]],col = cols1,add=TRUE, alpha = 0.8,legend = TRUE,zlim=c(sshmin, sshmax),
          legend.args=list(text='SSHa (m)', side=3, font=2, line=1, cex=0.8,adj=0),
     axis.args=list(cex.axis=0.8))#palr::chlPal(56)
 }
  if(tag[plot_opt]=="SSH") {# SSHa
    plot(ssh[[idate]],col = cols1,add=TRUE, alpha = 0.8,legend = TRUE,zlim=c(SSHmin, SSHmax),
              legend.args=list(text='SSH (m)', side=3, font=2, line=1, cex=0.8,adj=0),
     axis.args=list(cex.axis=0.8))#palr::chlPal(56)
#palr::chlPal(56)
 }

#...................................
# Sea Ice - needs reproj ***
     tmp_iceLL <- projectRaster(iceLL[[idate]],tmp) # big, do this  once only
plot(tmp_iceLL,col=gray.colors(56,start=0.1,end=0.9),add=TRUE,legend=FALSE,zlim=c(0,100))
#...................................
# voyage track waypoints
lines(wfile$lon,wfile$lat,col="gray30",lwd=2)
points(ctdstns[ctddate>thisdate,1],ctdstns[ctddate>thisdate,2],col="black",cex=1.5,pch=1,lwd=2)
points(ctdstns[ctddate<thisdate,1],ctdstns[ctddate<thisdate,2],col="green",cex=1.5,pch=1,lwd=2)
points(ctdstns[ctddate==thisdate,1],ctdstns[ctddate==thisdate,2],col="red",cex=1.5,pch=16)

#plot(ice2,col = palr::bathyDeepPal(56),add=TRUE,legend=TRUE,zlim=c(0,100),#alpha=0.8,
#  legend.args=list(text='Ice (%)', side=3, font=2, line=1, cex=0.8,adj=0),
#     axis.args=list(cex.axis=0.8)  )
#...................................
# Continent and voyage context
# Add some bathymetry contours, plus the sea uce and ice shelves, and the voyage track etc
contour(bathym, levels=c(-2000),add=TRUE,drawlabels=FALSE,col="gray50",lwd=2)
add_coast()
add_shelves()
box()
mtext('Longitude',side=1,cex=1,line=1.5)
mtext('Latitude',side=2,cex=1,line=1.5)
degAxis(1,at=seq(60,95,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(3,at=seq(60,95,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(2,at=seq(-70,-50,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)
degAxis(4,at=seq(-70,-50,by=5),cex.axis=cx,mgp=c(1,0.25,0),tcl=-0.25)

#...................................
# CURRENT vectors - FIX this***
scale <-  3
#arrows(df$x, df$y, df$x + scl_ll(df$u, df$v ), df$y + scl_ll(df$v, df$v), length=0.015,angle=15,col="black",lwd=1)
xx2 <- crds[,1] + values(u[[idate]]) * scale * 1.2 # axis distances are slightly non-equivalent
# 2113 km lat v 1745 lon

b<-1;
arrows(xx1[seq(1,length(xx1),by=b)], yy1[seq(1,length(xx1),by=b)], # subsample by 2s
  xx2[seq(1,length(xx1),by=b)], yy1[seq(1,length(xx1),by=b)], length = 0.05,col="black",angle=10,code=2)

# SSH contours
contour(ssh[[idate]],lev=c(-0.65),add=TRUE,col="red",drawlabels=FALSE,lwd=2)
contour(ssh[[idate]],lev=c(-0.7),add=TRUE,col="magenta",drawlabels=FALSE,lwd=2)
contour(ssh[[idate]],lev=c(-0.8),add=TRUE,col="magenta3",drawlabels=FALSE,lwd=2)
contour(ssh[[idate]],lev=c(-0.9),add=TRUE,col="purple",drawlabels=FALSE,lwd=2) #darkmagenta
contour(ssh[[idate]],lev=c(-1.0),add=TRUE,col="seagreen1",drawlabels=FALSE,lwd=2) # cyan3
contour(ssh[[idate]],lev=c(-1.1),add=TRUE,col="blue2",drawlabels=FALSE,lwd=2)
contour(ssh[[idate]],lev=c(-1.15),add=TRUE,col="blue4",drawlabels=FALSE,lwd=2)
contour(ssh[[idate]],lev=c(-1.2),add=TRUE,col="purple4",drawlabels=FALSE,lwd=2)
contour(ssh[[idate]],lev=c(-1.25),add=TRUE,col="tomato4",drawlabels=FALSE,lwd=2)

arrows(61, -69, # satellite
  61 + 0.05 * scale * 1.2, -69, length = 0.05,col="black",angle=10,code=2)
text(62,-69,expression(paste("5 cm ", s^-1, sep="")),adj=0,cex=cx)
arrows(61, -69.5, # satellite
  61 + 0.15 * scale * 1.2, -69.5, length = 0.05,col="black",angle=15,code=2)
text(62,-69.5,expression(paste("15 cm ", s^-1, sep="")),adj=0,cex=cx)
arrows(60.5, -70, # satellite
  60.5 + 0.5 * scale * 1.2, -70, length = 0.05,col="black",angle=10,code=2)
text(62.5,-70,expression(paste("50 cm ", s^-1, sep="")),adj=0,cex=cx)

# LEGEND
text(85,-67.85,"SSH contours (m)",font=2,adj=0,cex=1)
legend(81,-68, title="",title.adj=1,
  legend=c("-0.65","-0.70","-0.80","-0.90","-1.00"),
  #col=c(plotcols[5],"paleturquoise1",plotcols[4],plotcols[3],"purple",plotcols[2]),
  col=c("red","magenta","magenta3","purple","seagreen1"),
  lwd=2, cex=1,lty=1,bty="n",pt.cex=0.5)
legend(88,-68, title="",title.adj=1,
  legend=c("-1.10","-1.15","-1.20","-1.25"),
  #col=c(plotcols[5],"paleturquoise1",plotcols[4],plotcols[3],"purple",plotcols[2]),
  col=c("blue2","blue4","purple4","tomato4"),
  lwd=2, cex=1,lty=1,bty="n",pt.cex=0.5)

#}, video.name = paste("A1Video_KAXIS_", tag,".avi",sep=""), #other.opts = "-vcodec libx264 -crf 15 -pix_fmt yuv420p -vframes 32",#img.name = "Rplot", 
#    ffmpeg = 'C:/Program Files (x86)/ffmpeg-20171218-74f408c-win64-static/bin/ffmpeg.exe',
#   ani.width = 480*1.5, ani.height = 480*1.5, res=600)
} # idate loop
}, ani.width = 480*1.75, ani.height = 480*1.75, ani.dev="png",ani.type="png",imgdir="images",res=300,
htmlfile =paste("A",plot_opt,"Video_KAXIS_",tag[plot_opt],".html",sep="") )

} # plot loop
