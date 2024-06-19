# hacking Sven's code to make a pretty sonogram for DOTS manuscript
# 

libs <- c("ggplot2","scales","viridis","dichromat","ggpubr","mapproj", "SGAT")
#Check if libraries exist, if not install them
apply(matrix(libs),1,function(x) if (!x %in% installed.packages()){install.packages(x)})
#require libraries
lapply(libs, require, character.only = TRUE)

base_dir <- "~/GitHub/DOTS/DOTS_sonogram_data/k-axis/"
outdir <- paste0(base_dir, "R/output/")
highres <- readRDS(paste0(outdir, "int38.RDS"))

bmdat <- readRDS("~/GitHub/K_axis_midoc/DSRII_station_strata_biomass_abundance_data.RDS")
env   <- readRDS("~/GitHub/K_axis_midoc/derived data/midoc_stations_envdata.rda")

env$T1<- ifelse(env$midoc.n%in%c(1:6),T,F)
env$T2<- ifelse(env$midoc.n%in%c(7:14),T,F)
env$T3<- ifelse(env$midoc.n%in%c(15:23),T,F)
env$T4<- ifelse(env$midoc.n%in%c(23:27),T,F)
env$T5<- ifelse(env$midoc.n%in%c(27:31),T,F)
env$T6<- ifelse(env$midoc.n%in%c(31:36),T,F)
env$T7<- ifelse(env$midoc.n%in%c(36:40),T,F)

setwd(basedir)
highres$datetime <- as.POSIXct(as.character(paste(highres$Date_M,highres$Time_M)),
                               format = "%Y%m%d %H:%M:%S", 
                               tz="UTC")

palsel <- function(plot,pal, lims){
  if(pal=="viridis"){
    plot <- plot + 
      scale_fill_viridis(limits=lims, 
                         na.value="transparent", 
                         oob = scales::squish, 
                         name="Sv [dB]")
  }else if(pal=="bto"){
    #Custom colour scheme
    BlueToOrange <- dichromat_pal("BluetoOrange.12")(12)
    bto <- colorRampPalette(BlueToOrange)
    
    plot <- plot + scale_fill_gradientn(colors= bto(8),
                                        name="Sv [dB]",
                                        na.value = "transparent",
                                        limits=lims,oob=scales::squish)
  }
  plot <- plot + theme(panel.background=element_rect(fill="white"),
                       legend.position = "top",
                       axis.text.x = element_text(size=12, angle=75,hjust=1),
                       axis.text.y = element_text(size=14, hjust=1),
                       axis.title = element_text(size=16))
  return(plot)
}

subecho <- function(sono.data=highres, bmdata, start_date, end_date,inclusive=TRUE,timezone="UTC", hourly=TRUE ,lims=c(-100,-70), interpolate=FALSE, palette = "bto", sumfun="median"){
  
  dateformats <- c("%d-%m-%Y %H:%M:%OS",
                   "%d/%m/%Y %H:%M:%OS",
                   "%d-%m-%Y %H:%M",
                   "%d/%m/%Y %H:%M",
                   "%d-%m-%Y",
                   "%d/%m/%Y",
                   "%d%m%Y",
                   "%Y-%m-%d %H:%M:%OS",
                   "%Y/%m/%d %H:%M:%OS",
                   "%Y-%m-%d %H:%M",
                   "%Y/%m/%d %H:%M",
                   "%Y-%m-%d",
                   "%Y/%m/%d",
                   "%Y%m%d")
  
  start_date <- as.POSIXct(start_date, tz=timezone,
                           tryFormats = dateformats)
  end_date <- as.POSIXct(end_date, tz=timezone, 
                         tryFormats = dateformats)
  
  if(inclusive==TRUE){
    end_date <- end_date + 3600*24
  }   

  days <- seq(as.Date(start_date),as.Date(end_date),"day")
  
  #Remove points with missing GPS
  sono.data<-sono.data[which(sono.data$Lon_M<360 & sono.data$Lat_M<360),]
  
  #create datetime object in original datset
  sono.data$datetime <- as.POSIXct(as.character(paste(sono.data$Date_M,
                                                 sono.data$Time_M)),
                              format = "%Y%m%d %H:%M:%S",
                              tz="UTC")
  #Subset by date  
  subhigh <- subset(sono.data, datetime >= start_date & datetime <= end_date)
  
  #Set -999 to NA
  subhigh$Sv_mean[subhigh$Sv_mean < -200] <- NA
  
  if(hourly==TRUE){
    subhigh <- aggregate(sono.data= subhigh, cbind(NASC,
                                              Sv_mean,
                                              Lon_M,
                                              Lat_M,
                                              datetime,
                                              Date_M
    ) ~ format(subhigh$datetime, "%d/%m/%Y %H")+Layer_depth_min,
    FUN = mean, na.action=na.omit)
    subhigh$datetime<-as.POSIXct(subhigh$datetime, origin = "1970-01-01", tz="UTC") 
    names(subhigh)[1]<- "hfactor"
  }
  #Create label
  hours <- as.numeric(format(subhigh$datetime, "%H"))
  hours[which(diff(as.matrix(hours))==0)+1] <- ""
  #datsub <- subhigh$Date_M
  #datsub[which(diff(as.matrix(datsub))==0)+1]<- ""
  #labels = paste(datsub, hours)
  # subset to only even hours
  labels<- hours
  labels[nchar(labels)<5 & as.numeric(labels)%%2!=0]  <- ""
  
  #Sv raster
  if(hourly==FALSE){
    pp<-ggplot(sono.data=subhigh, aes(x=factor(datetime),
                                 y=-Layer_depth_min, 
                                 fill=Sv_mean))+
      scale_x_discrete(expand=c(0,0),
                       label=labels,
                       breaks=factor(subhigh$datetime))
  }else{
    pp <- ggplot(sono.data=subhigh, aes(x=factor(hfactor),
                                   y=-Layer_depth_min, 
                                   fill=Sv_mean))+
      scale_x_discrete(expand=c(0,0),
                       label=labels,
                       breaks=factor(subhigh$hfactor))
  }
  
  pp<-pp+
    scale_y_continuous(expand=c(0,0))+
    geom_raster(interpolate = interpolate)+
    ylab("Depth [m]")+
    xlab("Hour of the day [UTC]")
  
  # sunrises and sunsets - jiggery pokery to convert to factor axis
  # automating this is tricky. A hack is to just manually add at the correct hour
  # for these sites, sunrise is at ~2300 (2245) both days and sunset is at 1500 (1445)
  pp <- pp + geom_vline(xintercept = c(22.7,22.75+24,22.75+48), col="green", lwd=1.5) + geom_vline(xintercept=c(14.75, 14.75+24,14.75+48), col="mediumpurple2", lwd=1.5)
  
  
  #ppadjust the theme and palette
  pp <- palsel(pp,palette,lims=lims)
  
  print(pp)
  pp
}


# Transect 1

# Transect 4
pp<- subecho(start_date = "07/02/2016", end_date="09/02/2016", interpolate = TRUE, hourly=TRUE, lims = c(-100,-70))


