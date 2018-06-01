# 02_import_catch.R
# 30 May 2018
#
# Updated script for reading and checking K-axis catch data. Based on superseded script midoc_catch_summarise.R

# it produces the files, which should be used for all subsequent analyses:
  # codend_fish_biomass.rds
  # codend_taxa_biomass.rds

## TODO finish this:
# cod.end depths and volumes

# for converting to biomass per unit area
# checks in '01_import_net_logger_and_voyage_track.R' indicated that precise calculations of volumes swept wouldn't be possible, so assume 3.5 kt to calculate volumes

# 3.5 knots = 1.80056 m/s; mouth of net is nominally 180 m2
# so calculation will be 188 m^2 * 1.80056 m/s * time s to give swept volumes in m3
# durations for cod-end 6 are often somewhat longer as the net was at the surface for a while; but not fishing - manually set these to 30 min

# There was a switch from 100 min for CE1 to 90 min at MIDOC 14 (miodc 12 was 100 min; MIDOC 14 onward were 90 min )
	# Apart from exceptions (midoc 10, 13, ) volumes will be:
	# CE2--6: 188 m^2 * 1.80056 m/s * 1800 s = 609309.5
	# CE1 (stations 01 -- 12) : 188 m^2 * 1.80056 m/s * 6000 s = 2031032
	# CE1 (stations 14 -- 40) : 1827929
# ** this is conservative for CE1, as the ship typically went faster at the beginning of deployments while net was flying down **

# exclude midoc 33 - twisted bridle

function(x){
	ce1vol100 <- 2031032
	ce1vol90 <- 1827929
	ce2_6vol <- 609309.5

	x$depth_mid_m <- NA
	x$swept_m3 <- NA
	# stations 1:12; CE1s have swept vol for 100 min at 3.5 kt
	x[as.numeric(substr(x$midoc.stn,6,7))%in%c(1:12) & x$cod.end=="1",]$swept_m3 <- ce1vol100
	# stations 14:40; CE1s have swept vol for 90 min at 3.5 kt
	x[as.numeric(substr(x$midoc.stn,6,7))%in%c(14:40) & x$cod.end=="1",]$swept_m3 <- ce1vol90
	x[x$cod.end=="1",]

	# CE 2--6 have 30 min at 3.5 kt; other than exceptions 
		# MIDOC09 and 12: CE 5 & 6 combined ==> depth.mid = 200; time = 60 min
		x[x$cod.end%in%as.character(c(2:6)) & ((midoc.stn%in% c("MIDOC08","MIDOC09","MIDOC12")) == F),]$swept_m3 <- ce2_6vol
		x[x$cod.end%in%as.character(c(2:4)) & midoc.stn%in% c("MIDOC09","MIDOC12"),]$swept_m3 <- ce2_6vol
		x[x$cod.end== "5" & midoc.stn%in% c("MIDOC09","MIDOC12"),]$swept_m3 <- ce2_6vol*2

	# mid depths are

	x[x$midoc.stn=="MIDOC08",]$swept_m3 <- ce1vol100 + 5*ce2_6vol
	x[x$cod.end== "5" & midoc.stn%in% c("MIDOC09","MIDOC12"),]$depth.mid <- 200
	x[x$midoc.stn=="MIDOC08",]$depth_mid_m <- 500


	

	# MIDOC 2: cod ends 2 and 3 were both at 1200 - 1000 m, 4 was 900 - 600, 5 was 600 - 200, 6 was usual 200 - surface
	x[x$midoc.stn=="MIDOC02" & as.character(x$cod.end)%in%c("2","3"),]$depth <- 1100
	x 
	

	


	x$g_per_m3 <- x$biomass.g/x$swept_m3

	})
}


library("tidyverse")
library(lubridate)
library(readxl)
Sys.setenv(TZ='GMT')

#f <- "/Users/rowan/GitHub/K_axis_midoc/derived data"
f <- "/Users/dougt/GitHub/K_axis_midoc/derived data"
setwd(f)


mdd <- readRDS("midoc_logger_checked.rds")
ce.se <- readRDS("midoc_cod_ends_checked.rds")
ktr <- read_csv("../source data/v3_201516030_waypoints_dec.csv")
nav <- readRDS("nav_reduced.rds")

# import from excel
		# directory with data
		#the.dir <- "/Users/rowan/GitHub/K_axis_midoc/source data/"
		the.dir <- "/Users/dougt/GitHub/K_axis_midoc/source data/"

		# latest excel workbook
		the.wb <- "k_axis_IYGPT_field_data_8Nov2017.xlsx"
		f <- paste0(the.dir, the.wb)
		
		# read
			# sample data
			SD <- read_excel(f, sheet = "sample data", col_types="text")

			# cod end data
			CD <- read_excel(f, sheet = "codend data", col_types="text")

			# station data
			mds <- readRDS("midoc_stations_checked.rds") 

			# voyage data 
			nav <- readRDS("nav_reduced.rds")

			# taxon key
			tk <- read_excel(f, sheet = "taxa groups")
			
			colnames(tk)<- c("orig.tax","tax.grp1","tax.grp2")

			# check if taxon key needs to be updated
			# strip white spaces from
			tx <- unique(SD$taxon)
			tx[tx%in%tk$orig.tax==F]			


	# initial processing
		# new taxon column (based on lookup table from unique values in taxon column)
			SD$tax.grp  <- tk$tax.grp1[match(SD$taxon, tk$orig.tax)]
			SD$fish.grp <- tk$tax.grp2[match(SD$taxon, tk$orig.tax)] 

		# give 1 if empty to include.in.total
			SD$include.in.total<- as.numeric(SD$include.in.total)
			SD$include.in.total[is.na(SD$include.in.total)] <- 1
		# give n. individ 1 if empty
			SD$n.individuals[is.na(SD$n.individuals)] <- 1

		# give all <1g weights 0.4 g
			SD$wt.g[SD$wt.g%in%c("<1", ">1")] <- 0.4

		# convert all kg weights to g
		# and exclude biomass for trays from calculations (from early shots where trays rather than discards were weighed)
			SD$wt.g[is.na(SD$wt.g)]<- as.numeric(SD[is.na(SD$wt.g),]$wt.kg)*1000
			SD$wt.g <- as.numeric(SD$wt.g)*SD$include.in.total
	
	# create full IDs for individuals that were just given numbers to speed data entry
			# these are rows 
		ind<- !(grepl("MIDOC", SD$sample.id))&(SD$midoc.stn!="TRIAL")&!(grepl("CE", SD$sample.id)) # index for relevant rows
	SD[ind,]$sample.id<- paste(SD[ind,]$midoc.stn,SD[ind,]$cod.end,SD[ind,]$sample.id, sep="_")

	# remove "_" between MIDOC and number in IDs so all are MICOCnn (not MIDOC_nn)
	SD$sample.id<- sub("(\\w+)\\_(\\d+\\_.+\\_.+)", "\\1\\2", SD$sample.id )	

	# should also use a regular expression here to standardise form of all sample IDs - currently some have the letters "CE" while others just have the number - standardise all to CE
	SD$sample.id<- sub("(\\w+\\_)(\\d+\\_)(.+)", paste0("\\1","CE","\\2\\3"), SD$sample.id )

	# finally, change "front.of.net_front.of.net" created by previous steps to "front.of.net"
	SD$sample.id<- sub("front.of.net_front.of.net", "front.of.net", SD$sample.id )	

	# check for duplicates
		tmp<- data.frame(table(SD$sample.id))
		tmp[tmp$Freq>1,]
		tmp[grepl(c("discard"), x=tmp$Var1, ignore.case = T)==F & grepl(c("tray"), x=tmp$Var1, ignore.case = T)==F & tmp$Freq>1,]
		# list of duplicate IDs that need to be resolved:
		      # MIDOC06_CE2_08 
		#       MIDOC06_CE4_14 
		#       MIDOC06_CE6_31 
		#       MIDOC07_CE2_23 
		#       MIDOC11_CE2_31 
		#       MIDOC12_CE2_38 
		#       MIDOC13_CE6_21 
		#       MIDOC13_CE6_22 
		#       MIDOC13_CE6_23 
		#       MIDOC13_CE6_24 
		#       MIDOC13_CE6_25 
		#       MIDOC14_CE3_32 
		#       MIDOC15_CE4_07 
		#       MIDOC15_CE4_09 
		#       MIDOC16_CE1_01 
		#       MIDOC16_CE3_30 
		#       MIDOC16_CE3_40 
		#       MIDOC16_CE6_24 
		#       MIDOC19_CE1_01 
		#       MIDOC19_CE2_24 
		#       MIDOC19_CE2_25 
		#       MIDOC19_CE2_26 
		#       MIDOC19_CE2_27 
		#       MIDOC19_CE2_28 
		#       MIDOC19_CE2_29 
		#       MIDOC19_CE2_30 
		#       MIDOC19_CE4_25 
		#    MIDOC20_CE1_no.ID 
		#       MIDOC20_CE6_42 
		#       MIDOC21_CE1_04 
		#       MIDOC21_CE1_05 
		#       MIDOC21_CE1_08 
		#      MIDOC22_CE1_BAG 
		#      MIDOC23_CE1_BAG 
		#       MIDOC23_CE3_37 
		#       MIDOC23_CE5_37 
		# MIDOC23_front.of.net 
		#       MIDOC24_CE4_25 
		#       MIDOC24_CE6_01 
		#       MIDOC24_CE6_02 
		# MIDOC24_front.of.net 
		#      MIDOC25_CE1_BAG 
		#       MIDOC25_CE5_42 
		#       MIDOC25_CE6_53 
		#       MIDOC26_CE3_35 
		#       MIDOC26_CE3_36 
		#       MIDOC26_CE5_22 
		#       MIDOC26_CE6_11 
		#       MIDOC26_CE6_45 
		#      MIDOC27_CE1_BAG 
		#       MIDOC27_CE6_45 
		#       MIDOC28_CE6_01 
		#       MIDOC29_CE2_35 
		#       MIDOC29_CE6_19 
		#       MIDOC29_CE6_32 
		#       MIDOC31_CE1_02 
		#       MIDOC31_CE2_27 
		#       MIDOC31_CE2_38 
		#       MIDOC31_CE5_37 
		#       MIDOC34_CE6_26 
		#       MIDOC34_CE6_38 
		#       MIDOC35_CE2_46 
		#       MIDOC37_CE2_48 
		#       MIDOC38_CE6_54 
		#       MIDOC39_CE6_15 
		#       MIDOC40_CE1_06 	 
		#
		# NEED TO LOOK AT THESE INDIVIDUALLY!!!! 
		#

	# adjust codend totals for weights of codends
		cew<- read.csv("../source data/codend.wts.csv")
		cew$cen.col <- paste(cew$cen, cew$col)
		CD$cen.col <- paste(CD$cod.end, CD$codend.col)
		CD$cew<- cew$wt[match(CD$cen.col, cew$cen.col)]
		CD[is.na(CD$catch.wt.tot),]$catch.wt.tot <- as.numeric(CD[is.na(CD$catch.wt.tot),]$catch.wt.raw) - CD[is.na(CD$catch.wt.tot),]$cew
#
# summarise biomass by site and depth stratum, within each taxonomic grouping
#		
	# exclude TRIAL from calculations
	SD <- SD %>% filter(midoc.stn%in%"TRIAL" == F)

	# as totals and proportions

	bm.codends <- SD %>% 	group_by(midoc.stn, cod.end) %>% 
			mutate(tbm=sum(wt.g, na.rm=T)) %>% # total biomass for each cod-end
			group_by(midoc.stn, cod.end , tax.grp) %>% # total biomass for each taxon in each cod end
			mutate(bm = sum(wt.g, na.rm=T)) %>%
			mutate(pbm = bm/tbm) %>%
			distinct(pbm, .keep_all = T) %>% # keeps unique values of pbm for each combination of station, cod.end, tax.grp; retains all columns
			select(midoc.stn, cod.end, tax.grp, fish.grp, tbm, bm, pbm)
			# not sure why slice(1) doesn't work here.

	saveRDS(bm.codends, "../derived data/codend_taxa_biomass.rds")

	# fish only
	fbm <- SD %>% filter(tax.grp %in% "fish") %>% group_by(midoc.stn,cod.end) %>% mutate(tbm=sum(wt.g, na.rm=T))
	saveRDS(fbm, "../derived data/codend_fish_biomass.rds")	


# within fish, relative dominance of groups
	fbm.dat<- fbm %>% group_by(midoc.stn,cod.end,fish.grp) %>%
	mutate(bm=sum(wt.g, na.rm=T)) %>%
	mutate(pbm = bm/tbm) %>%
	distinct(pbm, .keep_all = T) %>%
	select(midoc.stn, cod.end, tax.grp, fish.grp, tbm, bm, pbm)
	

	fp.d<- fbm.dat[fbm.dat$cod.end%in%c("front.of.net","1")==F & fbm.dat$midoc.stn%in%"TRIAL"==F ,]
	ggplot(data=fp.d, aes(x=cod.end, weight=bm, fill=fish.grp, facets=midoc.stn), geom="bar", ylab="biomass") +
	geom_bar() +
	theme_bw() +
	facet_wrap(~midoc.stn)
	ggsave("biomass_by_fish_grp090216.pdf")

	# proportions
	ggplot(data=fp.d, aes(x=cod.end, weight=pbm, fill=fish.grp, facets=midoc.stn), geom="bar", ylab="% biomass") +
	geom_bar() +
	theme_bw() +
	facet_wrap(~midoc.stn)
	ggsave("proportion_biomass_by_coarse_taxonomic_grp190216.pdf")


###### 
##
## incomplete below, additional checks, exploration and to-do
##	
######	

# quick diagnostic for biomass: total biomass (sum of sample data) vs. codend wet weights... what's the difference
	bm.tots<- ddply(bm.gd, .(midoc.stn,cod.end), summarise, bm=sum(bm))
	bm.tots$MCE <- paste0(bm.tots$midoc.stn, bm.tots$cod.end)
	CD$MCE<- paste0(CD$midoc.station.id, CD$cod.end)
	bm.tots$CE.tot<-  CD$catch.wt.tot[match(bm.tots$MCE, CD$MCE)]
	bm.tots$CE.tot<- as.numeric(bm.tots$CE.tot)*1000
	
	with(bm.tots[bm.tots$MCE!="MIDOC081",], plot(x=bm, y=CE.tot))
	with(bm.tots[bm.tots$MCE!="MIDOC081",], text(x=as.numeric(bm), y=as.numeric(CE.tot), labels=as.character(MCE))
	lines(0:350000, 0:350000)
	with(bm.tots[bm.tots$MCE!="MIDOC081",], hist(as.numeric(bm)/as.numeric(CE.tot)))
	bm.tots$prop<- bm.tots$bm/bm.tots$CE.tot
	summary(bm.tots) # most shots have comparible biomass totals from cod-end weights vs. sum of sample weights
	
	# outliers with more biomass in summed totals than cod-end totals (these are likely to be errors)
	bm.tots[bm.tots$prop>1,]
       # midoc.stn cod.end      bm      MCE CE.tot     prop
       	 # MIDOC02       4  8582.4 MIDOC024   4066 2.110772
       	 # MIDOC03       3  3475.2 MIDOC033   3455 1.005847
       	 # MIDOC03       5 29302.8 MIDOC035   5609 5.224247
       	 # MIDOC04       2  4519.4 MIDOC042   3799 1.189629
       	 # MIDOC04       3 50421.6 MIDOC043  12255 4.114370
       	 # MIDOC05       2 14184.4 MIDOC052  13000 1.091108
       	 # MIDOC07       2 23429.4 MIDOC072  21699 1.079746
       	 # MIDOC07       3  4216.0 MIDOC073   3755 1.122770
       	 # MIDOC09       2 10644.2 MIDOC092  10000 1.064420


	# next plot - reproduce this in base; with following additions
		# full bars showing codend wet-weight, behind morphospecies bars
		# background colour coded by time of day
		# for now, use "Set3" colours from RColorBrewer
		col.key <- data.frame(tax.grp= na.omit(unique(p.d$tax.grp)), col=brewer.pal(11, "Set3"))