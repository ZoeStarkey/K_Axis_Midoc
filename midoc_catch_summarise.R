	# midoc_catch_summarise.R
	# script to create biomass summaries of midoc catch data at sea

	library(readxl)
	library(ggplot2)
	library(dplyr)
	library(SGAT)
	library(tidyr)
	library(lubridate)

	# import from excel
		# directory with data
		the.dir <- "./excel source data/"
		# latest excel workbook
		the.wb <- "k_axis_IYGPT_field_data_8Nov2017.xlsx"
		f <- paste0(the.dir, the.wb)
		
		# read
			# sample data
			SD <- read_excel(f, sheet = "sample data", col_types="text")

			# cod end data
			CD <- read_excel(f, sheet = "codend data", col_types="text")

			# station data
			# TODO: replace this with new data
			stn <- readr::read_csv("./excel source data/midoc_stations_locations.csv")

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
		cew<- read.csv("./excel source data/codend.wts.csv")
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

# biomass plots
	# first cut with ggplot
	# total biomass
	p.d<- bm.dat[bm.dat$cod.end%in%"front.of.net"==F & bm.dat$midoc.stn%in%c("TRIAL","2","8","10","33")==F ,]
	ggplot(data=p.d, aes(x=cod.end, weight=bm, fill=tax.grp, facets=midoc.stn), geom="bar", ylab="biomass") +
	geom_bar() +
	theme_bw() +
	facet_wrap(~midoc.stn, scales = "free_y")
	ggsave("biomass_by_coarse_taxonomic_grp190216.pdf")

	# proportions
	ggplot(data=p.d, aes(x=cod.end, weight=pbm, fill=tax.grp, facets=midoc.stn), geom="bar", ylab="% biomass") +
	geom_bar() +
	theme_bw() +
	facet_wrap(~midoc.stn)
	ggsave("proportion_biomass_by_coarse_taxonomic_grp190216.pdf")

# within fish, relative dominance of groups
	fd <- SD %>% filter(tax.group %in% "fish") %>% group_by(midoc.stn,cod.end) %>% mutate(tbm=sum(wt.g, na.rm=T)) %>%
	group_by(midoc.stn,cod.end,fish.grp) %>%
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

####
#
# Basic summary info
#
####

# add day/night crepuscular
		# location for each station (start if present, end if not)
		stn$lon<- as.numeric(stn$lon.start)
		stn$lat<- as.numeric(stn$lat.start)
		stn$lon[is.na(stn$lon)]<- as.numeric(stn$lon.end[is.na(stn$lon)])
		stn$lat[is.na(stn$lat)]<- as.numeric(stn$lat.end[is.na(stn$lat)])

		# do as spanning sunrise/sunset
		# compare results of this method with shots for which d/n/c was recorded
		# for calculations use t.midoc.in and t.midoc.out
		stn$t.start <- as.POSIXct(gsub("'","", stn$t.MIDOC.in), format="%d/%m/%y %H:%M",tz="utc")
		stn$t.end   <- as.POSIXct(gsub("'","", stn$t.midoc.out), format="%d/%m/%y %H:%M",tz="utc")

		ss<- stn[,c(3,25:30)] # reduced version of station, only with relevant columns

		# for midoc 32, the only recorded time is when the midoc was switched on. 
		# for midoc 30, no end times recorded
		# as a stop-gap, assume that MIDOC was turned on 15 min before deployment, and reached the surface some time later for midoc 32
		# for both 30 and 32, add median duration time 
		# median duration between start and end:
		#summary(as.duration(ss$shot.time)) #  244 minutes
		ss[ss$midoc.stn=="MIDOC32",]$t.start<- as.POSIXct("12/02/16 7:37", format="%d/%m/%y %H:%M",tz="utc")
		ss[ss$midoc.stn=="MIDOC32",]$t.end<- ss[ss$midoc.stn=="MIDOC32",]$t.start + minutes(244)
		ss[ss$midoc.stn=="MIDOC30",]$t.end<- ss[ss$midoc.stn=="MIDOC30",]$t.start + minutes(244)

		# can use lubridate interval to see if sunrise or sunset were within the period
		ss$shot.time<- as.interval(ss$t.start, ss$t.end)
		
		# do this as: sunrise %within% shot.time
		# but first need to check how many shots spanned midnight UTC
		ss$start.day<- day(ss$t.start)
		ss$end.day<- day(ss$t.end)
		ss$spans.midnight<- ss$end.day-ss$start.day
		ss$spans.midnight # 4 stations span midnight UTC - double check conversions for these cases

		ss$start.sunset  <- sunset(ss$t.start, ss$lon, ss$lat); tz(ss$start.sunset)<- "UTC"
		ss$start.sunrise <- sunrise(ss$t.start, ss$lon, ss$lat); tz(ss$start.sunrise)<- "UTC"
		ss$end.sunrise <- sunrise(ss$t.end, ss$lon, ss$lat); tz(ss$end.sunrise)<- "UTC"
		ss$end.sunset  <- sunset(ss$t.end, ss$lon, ss$lat); tz(ss$end.sunset)<- "UTC"

		# for some reason, time calculations are throwing errors, saying tzone attributes are inconsistent
		# force everything to POSIXct tz UTC
		ss$t.start<- as.POSIXct(ss$t.start, tz="UTC")
		ss$t.end<- as.POSIXct(ss$t.end, tz="UTC")
		ss$start.sunrise<- as.POSIXct(ss$start.sunrise, tz="UTC")
		ss$start.sunset<- as.POSIXct(ss$start.sunset, tz="UTC")
		ss$end.sunrise<- as.POSIXct(ss$end.sunrise, tz="UTC")
		ss$end.sunset<- as.POSIXct(ss$end.sunset, tz="UTC")

		ss$DNC<- NA
		
		ss[(ss$t.start < ss$start.sunset) & (ss$t.end < ss$end.sunset),]$DNC <- "D"
		ss[(ss$t.start > ss$start.sunrise) & (ss$t.start > ss$start.sunset) & (ss$t.end < ss$end.sunset),]$DNC<- "D"
		ss[(ss$t.start >ss$start.sunset) & (ss$t.start < ss$start.sunrise),]$DNC <- "N"

		ss[ss$start.sunrise %within% ss$shot.time,]$DNC<- "MC"
		ss[ss$start.sunset %within% ss$shot.time,]$DNC<- "NC"
		## TODO: at some stage could do a plot to show day/night cycle with deployments

# total biomass of each group
	# all together
	SD.calc.dat<- SD[SD$cod.end%in%c("front.of.net","1")==F & SD$midoc.stn%in%"TRIAL"==F & SD$include.in.total ,]
	SD.calc.dat<- SD.calc.dat[SD.calc.dat$cod.end%in%"front.of.net"==F & SD.calc.dat$midoc.stn%in%c("TRIAL","MIDOC02","MIDOC08","MIDOC33")==F ,]
	# for initial biomass summaries, not worth including stations below for following reasons
	# TRIAL - trial station, not part of survey
	# 2 	- winch and scanmar problems, net not likely to have flown properly, and may have gone to 1500m
	# 8 	- MIDOC not set properly so cod-ends did not trip; all catch in cod-end 1 other than small amout in CE2.
	# 33 	- net tangled, redeployed as 34

	SD.bm.tots<- ddply(SD.calc.dat, .(midoc.stn,tax.grp), summarise, bm.grand.tot=sum(wt.g, na.rm=T)/1000)
	
	#fish only, by station
	fg.bm.tots<- ddply(subset(SD.calc.dat, tax.grp=="fish"), .(fish.grp, midoc.stn), summarise, bm.grand.tot=sum(wt.g, na.rm=T)/1000)
	fg.bm.tots.wide <- spread(fg.bm.tots, key=fish.grp, value=bm.grand.tot)

	# fish only, split by codend
	#fish only, by station
	fg.bm.CE.tots<- ddply(subset(SD.calc.dat, tax.grp=="fish"), .(fish.grp, midoc.stn, cod.end), summarise, bm.grand.tot=sum(wt.g, na.rm=T)/1000)
	fg.bm.CE.tots.wide <- spread(fg.bm.CE.tots, key=fish.grp, value=bm.grand.tot)

	# all fish totals
	f.tots<- ddply(subset(SD.calc.dat, tax.grp=="fish"), .(midoc.stn), summarise, bm.grand.tot=sum(wt.g, na.rm=T)/1000)
	
	# split by codend
	f.CE.tots <- ddply(subset(SD.calc.dat, tax.grp=="fish"), .(midoc.stn, cod.end), summarise, bm.grand.tot=sum(wt.g, na.rm=T)/1000)
	# totals from codends
	# CD.calc.data<- CD[CD$midoc.station.id%in%"TRIAL"==F,]
	# ddply(CD.calc.data, .(cod.end), summarise, ce.bm.grand.tot=sum(as.numeric(catch.wt.tot), na.rm=T))

	

	# merge with biomass summaries
	# TODO: want this to have crepuscular classification!
	stn.bm.tots <- merge(ss[c("midoc.stn","lat","lon","DNC")], fg.bm.tots) 
	bm.tots.CE  <- merge(ss[c("midoc.stn","lat","lon","DNC")], fg.bm.CE.tots)
	f.tots 		<- merge(ss[c("midoc.stn","lat","lon","DNC")], f.tots)
	f.grps.tots <- merge(ss[c("midoc.stn","lat","lon","DNC")], fg.bm.CE.tots)
	f.CE.tots 	<- merge(ss[c("midoc.stn","lat","lon","DNC")], f.CE.tots)
	stn.tax.bm.tots<- merge(ss[c("midoc.stn","lat","lon","DNC")], SD.bm.tots) 

	write.csv(stn.bm.tots, "stn.bm.tots.csv")		
	write.csv(bm.tots.CE, "bm.tots.CE.csv")	
	write.csv(f.tots, "f.tots.csv")		
	write.csv(f.CE.tots, "f.CE.tots.csv")	
	write.csv(stn.tax.bm.tots, "stn.tax.bm.tots.csv")	

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

	# outliers with a lot more biomass for cod-end totals than summed samples (these are likely to be errors)


	# next plot - reproduce this in base; with following additions
		# full bars showing codend wet-weight, behind morphospecies bars
		# background colour coded by time of day
		# for now, use "Set3" colours from RColorBrewer
		col.key <- data.frame(tax.grp= na.omit(unique(p.d$tax.grp)), col=brewer.pal(11, "Set3"))

# details for report:
	# towing speed during deployment and retrieval from voyage logs
	# average net opening (height and width) from scanmar
	# ? average accuracy in achieving target depths
	# number of day/night/crepuscular shots


# get lengths from photos then do xy plots by species for lengths vs weights: visually assess outliers 
	