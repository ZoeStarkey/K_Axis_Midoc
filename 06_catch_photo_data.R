# import data from catch photo measurements
# generate:
#	- presence/absence by site for taxonomic groups
#	- abundances of groups per cod-end
#	- size distributions

library(tidyverse)
library(readxl)
library(RColorBrewer)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/")
setwd(d)
		
# readMIDOC_fish_photo_measuring_9Oct2017.xlsx
	# sample data
	f <- "./source data/MIDOC_fish_photo_measuring_9Oct2017.xlsx"
	fl <- read_xlsx(f)
	colnames(fl)<- c("midoc.stn", "cod.end", "n.in.photo", "photo.label", "SL.mm", "sample.label", "taxon", "tax.group", "taxon.uncertain", "notes", "measured.by")

	# taxon lookup key
	tk <- read_xlsx(f, sheet = "taxon lookup")
			
# processing and checks
	# merge/simplify taxa into fish.groups
	fl$fgroup <- tk$fish_group[match(fl$taxon, tk$recorded)]

# # checking sizes
# hist(fl$SL.mm) # shows quite a few fish seem to have unrealistic sizes			
# fl$SL.mm <- as.numeric(fl$SL.mm)

# fl[fl$SL.mm>1000,] ## first cut, anything over 1m is wrong
# # from the following photos
# 	# KX16-MIDOC04_016.jpg
# 	# KX16-MIDOC05_051.jpg
# 	# KX16-MIDOC07_047.jpg
# 	# KX16-MIDOC09_020.jpg
# 	# KX16-MIDOC14_033.jpg
	


# fl[fl$SL.mm>300,] ##  electrona and gymno with impossible measurements
# # from the following photos
# 	# KX16-MIDOC15_004.jpg
# 	# KX16-MIDOC15_006.jpg

fl[fl$midoc.stn%in%c("33","34","35","36","37","38","39","40"),]$midoc.stn <- paste0("MIDOC",fl[fl$midoc.stn%in%c("33","34","35","36","37","38","39","40"),]$midoc.stn)

# presence/absence
# site-level

# list of species present at each site

# there are gaps that need to be filled for MIDOC 1 (cod-ends 2:6) and MIDOC 21 (cod-ends 2 & 3) as there were no photos for these
# do these first, so that they can be slotted in to the full dataset before expanding to complete records
		# latest excel workbook
		f2 <- paste0(d, "/source data/k_axis_IYGPT_field_data_5Jul2018.xlsx")
		SD <- read_xlsx(f2, sheet = 3)
		tk2 <- read_xlsx(f2, sheet = 5)
		colnames(tk2)<- c("orig.tax","tax.grp1","tax.grp2")
		# update taxon key cateories to match photo data
		tk2$tax.grp2<- gsub("Bathylagiids", "Bathylagidae",tk2$tax.grp2)
		tk2$tax.grp2<- gsub("cyclothone", "Gonostomatidae",tk2$tax.grp2)
		tk2$tax.grp2<- gsub("Cyclothone", "Gonostomatidae",tk2$tax.grp2)
		tk2$tax.grp2<- gsub("melamphid", "Melamphidae",tk2$tax.grp2)
		tk2$tax.grp2<- gsub("Macrurids", "Macrouridae",tk2$tax.grp2)
		tk2$tax.grp2<- gsub("Protomyctophum sp", "Protomyctophum",tk2$tax.grp2)
		tk2$tax.grp2<- gsub(" sp.", "",tk2$tax.grp2)
		tk2$tax.grp2<- gsub("Other myctophid", "",tk2$tax.grp2)

		SD$fgroup <- tk2$tax.grp2[match(SD$taxon, tk2$orig.tax)]

	pa.m <- SD %>% filter(!is.na(fgroup), fgroup%in%c("mixed/other fish","NA")==F, cod.end%in%as.character(c(1:6))) %>%
					filter((midoc.stn == "MIDOC01" & cod.end %in% as.character(2:6)) | (midoc.stn == "MIDOC21" & cod.end%in%as.character(1:6))) %>%
					select(midoc.stn, fgroup) %>% distinct() 

# everything else, from photos
md.pa <- fl %>% filter(!is.na(fgroup), fgroup%in%c("squid","other fish", "larval fish")==F, cod.end%in%as.character(c(1:6))) %>%
				select(midoc.stn, fgroup) %>% 
				# slot in the missing records from above here
				bind_rows(pa.m) %>%
				distinct() %>% mutate(PA=1) %>%
				complete(midoc.stn, nesting(fgroup))
md.pa[is.na(md.pa$PA),]$PA <- 0
md.pa

# join this with station data
md.pa <- readRDS("./derived data/midoc_stations_checked.rds") %>% select(midoc.stn, start_time, lat_start, lon_start) %>% inner_join(md.pa)

saveRDS(md.pa, "./derived data/midoc_fish_presence_absence.rds")

# # a restricted version, only including catch in cod-ends 2:6
# md.p16 <- fl %>% filter(!is.na(fgroup), fgroup%in%c("squid","other fish")==F, cod.end%in%as.character(c(1:6))) %>%
# 				select(midoc.stn, fgroup) %>% distinct() %>% mutate(PA=1) %>%
# 				complete(midoc.stn, nesting(fgroup))
# md.p16[is.na(md.p16$PA),]$PA <- 0
# this is no different -- no extra species

# for cod-ends
	# again, do the missing cod-ends first
	ce.pa.m <- SD %>% filter(!is.na(fgroup), fgroup%in%c("mixed/other fish","NA")==F, cod.end%in%as.character(c(1:6))) %>%
					filter((midoc.stn == "MIDOC01" & cod.end %in% as.character(2:6)) | (midoc.stn == "MIDOC21" & cod.end%in%as.character(1:6))) %>%
					select(midoc.stn, cod.end, fgroup) %>% distinct() 

ce.pa <- fl %>% filter(!is.na(fgroup), fgroup%in%c("squid","other fish", "larval fish")==F, cod.end%in%as.character(c(1:6))) %>%
				select(midoc.stn, cod.end,fgroup) %>%
				bind_rows(ce.pa.m) %>%
				arrange(midoc.stn, cod.end, fgroup) %>%
				distinct() %>% mutate(PA=1) %>%
				complete(midoc.stn, cod.end,nesting(fgroup))
ce.pa[is.na(ce.pa$PA),]$PA <- 0
ce.pa

saveRDS(ce.pa, "./derived data/midoc_cod-end_fish_presence_absence.rds")


# size distributions
calc_frequency_distribution = function(input_data, the.brks=seq(-4,4,length.out=50), bw.adj=1){
    tmp_hist = hist(input_data, plot=FALSE, breaks=the.brks)
    tmp_multiplier = (tmp_hist$counts / tmp_hist$density)[!is.na(tmp_hist$counts / tmp_hist$density)][1] # note that this will be the same for every bin, so you just keep the first one
    tmp_freq_dist = density(input_data, adjust=bw.adj)
    freq_dist = data.frame(data_value=tmp_freq_dist$x, count=tmp_freq_dist$y*tmp_multiplier)
    freq_dist
  }


cols<- paste0(brewer.pal(7,"Set2"),99)
# cols<- brewer.pal(7,"Set2")
grps<- c("Bathylagidae","Electrona","Gymnoscopelus","Krefftichthys anderssoni","Paralepididae","Protomyctophum","squid")
brks<- seq(0,300, length.out=50)
bw.adj<- 2
pd <- fl[(fl$SL.mm>= min(brks) ) & (fl$SL.mm<= max(brks)),]

plot(c(0,300), c(0,2000), type="n", axes=F, ylab="count", xlab="SL (cm)")
axis(2)
axis(1, at=seq(0, 400, 100), labels=seq(0,40,10))
par(lwd=5)
# distribution for all fish
lines(calc_frequency_distribution(na.omit(pd$SL.mm),the.brks=brks, ), col="dark grey")
for(i in 1:7){
	lines(calc_frequency_distribution(na.omit(pd[fl$fgroup==grps[i],]$SL.mm),the.brks=brks), col=cols[i])
# legend
	text(x=200, y=1500-((i-1)*75), grps[i], pos=4)
	lines(x=c(180,199), y=rep(1500-((i-1)*75),2), col=cols[i])
}
text(x=200, y=1500 + 75, "all", pos=4)
lines(x=c(180,199), y=rep(1500+75,2), col="dark grey")