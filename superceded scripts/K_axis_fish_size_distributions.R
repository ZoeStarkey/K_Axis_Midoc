# K_axis_fish_size_distributions.R
# Preliminary exploratory plots of size distributions of fish, from photo measurements

library(readxl)
library(RColorBrewer)

col.names <- c("midoc.no", "cod.end", "n.in.photo", "photo.label", "SL.mm", "sample.label", "taxon", "fish.group", "taxon.uncertain", "notes", "measured.by")


# import from excel
		# directory with data
		the.dir <- "~/Dropbox/ACE CRC/K_axis/k_axis_reporting_shared/Fish measuring/"
		# latest excel workbook
		the.wb <- "MIDOC_fish_photo_measuring_27June2016.xlsx"
		f <- paste0(the.dir, the.wb)
		
		# read
			# sample data
			fl <- read_excel(f, sheet = "measurements", col_names=col.names, col_types=rep("text", length(col.names)), skip=1)

			# taxon lookup key
			tk <- read_excel(f, sheet = "taxon lookup")
			
# processing and checks
	# merge/simplify taxa into fish.groups
	fl$fgroup <- tk$fish_group[match(fl$taxon, tk$recorded)]

# checking sizes
hist(fl$SL.mm) # shows quite a few fish seem to have unrealistic sizes			
fl$SL.mm <- as.numeric(fl$SL.mm)

fl[fl$SL.mm>1000,] ## first cut, anything over 1m is wrong
# from the following photos
	# KX16-MIDOC04_016.jpg
	# KX16-MIDOC05_051.jpg
	# KX16-MIDOC07_047.jpg
	# KX16-MIDOC09_020.jpg
	# KX16-MIDOC14_033.jpg
	


fl[fl$SL.mm>300,] ##  electrona and gymno with impossible measurements
# from the following photos
	# KX16-MIDOC15_004.jpg
	# KX16-MIDOC15_006.jpg

# omit these for now 
fl <- fl[substr(fl$photo.label, 1,20)%in%c("KX16-MIDOC04_016.jpg","KX16-MIDOC05_051.jpg","KX16-MIDOC07_047.jpg","KX16-MIDOC09_020.jpg","KX16-MIDOC14_033.jpg","KX16-MIDOC15_004.jpg","KX16-MIDOC15_006.jpg")==F,]


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



