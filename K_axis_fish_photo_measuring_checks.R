# K_axis_fish_photo_measuring_checks.R
# Script for QC checks of measurements from photos of K-axis MIDOC catch

library(readxl)
library(RColorBrewer)

col.names <- c("midoc.no", "cod.end", "n.in.photo", "photo.label", "SL.mm", "sample.label", "taxon", "fish.group", "taxon.uncertain", "notes", "measured.by")


# import from excel
		# directory with data
		the.dir <- "~/Dropbox/ACE CRC/K_axis/k_axis_reporting_shared/Fish measuring/"
		# latest excel workbook
		the.wb <- "MIDOC_fish_photo_measuring_28July2016.xlsx"
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

fl[fl$SL.mm>1000,] # looks ok
	
fl[fl$SL.mm>300 & !is.na(fl$photo.label),] ##  electrona and gymno with impossible measurements
	# check sizes in the following photos:  KX16-MIDOC19_010.jpg - has a gymno at 32.5 cm
	# looks okay otherwise



# standardise midoc.no column (all MIDOCxx)

fl[is.na(fl$midoc.no),]$midoc.no <- grep("",fl[is.na(fl$midoc.no),]$photo.label)



# check for conflicts between codend in ID tag and codend from photo name (there is one known mismatch for midoc 3 - where several samples are shown as CE4 in photo name, but labels say CE6)
fl[!is.na(fl$sample.label)&!grepl("stop",fl$sample.label),]$sample.label
fl$photo_ce<- 
