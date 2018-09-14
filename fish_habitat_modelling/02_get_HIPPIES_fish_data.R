# 02_get_HIPPIES_fish_data.R
# script to bring in HIPPIES fish data, and to convert to format appropriate for habitat modelling

library(tidyverse)
library(readxl)
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/fish_habitat_modelling")
setwd(d)

# fish table
hfd <- read_csv("../source data/HIPPIES DB/Fish Data.csv")

# haul table
hh <- read_csv("../source data/HIPPIES DB/haul.csv")

# species list table... check if this aligns properly
sp <- read_xlsx("../source data/HIPPIES DB/SpeciesList.xlsx")

# restrict to gear type 35 == IYGPT

# relevant columns of fish data
hfd <- hfd %>% select("Index","Serial Number","HaulID","Fish Type","Weight (g)")

# match species to catch data
hfd$sp <- sp$SpeciesNameLong[match(hfd$`Fish Type`, sp$SpeciesID)]

# relevant columns from haul data 
hh <- hh %>% select("HaulID","HaulNumber","Fishing Depth (m)","Start Haul Lat","Start Haul Lon","End Haul Lat","End Haul Lon","Tow Distance (NM)","Tow Speed (knots)","Comments")

# plots/checks
	# depth ~ distance
	# speed
	# plot locations (starts and ends) on map
	


hfd$tow_type <- hh$`Gear Type`[match(hfd$HaulID, hh$HaulID)]

# restrict to only IYGPT shots
hp_rmt <- hfd %>% filter(tow_type==34)
hfd <- hfd %>% filter(tow_type==35)

# write out shortened version of station data

# summarise biomass and abundance to depth strata and convert to densities per volume swept
