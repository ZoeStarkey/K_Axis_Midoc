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
hh <- read_csv("../source data/HIPPIES DB/Fish Data.csv")

# species list table... check if this aligns properly
sp <- read_xlsx("../source data/HIPPIES DB/SpeciesList.xlsx")

# restrict to gear type 35 == IYGPT

# relevant columns of fish data
hfd <- hfd %>% select("Index","Serial Number","HaulID","Fish Type","Weight (g)")

# match to relevant bits of 
