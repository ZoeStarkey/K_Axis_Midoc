# DSRII_micronekton_paper_acousitc_data.R
# 17_May_2019
# import acoustic data and calculate NASC sum for grouped strata

library(tidyverse)
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/")
setwd(d)

files <- list.files(paste0(d, "source data/midoc_acoustic/"), pattern = "^SvMIDOC.*csv$", recursive = TRUE, full.names = TRUE)

md_acoustic <- dplyr::bind_rows(
  lapply(files, function(xfile) readr::read_csv(xfile) %>% mutate(filename = basename(xfile))))

md_acoustic<- md_acoustic %>% mutate(midoc.stn = substr(filename,3,9), cod.end=substr(Region_name, 8,8)) %>% mutate(layer=ifelse(cod.end==1, "full water-column",ifelse(cod.end==6, "epipelagic", ifelse(cod.end%in%c(4:5),"upper mesopelagic", "lower mesopelagic"))))


# caculate linearisation of SVmean as 10^(SVmean/10)

d <- dplyr::select(md_acoustic, filename, Sv_mean, PRC_NASC, layer) %>%
		group_by(filename, layer) %>%
		summarise(sum_NASC = sum(PRC_NASC), sum_Sv_mean=sum(Sv_mean)) %>% mutate(sum_Sv_mean_linear=10^(sum_Sv_mean/10))

saveRDS(d, "./Trebilco_DSRII_ms/DSRII_midoc_acoustic_sums_layers.rds")
