# 04_md_acoustic.R
# 29 June 2018
# import acoustic data and calculate NASC sum for each cod-end

library(tidyverse)
usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/GitHub/K_axis_midoc/")
setwd(d)

files <- list.files(paste0(d, "source data/midoc_acoustic/"), pattern = "^SvMIDOC.*csv$", recursive = TRUE, full.names = TRUE)

md_acoustic <- dplyr::bind_rows(
  lapply(files, function(xfile) readr::read_csv(xfile) %>% mutate(filename = basename(xfile))))


# caculate linearisation of SVmean as 10^(SVmean/10)

d <- dplyr::select(md_acoustic, filename, Sv_mean, PRC_NASC, Region_name) %>%
		group_by(filename, Region_name) %>%
		summarise(sum_NASC = sum(PRC_NASC), sum_Sv_mean=sum(Sv_mean)) %>%
		mutate(midoc.stn = substr(filename,3,9), cod.end=substr(Region_name, 8,8), sum_Sv_mean_linear=10^(sum_Sv_mean/10)) %>% select(-Region_name)

saveRDS(d, "./derived data/midoc_acoustic_sums.rda")
