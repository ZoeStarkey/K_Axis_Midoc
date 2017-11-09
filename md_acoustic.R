# path to parent folder 

pth <- "~/Library/Mobile Documents/com~apple~CloudDocs/RT_downloads/Shared/midoc"

files <- list.files(pth, pattern = "^SvMIDOC.*csv$", recursive = TRUE, full.names = TRUE)

md_acoustic <- dplyr::bind_rows(
  lapply(files, function(xfile) readr::read_csv(xfile) %>% mutate(filename = basename(xfile))))


# Region_name gives us the grouping of data that we want for this
# within each of these bins, sum NASC and SVmean. Then caculate linearisation of SVmean as 10^(SVmean/10)
# md_acoustic %>% filter(filename=="1") %>% ggplot(aes(x=1:nrow(.), y=-Layer_depth_max, colour=Region_name)) +geom_point()

d <- dplyr::select(md_acoustic, filename, Sv_mean, PRC_NASC, Region_name) %>%
		group_by(filename, Region_name) %>%
		summarise(sum_NASC = sum(PRC_NASC), sum_Sv_mean=sum(Sv_mean)) %>%
		mutate(midoc = substr(filename,3,7), codend=substr(Region_name, 8,8), sum_Sv_mean_linear=10^(sum_Sv_mean/10)) %>% select(-Region_name)

saveRDS(d, "midoc_acoustic_sums.rda")



