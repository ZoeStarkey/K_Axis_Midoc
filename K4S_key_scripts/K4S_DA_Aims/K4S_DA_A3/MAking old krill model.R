load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum_2.Rda")
#removing the outliers deviated substantially from the majority of SST values (0.7 to 2.2°C), preventing the GAMs from fitting reasonable smooths across all stations. 
km_bm_sum_2 <- km_bm_sum_2 %>% filter(SST > 0)


#krill biomass
krill_additive_all_vars <-gam(log(bm_sum_krill) ~ s(SST)+ s(CUR) + s(CHLA) +s(days_since_melt), data = km_bm_sum_2)
draw(krill_additive_all_vars, residuals = TRUE) + ggtitle("Krill Biomass (Log) All Taxa Additive model all vars") + 
  theme(plot.title = element_text(hjust = -7.5, vjust = -20 ))
summary(krill_additive_all_vars)
gam.check(krill_additive_all_vars)


load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum_2.Rda")
km_bm_sum_2 <- km_bm_sum_2 %>% filter(SST > 0)

krill_additive_all_vars <-gam(log(bm_sum_krill) ~ s(SST)+ s(CUR) + s(CHLA) +s(TSM), data = km_bm_sum_2)
draw(krill_additive_all_vars, residuals = TRUE) + ggtitle("Krill Biomass (Log) All Taxa Additive model all vars") + 
  theme(plot.title = element_text(hjust = -7.5, vjust = -20 ))
summary(krill_additive_all_vars)
gam.check(krill_additive_all_vars)
