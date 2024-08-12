usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_DA_A2")
setwd(d)
dir.exists(d)



############. SUMMED BIOMASS. ##################
# 1. SUMMED BIOMASS - Excluding Gelatinous 
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")

#TSM
allbiom_sum.TSM <- gam(log(bm_sum_all_taxa) ~ s(TSM),data = km_bm_sum)
plot_object <- draw(allbiom_sum.TSM, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - TSM") 
summary(allbiom_sum.TSM)

#CUR
allbiom_sum.CUR <- gam(log(bm_sum_all_taxa) ~ s(CUR),data = km_bm_sum)
plot_object <- draw(allbiom_sum.CUR, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - CUR")
summary(allbiom_sum.CUR)

#CHLA
allbiom_sum.CHLA <- gam(log(bm_sum_all_taxa) ~ s(CHLA),data = km_bm_sum)
plot_object <- draw(allbiom_sum.CHLA, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - CHLA")
summary(allbiom_sum.CHLA)

#SST
allbiom_sum.SST <- gam(log(bm_sum_all_taxa) ~ s(SST),data = km_bm_sum)
plot_object <- draw(allbiom_sum.SST, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - SST")
summary(allbiom_sum.SST)

#  2. SUMMED BIOMASS - FISH
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")

#TSM
fishbiom_sum.TSM <- gam(log(bm_sum_fish) ~ s(TSM),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.TSM, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Fish - TSM")
summary(fishbiom_sum.TSM)

#CUR
fishbiom_sum.CUR <- gam(log(bm_sum_fish) ~ s(CUR),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.CUR, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Fish - CUR")
summary(fishbiom_sum.CUR)

#CHLA
fishbiom_sum.CHLA <- gam(log(bm_sum_fish) ~ s(CHLA),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.CHLA, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Fish - CHLA")
summary(fishbiom_sum.CHLA)

#SST
fishbiom_sum.SST <- gam(log(bm_sum_fish) ~ s(SST),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.SST, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Fish - SST")
summary(fishbiom_sum.SST)

###SUMMED CEPHALOPODS
#TSM
cephbiom_sum.TSM <- gam(log(bm_sum_ceph) ~ s(TSM),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.TSM, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - TSM")
summary(cephbiom_sum.TSM)


#CUR
cephbiom_sum.CUR <- gam(log(bm_sum_ceph) ~ s(CUR),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.CUR, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - CUR")
summary(cephbiom_sum.CUR)

#CHLA
cephbiom_sum.CHLA <- gam(log(bm_sum_ceph) ~ s(CHLA),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.CHLA, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - CHLA")
summary(cephbiom_sum.CHLA)

#SST
cephbiom_sum.SST <- gam(log(bm_sum_ceph) ~ s(SST),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.SST, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - SST")
summary(cephbiom_sum.SST)

#KRILL

#TSM
krillbiom_sum.TSM <- gam(log(bm_sum_krill) ~ s(TSM),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.TSM, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - TSM")
summary(krillbiom_sum.TSM)

#CUR
krillbiom_sum.CUR <- gam(log(bm_sum_krill) ~ s(CUR),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.CUR, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - CUR")
summary(krillbiom_sum.CUR)

#CHLA
krillbiom_sum.CHLA <- gam(log(bm_sum_krill) ~ s(CHLA),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.CHLA, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - CHLA")
summary(krillbiom_sum.CHLA)

#SST
krillbiom_sum.SST <- gam(log(bm_sum_krill) ~ s(SST),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.SST, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - SST")
summary(krillbiom_sum.SST)



###################INSITU DATA######################
#ALL TAXA - Exluding gelatinous 
#tmin 
allbiom_sum.tmin <- gam(log(bm_sum_all_taxa) ~ s(Tmin),data = km_bm_sum)
plot_object <- draw(allbiom_sum.tmin, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - Tmin")
summary(allbiom_sum.tmin)

#O2_min
allbiom_sum.O2_min <- gam(log(bm_sum_all_taxa) ~ s(O2_min),data = km_bm_sum)
plot_object <- draw(allbiom_sum.O2_min, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - O2_min")
summary(allbiom_sum.O2_min)

#SML
allbiom_sum.SML <- gam(log(bm_sum_all_taxa) ~ s(SML),data = km_bm_sum)
plot_object <- draw(allbiom_sum.SML, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - SML")
summary(allbiom_sum.SML)

#Smax
allbiom_sum.Smax <- gam(log(bm_sum_all_taxa) ~ s(Smax),data = km_bm_sum)
plot_object <- draw(allbiom_sum.Smax, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - Smax")
summary(allbiom_sum.Smax)

########FISH######## 
#tmin
fishbiom_sum.tmin <- gam(log(bm_sum_fish) ~ s(Tmin),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.tmin, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Fish - Tmin")
summary(fishbiom_sum.tmin)

#O2_min
fishbiom_sum.O2_min <- gam(log(bm_sum_fish) ~ s(O2_min),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.O2_min, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Fish - O2_min")
summary(fishbiom_sum.O2_min)

#SML
fishbiom_sum.SML <- gam(log(bm_sum_fish) ~ s(SML),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.SML, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Fish - SML")
summary(fishbiom_sum.SML)

#Smax
fishbiom_sum.Smax <- gam(log(bm_sum_fish) ~ s(Smax),data = km_bm_sum)
plot_object <- draw(fishbiom_sum.Smax, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Fish - Smax")
summary(fishbiom_sum.Smax)

#CEPHALOPODS
#tmin
cephbiom_sum.tmin <- gam(log(bm_sum_ceph) ~ s(Tmin),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.tmin, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - Tmin")
summary(cephbiom_sum.tmin)

#O2_min
cephbiom_sum.O2_min <- gam(log(bm_sum_ceph) ~ s(O2_min),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.O2_min, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - O2_min")
summary(cephbiom_sum.O2_min)

#SML
cephbiom_sum.SML <- gam(log(bm_sum_ceph) ~ s(SML),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.SML, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - SML")
summary(cephbiom_sum.SML)

#Smax
cephbiom_sum.Smax <- gam(log(bm_sum_ceph) ~ s(Smax),data = km_bm_sum)
plot_object <- draw(cephbiom_sum.Smax, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Cephalopods - Smax")
summary(cephbiom_sum.Smax)

#KRILL
#tmin
krillbiom_sum.tmin <- gam(log(bm_sum_krill) ~ s(Tmin),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.tmin, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - Tmin")
summary(krillbiom_sum.tmin)

#O2_min
krillbiom_sum.O2_min <- gam(log(bm_sum_krill) ~ s(O2_min),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.O2_min, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - O2_min")
summary(krillbiom_sum.O2_min)

#SML
krillbiom_sum.SML <- gam(log(bm_sum_krill) ~ s(SML),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.SML, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - SML")
summary(krillbiom_sum.SML)

#Smax
krillbiom_sum.Smax <- gam(log(bm_sum_krill) ~ s(Smax),data = km_bm_sum)
plot_object <- draw(krillbiom_sum.Smax, residuals = TRUE)
plot_object + ggtitle("Sum Biomass (Logged) Krill - Smax")
summary(krillbiom_sum.Smax)



