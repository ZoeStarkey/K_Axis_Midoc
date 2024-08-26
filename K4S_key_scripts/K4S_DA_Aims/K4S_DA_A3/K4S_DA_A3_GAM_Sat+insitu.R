



library(dplyr)
library(mgcv)
library(gamm4)
library(gratia)
library(patchwork)
library(ggplot2)
library(grid)



usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_DA_A2")
setwd(d)
dir.exists(d)

par(mfrow=c(2,2))


#removing midoc stations where SST is less than 0
km_bm_sum <- km_bm_sum %>% filter(SST > 0)

############. SUMMED BIOMASS. ##################
# 1. SUMMED BIOMASS - Excluding Gelatinous 
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")

#TSM
allbiom_sum.TSM <- gam(log(bm_sum_all_taxa) ~ s(TSM), data = km_bm_sum)
plot_TSM <- draw(allbiom_sum.TSM, residuals = TRUE) +
  ggtitle("TSM") +
  theme(plot.title = element_text(size = 10))
summary(allbiom_sum.TSM)
gam.check(allbiom_sum.TSM)

par(mfrow=c(2,2))

# CUR model
allbiom_sum.CUR <- gam(log(bm_sum_all_taxa) ~ s(CUR), data = km_bm_sum)
plot_CUR <- draw(allbiom_sum.CUR, residuals = TRUE) +
  ggtitle("CUR") +
  theme(plot.title = element_text(size = 10))
summary(allbiom_sum.CUR)
gam.check(allbiom_sum.CUR)

# CHLA model
allbiom_sum.CHLA <- gam(log(bm_sum_all_taxa) ~ s(CHLA), data = km_bm_sum)
plot_CHLA <- draw(allbiom_sum.CHLA, residuals = TRUE) +
  ggtitle("CHLA") +
  theme(plot.title = element_text(size = 10))
summary(allbiom_sum.CHLA)
gam.check(allbiom_sum.CHLA)

# SST model
allbiom_sum.SST <- gam(log(bm_sum_all_taxa) ~ s(SST), data = km_bm_sum)
plot_SST <- draw(allbiom_sum.SST, residuals = TRUE) +
  ggtitle("SST") +
  theme(plot.title = element_text(size = 10))
summary(allbiom_sum.SST)
gam.check(allbiom_sum.SST)

# Combine all plots
(plot_TSM + plot_CUR + plot_layout(ncol = 2)) / 
  (plot_CHLA + plot_SST + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "Sum Biomass (Logged) All Taxa (Exclude Gelatinous)",
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))
  )



#  2. SUMMED BIOMASS - FISH
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_sum.Rda")

#TSM
fishbiom_sum.TSM <- gam(log(bm_sum_fish) ~ s(TSM), data = km_bm_sum)
plot_TSM <- draw(fishbiom_sum.TSM, residuals = TRUE) +
  ggtitle("TSM") +
  theme(plot.title = element_text(size = 10))
summary(fishbiom_sum.TSM)
gam.check(fishbiom_sum.TSM)

# CUR model
fishbiom_sum.CUR <- gam(log(bm_sum_fish) ~ s(CUR), data = km_bm_sum)
plot_CUR <- draw(fishbiom_sum.CUR, residuals = TRUE) +
  ggtitle("CUR") +
  theme(plot.title = element_text(size = 10))
summary(fishbiom_sum.CUR)
gam.check(fishbiom_sum.CUR)

# CHLA model
fishbiom_sum.CHLA <- gam(log(bm_sum_fish) ~ s(CHLA), data = km_bm_sum)
plot_CHLA <- draw(fishbiom_sum.CHLA, residuals = TRUE) +
  ggtitle("CHLA") +
  theme(plot.title = element_text(size = 10))
summary(fishbiom_sum.CHLA)
gam.check(fishbiom_sum.CHLA)

# SST model
fishbiom_sum.SST <- gam(log(bm_sum_fish) ~ s(SST), data = km_bm_sum)
plot_SST <- draw(fishbiom_sum.SST, residuals = TRUE) +
  ggtitle("SST") +
  theme(plot.title = element_text(size = 10))
summary(fishbiom_sum.SST)
gam.check(fishbiom_sum.SST)

# Combine all plots
combined_plot <- (plot_TSM + plot_CUR + plot_layout(ncol = 2)) / 
  (plot_CHLA + plot_SST + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "Sum Biomass (Logged) Fish",
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))
  )

# Display the combined plot
print(combined_plot)

###SUMMED CEPHALOPODS
# TSM model
cephbiom_sum.TSM <- gam(log(bm_sum_ceph) ~ s(TSM), data = km_bm_sum)
plot_TSM <- draw(cephbiom_sum.TSM, residuals = TRUE) +
  ggtitle("TSM") +
  theme(plot.title = element_text(size = 10))
summary(cephbiom_sum.TSM)
gam.check(cephbiom_sum.TSM)

# CUR model
cephbiom_sum.CUR <- gam(log(bm_sum_ceph) ~ s(CUR), data = km_bm_sum)
plot_CUR <- draw(cephbiom_sum.CUR, residuals = TRUE) +
  ggtitle("CUR") +
  theme(plot.title = element_text(size = 10))
summary(cephbiom_sum.CUR)
gam.check(cephbiom_sum.CUR)

# CHLA model
cephbiom_sum.CHLA <- gam(log(bm_sum_ceph) ~ s(CHLA), data = km_bm_sum)
plot_CHLA <- draw(cephbiom_sum.CHLA, residuals = TRUE) +
  ggtitle("CHLA") +
  theme(plot.title = element_text(size = 10))
summary(cephbiom_sum.CHLA)
gam.check(cephbiom_sum.CHLA)

# SST model
cephbiom_sum.SST <- gam(log(bm_sum_ceph) ~ s(SST), data = km_bm_sum)
plot_SST <- draw(cephbiom_sum.SST, residuals = TRUE) +
  ggtitle("SST") +
  theme(plot.title = element_text(size = 10))
summary(cephbiom_sum.SST)
gam.check(cephbiom_sum.SST)

# Combine all plots
combined_plot <- (plot_TSM + plot_CUR + plot_layout(ncol = 2)) / 
  (plot_CHLA + plot_SST + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "Sum Biomass (Logged) Cephalopods",
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))
  )

# Display the combined plot
print(combined_plot)

#KRILL

#TSM
krillbiom_sum.TSM <- gam(log(bm_sum_krill) ~ s(TSM), data = km_bm_sum)
plot_TSM <- draw(krillbiom_sum.TSM, residuals = TRUE) +
  ggtitle("TSM") +
  theme(plot.title = element_text(size = 10))
summary(krillbiom_sum.TSM)
gam.check(krillbiom_sum.TSM)

# CUR model
krillbiom_sum.CUR <- gam(log(bm_sum_krill) ~ s(CUR), data = km_bm_sum)
plot_CUR <- draw(krillbiom_sum.CUR, residuals = TRUE) +
  ggtitle("CUR") +
  theme(plot.title = element_text(size = 10))
summary(krillbiom_sum.CUR)
gam.check(krillbiom_sum.CUR)

# CHLA model
krillbiom_sum.CHLA <- gam(log(bm_sum_krill) ~ s(CHLA), data = km_bm_sum)
plot_CHLA <- draw(krillbiom_sum.CHLA, residuals = TRUE) +
  ggtitle("CHLA") +
  theme(plot.title = element_text(size = 10))
summary(krillbiom_sum.CHLA)
gam.check(krillbiom_sum.CHLA)

# SST model
krillbiom_sum.SST <- gam(log(bm_sum_krill) ~ s(SST), data = km_bm_sum)
plot_SST <- draw(krillbiom_sum.SST, residuals = TRUE) +
  ggtitle("SST") +
  theme(plot.title = element_text(size = 10))
summary(krillbiom_sum.SST)
gam.check(krillbiom_sum.SST)

# Combine all plots
combined_plot <- (plot_TSM + plot_CUR + plot_layout(ncol = 2)) / 
  (plot_CHLA + plot_SST + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "Sum Biomass (Logged) Krill",
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))
  )

# Display the combined plot
print(combined_plot)








###################INSITU DATA######################
#ALL TAXA - Exluding gelatinous 
#tmin 
allbiom_sum.tmin <- gam(log(bm_sum_all_taxa) ~ s(Tmin),data = km_bm_sum)
all_tax_plot_tmin <- draw(allbiom_sum.tmin, residuals = TRUE) + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - Tmin")
summary(allbiom_sum.tmin)
gam.check(allbiom_sum.tmin)



#O2_min
allbiom_sum.O2_min <- gam(log(bm_sum_all_taxa) ~ s(O2_min),data = km_bm_sum)
all_tax_plot_O2_min <- draw(allbiom_sum.O2_min, residuals = TRUE)+ ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - O2_min")
summary(allbiom_sum.O2_min)
gam.check(allbiom_sum.O2_min)

#SML
allbiom_sum.SML <- gam(log(bm_sum_all_taxa) ~ s(SML),data = km_bm_sum)
all_tax_plot_SML <- draw(allbiom_sum.SML, residuals = TRUE) + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - SML")
summary(allbiom_sum.SML)
gam.check(allbiom_sum.SML)

#Smax
allbiom_sum.Smax <- gam(log(bm_sum_all_taxa) ~ s(Smax),data = km_bm_sum)
all_tax_plot_Smax <- draw(allbiom_sum.Smax, residuals = TRUE) + ggtitle("Sum Biomass (Logged) All Taxa (Exclude Gelatinous) - Smax")
summary(allbiom_sum.Smax)
gam.check(allbiom_sum.Smax)

 (all_tax_plot_tmin + all_tax_plot_O2_min + plot_layout(ncol = 2)) /
  (all_tax_plot_SML + all_tax_plot_Smax + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "Sum Biomass (Logged) All Taxa (Exclude Gelatinous)",
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))
  )
  
  
  
  


########FISH######## 
#tmin
fishbiom_sum.tmin <- gam(log(bm_sum_fish) ~ s(Tmin),data = km_bm_sum)
fish_plot_tmin <- draw(fishbiom_sum.tmin, residuals = TRUE) + ggtitle("Sum Biomass (Logged) Fish - Tmin")
summary(fishbiom_sum.tmin)
gam.check(fishbiom_sum.tmin)

#O2_min
fishbiom_sum.O2_min <- gam(log(bm_sum_fish) ~ s(O2_min),data = km_bm_sum)
fish_plot_O2_min <- draw(fishbiom_sum.O2_min, residuals = TRUE) + ggtitle("Sum Biomass (Logged) Fish - O2_min")
summary(fishbiom_sum.O2_min)
gam.check(fishbiom_sum.O2_min)

#SML
fishbiom_sum.SML <- gam(log(bm_sum_fish) ~ s(SML),data = km_bm_sum)
fish_plot_SML <- draw(fishbiom_sum.SML, residuals = TRUE) + ggtitle("Sum Biomass (Logged) Fish - SML")
summary(fishbiom_sum.SML)
gam.check(fishbiom_sum.SML)

#Smax
fishbiom_sum.Smax <- gam(log(bm_sum_fish) ~ s(Smax),data = km_bm_sum)
fish_plot_Smax <- draw(fishbiom_sum.Smax, residuals = TRUE) + ggtitle("Sum Biomass (Logged) Fish - Smax")
summary(fishbiom_sum.Smax)
gam.check(fishbiom_sum.Smax)


#summary plot 
(fish_plot_tmin + fish_plot_O2_min + plot_layout(ncol = 2)) /
  (fish_plot_SML + fish_plot_Smax + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "Sum Biomass (Logged) Fish",
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))
  )

#######CEPHALOPODS#########
#tmin
cephbiom_sum.tmin <- gam(log(bm_sum_ceph) ~ s(Tmin),data = km_bm_sum)
ceph_plot_tmin <- draw(cephbiom_sum.tmin, residuals = TRUE)+ ggtitle("Sum Biomass (Logged) Cephalopods - Tmin")
summary(cephbiom_sum.tmin)
gam.check(cephbiom_sum.tmin)

#O2_min
cephbiom_sum.O2_min <- gam(log(bm_sum_ceph) ~ s(O2_min),data = km_bm_sum)
ceph_plot_O2_min <- draw(cephbiom_sum.O2_min, residuals = TRUE) + ggtitle("Sum Biomass (Logged) Cephalopods - O2_min")
summary(cephbiom_sum.O2_min)
gam.check(cephbiom_sum.O2_min)

#SML
cephbiom_sum.SML <- gam(log(bm_sum_ceph) ~ s(SML),data = km_bm_sum)
ceph_plot_SML <- draw(cephbiom_sum.SML, residuals = TRUE) + ggtitle("Sum Biomass (Logged) Cephalopods - SML")
summary(cephbiom_sum.SML)
gam.check(cephbiom_sum.SML)

#Smax
cephbiom_sum.Smax <- gam(log(bm_sum_ceph) ~ s(Smax),data = km_bm_sum)
ceph_plot_Smax <- draw(cephbiom_sum.Smax, residuals = TRUE) + ggtitle("Sum Biomass (Logged) Cephalopods - Smax")
summary(cephbiom_sum.Smax)
gam.check(cephbiom_sum.Smax)

#summary plot 
(ceph_plot_tmin + ceph_plot_O2_min + plot_layout(ncol = 2)) /
  (ceph_plot_SML + ceph_plot_Smax + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "Sum Biomass (Logged) Cephalopods",
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))
  )


#KRILL
#tmin
krillbiom_sum.tmin <- gam(log(bm_sum_krill) ~ s(Tmin),data = km_bm_sum)
krill_plot_tmin <- draw(krillbiom_sum.tmin, residuals = TRUE) + ggtitle("Sum Biomass (Logged) Krill - Tmin")
summary(krillbiom_sum.tmin)
gam.check(krillbiom_sum.tmin)

#O2_min
krillbiom_sum.O2_min <- gam(log(bm_sum_krill) ~ s(O2_min),data = km_bm_sum)
krill_plot_O2_min <- draw(krillbiom_sum.O2_min, residuals = TRUE) + ggtitle("Sum Biomass (Logged) Krill - O2_min")
summary(krillbiom_sum.O2_min)
gam.check(krillbiom_sum.O2_min)

#SML
krillbiom_sum.SML <- gam(log(bm_sum_krill) ~ s(SML),data = km_bm_sum)
krill_plot_SML <- draw(krillbiom_sum.SML, residuals = TRUE) + ggtitle("Sum Biomass (Logged) Krill - SML")
summary(krillbiom_sum.SML)
gam.check(krillbiom_sum.SML)

#Smax
krillbiom_sum.Smax <- gam(log(bm_sum_krill) ~ s(Smax),data = km_bm_sum)
krill_plot_Smax <- draw(krillbiom_sum.Smax, residuals = TRUE) + ggtitle("Sum Biomass (Logged) Krill - Smax")
summary(krillbiom_sum.Smax)
gam.check(krillbiom_sum.Smax)

#summary plot 
(krill_plot_tmin + krill_plot_O2_min + plot_layout(ncol = 2)) /
  (krill_plot_SML + krill_plot_Smax + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "Sum Biomass (Logged) Krill",
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5)))
  






###################ADDITIVE MODELS SATELLITE###########
#ALL TAXA - SST
par(mfrow=c(2,2))

allbiom_additive_SST <-gam(log(bm_sum_all_taxa) ~ s(SST)+ s(CUR) + s(CHLA), data = km_bm_sum)
draw(allbiom_additive_SST, residuals = TRUE) + ggtitle("Biomass (Log) All Taxa Additive model SST") + 
  theme(plot.title = element_text(hjust = -7.5, vjust = -20 ))
summary(allbiom_additive_SST)
gam.check(allbiom_additive_SST)

#CHECKING AIC 
AIC(allbiom_additive_SST, allbiom_additive_TSM, fish_additive_SST, fish_additive_TSM, ceph_additive_SST, ceph_additive_TSM, krill_additive_SST, krill_additive_TSM)

#ALL TAXA - TSM 
allbiom_additive_TSM <-gam(log(bm_sum_all_taxa) ~ s(TSM)+ s(CUR) + s(CHLA), data = km_bm_sum)
draw(allbiom_additive_TSM, residuals = TRUE) + ggtitle("Biomass (Log) All Taxa Additive model TSM") + 
  theme(plot.title = element_text(hjust = -7.5, vjust = -20 ))
summary(allbiom_additive_TSM)
gam.check(allbiom_additive_TSM)


#Fish - SST
fish_additive_SST<- gam(log(bm_sum_fish) ~ s(SST)+ s(CUR) + s(CHLA), data = km_bm_sum)
draw(fish_additive_SST, residuals = TRUE) + ggtitle("Biomass (Log) Fish - Additive model SST") + 
  theme(plot.title = element_text(hjust = -6.5 , vjust =-25))
summary(fish_additive_SST)
gam.check(fish_additive_SST)

#FISH - TSM
fish_additive_TSM <- gam(log(bm_sum_fish) ~ s(TSM)+ s(CUR) + s(CHLA), data = km_bm_sum)
draw(fish_additive_TSM, residuals = TRUE) + ggtitle("Biomass (Log) Fish - Additive model TSM") + 
  theme(plot.title = element_text(hjust = -14 , vjust =-25))
summary(fish_additive_TSM)
gam.check(fish_additive_TSM)


#Cephalopods - SST
ceph_additive_SST <- gam(log(bm_sum_ceph) ~ s(SST)+ s(CUR) + s(CHLA), data = km_bm_sum)
draw(ceph_additive_SST, residuals = TRUE) + ggtitle("Biomass (Log) Ceph - Additive model SST") + 
  theme(plot.title = element_text(hjust = -18, vjust = -25))
summary(ceph_additive_SST)
gam.check(ceph_additive_SST)

#Cephalopods - TSM 
ceph_additive_TSM <- gam(log(bm_sum_ceph) ~ s(TSM)+ s(CUR) + s(CHLA), data = km_bm_sum)
draw(ceph_additive_TSM, residuals = TRUE) + ggtitle("Biomass (Log) Ceph - Additive model TSM") + 
  theme(plot.title = element_text(hjust = -15, vjust = -25))
summary(ceph_additive_TSM)
gam.check(ceph_additive_TSM)


#Krill - SST
krill_additive_SST <- gam(log(bm_sum_krill) ~ s(SST)+ s(CUR) + s(CHLA), data = km_bm_sum)
draw(krill_additive_SST, residuals = TRUE) + ggtitle("Biomass (Logged) Krill - Additive model SST") + 
  theme(plot.title = element_text(hjust = -9.5, vjust = -20))
summary(krill_additive_SST)
gam.check(krill_additive_SST)

#KRILL - TSM 
krill_additive_TSM <- gam(log(bm_sum_krill) ~ s(TSM)+ s(CUR) + s(CHLA), data = km_bm_sum)
draw(krill_additive_TSM, residuals = TRUE) + ggtitle("Biomass (Logged) Krill - Additive model TSM") + 
  theme(plot.title = element_text(hjust = -8.5, vjust = -20))
summary(krill_additive_TSM)
gam.check(krill_additive_TSM)



#trying to work out where that residual came from 

which.max(abs(residuals(allbiom_additive_SST)))
which.max(abs(residuals(fish_additive_SST)))

#make a new dataframe including teh columns bm_sum_all_taxa and SST 


km_bm_sum_residual_fix <- km_bm_sum %>% select(bm_sum_all_taxa,bm_sum_fish, SST, TSM, CHLA, CUR)
km_bm_sum_residual_fix$bm_sum_all_taxa_log <- log(km_bm_sum_residual_fix$bm_sum_all_taxa)





###################ADDITIVE MODELS IN SITU###########

#ALL TAXA 
#Tmin
allbiom_additive_insitu <- gam(log(bm_sum_all_taxa) ~ s(Tmin) + s(O2_min), data = km_bm_sum)
draw(allbiom_additive_insitu, residuals = TRUE) + ggtitle("Biomass (Log) All Taxa - Additive model")  +
  theme(plot.title = element_text(hjust = -7, vjust = 1))
summary(allbiom_additive_insitu)
gam.check(allbiom_additive_insitu)


#Fish
#Tmin
fish_additive_insitu <- gam(log(bm_sum_fish) ~ s(Tmin) + s(O2_min), data = km_bm_sum)
draw(fish_additive_insitu, residuals = TRUE) + ggtitle("Biomass (Log) Fish - Additive model")  +
  theme(plot.title = element_text(hjust = -3.5, vjust = 1))
summary(fish_additive_insitu)
gam.check(fish_additive_insitu)


#Cephalopods
#Tmin
ceph_additive_insitu <- gam(log(bm_sum_ceph) ~ s(Tmin) + s(O2_min), data = km_bm_sum)
draw(ceph_additive_insitu, residuals = TRUE) + ggtitle("Biomass (Log) Cephalopods - Additive model")  +
  theme(plot.title = element_text(hjust = -3.5, vjust = 1))
summary(ceph_additive_insitu)
gam.check(ceph_additive_insitu)

#Krill
#Tmin
krill_additive_insitu <- gam(log(bm_sum_krill) ~ s(Tmin) + s(O2_min), data = km_bm_sum)
draw(krill_additive_insitu, residuals = TRUE) + ggtitle("Biomass (Log) Krill - Additive model")  +
  theme(plot.title = element_text(hjust = -3.5, vjust = 1))
summary(krill_additive_insitu)
gam.check(krill_additive_insitu)




################ONLY UPPER 200m ###############

load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_bm_depth.Rda")

#Filtering upper 200m 
km_bm_surface <- km_bm_depth %>%
  filter(depth == "0-200")

allbiom_sum.SST <- gam(log(bm_depth_all_taxa) ~ s(SST), data = km_bm_surface)
plot_SST <- draw(allbiom_sum.TSM, residuals = TRUE) +
  ggtitle("TSM") +
  theme(plot.title = element_text(size = 10))
summary(allbiom_sum.SST)



#Function 

gam_analysis <- function(data, response_var, predictor_var) {
  # Create the formula for GAM
  formula <- as.formula(paste("log(", response_var, ") ~ s(", predictor_var, ")"))
  
  # Fit the GAM model
  gam_model <- gam(formula, data = data)
  
  # Create the plot
  plot <- draw(gam_model, residuals = TRUE) +
    ggtitle(predictor_var) +
    theme(plot.title = element_text(size = 10))
  
  # Return a list containing the model and the plot
  return(list(model = gam_model, plot = plot))
}

######ALL TAXA 0-200m#########


#TSM
surface_all_tax_TSM <- gam_analysis(km_bm_surface, "bm_depth_all_taxa", "TSM")
surface_all_tax_TSM_plot <-(surface_all_tax_TSM$plot)
summary(surface_all_tax_TSM$model)

#CUR
surface_all_tax_CUR <- gam_analysis(km_bm_surface, "bm_depth_all_taxa", "CUR")
surface_all_tax_CUR_plot <-(surface_all_tax_CUR$plot)
summary(surface_all_tax_CUR$model)

#CHLA
surface_all_tax_CHLA <- gam_analysis(km_bm_surface, "bm_depth_all_taxa", "CHLA")
surface_all_tax_CHLA_plot <-(surface_all_tax_CHLA$plot)
summary(surface_all_tax_CHLA$model)

#SST
surface_all_tax_SST <- gam_analysis(km_bm_surface, "bm_depth_all_taxa", "SST")
surface_all_tax_SST_plot <- (surface_all_tax_SST$plot)
summary(surface_all_tax_SST$model)





(surface_all_tax_TSM_plot + surface_all_tax_CUR_plot + plot_layout(ncol = 2)) / 
  (surface_all_tax_CHLA_plot + surface_all_tax_SST_plot + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "0-200m Biomass (Logged) All Taxa (Exclude Gelatinous)",
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))
  )



#fish


#TSM
surface_fish_TSM <- gam_analysis(km_bm_surface, "bm_depth_fish", "TSM")
surface_fish_plot_TSM <-(surface_fish_TSM$plot)
summary(surface_fish_TSM$model)


#CUR
surface_fish_CUR <- gam_analysis(km_bm_surface, "bm_depth_fish", "CUR")
surface_fish_plot_CUR <-(surface_fish_CUR$plot)
summary(surface_fish_CUR$model)

#CHLA
surface_fish_CHLA <- gam_analysis(km_bm_surface, "bm_depth_fish", "CHLA")
surface_fish_plot_CHLA <-(surface_fish_CHLA$plot)
summary(surface_fish_CHLA$model)


#SST
surface_fish_SST <- gam_analysis(km_bm_surface, "bm_depth_fish", "SST")
surface_fish_plot_SST <-(surface_fish_SST$plot)
summary(surface_fish_SST$model)



(surface_fish_plot_TSM + surface_fish_plot_CUR + plot_layout(ncol = 2)) / 
  (surface_fish_plot_CHLA + surface_fish_plot_SST + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "0-200m Biomass (Logged) Fish (Exclude Gelatinous)",
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))
  )



#######CEPHALOPODS

#TSM
surface_ceph_TSM <- gam_analysis(km_bm_surface, "bm_depth_ceph", "TSM")
surface_ceph_plot_TSM <-(surface_ceph_TSM$plot)
summary(surface_ceph_TSM$model)

#CHLA
surface_ceph_CHLA <- gam_analysis(km_bm_surface, "bm_depth_ceph", "CHLA")
surface_ceph_plot_CHLA <-(surface_ceph_CHLA$plot)
summary(surface_ceph_CHLA$model)

#CUR
surface_ceph_CUR <- gam_analysis(km_bm_surface, "bm_depth_ceph", "CUR")
surface_ceph_plot_CUR <-(surface_ceph_CUR$plot)
summary(surface_ceph_CUR$model)

#SST
surface_ceph_SST <- gam_analysis(km_bm_surface, "bm_depth_ceph", "SST")
surface_ceph_SST_plot <- (surface_ceph_SST$plot)
summary(surface_ceph_SST$model)


(surface_ceph_plot_TSM + surface_ceph_plot_CUR + plot_layout(ncol = 2)) / 
  (surface_ceph_plot_CHLA + surface_ceph_SST_plot + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "0-200m Biomass (Logged) Cephalopods",
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))
  )


#krill
#TSM
surface_krill_TSM <- gam_analysis(km_bm_surface, "bm_depth_krill", "TSM")
surface_krill_plot_TSM <- (surface_krill_TSM$plot)
summary(surface_krill_TSM$model)

#CUR
surface_krill_CUR <- gam_analysis(km_bm_surface, "bm_depth_krill", "CUR")
surface_krill_plot_CUR <- (surface_krill_CUR$plot)
summary(surface_krill_CUR$model)

#CHLA
surface_krill_CHLA <- gam_analysis(km_bm_surface, "bm_depth_krill", "CHLA")
surface_krill_plot_CHLA <- (surface_krill_CHLA$plot)
summary(surface_krill_CHLA$model)

#SST
surface_krill_SST <- gam_analysis(km_bm_surface, "bm_depth_krill", "SST")
surface_krill_plot_SST <- (surface_krill_SST$plot)
summary(surface_krill_SST$model)

(combined_plot <- (surface_krill_plot_TSM + surface_krill_plot_CUR + plot_layout(ncol = 2)) / 
  (surface_krill_plot_CHLA + surface_krill_plot_SST + plot_layout(ncol = 2)) +
  plot_annotation(
    title = "0-200m Biomass (Logged) Krill",
    theme = theme(plot.title = element_text(size = 14, hjust = 0.5))
  ))





















#INSITU 
#ALL TAXA
#tmin
surface_all_tax_SST <- gam_analysis(km_bm_surface, "bm_depth_all_taxa", "Tmin")
print(surface_all_tax_SST$plot)
summary(surface_all_tax_SST$model)

#O2_min
surface_all_tax_O2_min <- gam_analysis(km_bm_surface, "bm_depth_all_taxa", "O2_min")
print(surface_all_tax_O2_min$plot)
summary(surface_all_tax_O2_min$model)

#SML
surface_all_tax_SML <- gam_analysis(km_bm_surface, "bm_depth_all_taxa", "SML")
print(surface_all_tax_SML$plot)
summary(surface_all_tax_SML$model)

#Smax
surface_all_tax_Smax <- gam_analysis(km_bm_surface, "bm_depth_all_taxa", "Smax")
print(surface_all_tax_Smax$plot)
summary(surface_all_tax_Smax$model)

#FISH
#tmin
surface_all_tax_SST <- gam_analysis(km_bm_surface, "bm_depth_all_fish", "Tmin")
print(surface_all_tax_SST$plot)
summary(surface_all_tax_SST$model)

#O2_min
surface_all_tax_O2_min <- gam_analysis(km_bm_surface, "bm_depth_all_fish", "O2_min")
print(surface_all_tax_O2_min$plot)
summary(surface_all_tax_O2_min$model)

#SML
surface_all_tax_SML <- gam_analysis(km_bm_surface, "bm_depth_all_fish", "SML")
print(surface_all_tax_SML$plot)
summary(surface_all_tax_SML$model)

#Smax
surface_all_tax_Smax <- gam_analysis(km_bm_surface, "bm_depth_all_fish", "Smax")
print(surface_all_tax_Smax$plot)
summary(surface_all_tax_Smax$model)

#CEPHALOPODS
#tmin
surface_all_tax_SST <- gam_analysis(km_bm_surface, "bm_depth_all_ceph", "Tmin")
print(surface_all_tax_SST$plot)
summary(surface_all_tax_SST$model)

#O2_min
surface_all_tax_O2_min <- gam_analysis(km_bm_surface, "bm_depth_all_ceph", "O2_min")
print(surface_all_tax_O2_min$plot)
summary(surface_all_tax_O2_min$model)

#SML
surface_all_tax_SML <- gam_analysis(km_bm_surface, "bm_depth_all_ceph", "SML")
print(surface_all_tax_SML$plot)
summary(surface_all_tax_SML$model)

#Smax
surface_all_tax_Smax <- gam_analysis(km_bm_surface, "bm_depth_all_ceph", "Smax")
print(surface_all_tax_Smax$plot)
summary(surface_all_tax_Smax$model)