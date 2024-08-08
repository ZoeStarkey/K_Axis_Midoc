
library(dplyr)
library(mgcv)
library(gamm4)
library(gratia)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A2/K4S_DA_A2")
setwd(d)
dir.exists(d)



############. SUMMED BIOMASS. ##################
# 1. SUMMED BIOMASS - Excluding Gelatinous 
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/km_bm_sum.Rda")

#Day
allbiom_sum.day <- gam(log(bm_sum_all_taxa) ~ s(day),data = km_bm_sum)
draw(allbiom_sum.day, residuals = TRUE) 
summary(allbiom_sum.day)


#Lunar fraction - illuminated disk 
allbiom_sum.lunar <- gam(log(bm_sum_all_taxa) ~ s(lunar_fraction),data = km_bm_sum)
draw(allbiom_sum.lunar, residuals = TRUE) 
summary(allbiom_sum.lunar)

#Solar angle 
allbiom_sum.solar <- gam(log(bm_sum_all_taxa) ~ s(altitude),data = km_bm_sum)
draw(allbiom_sum.solar, residuals = TRUE) 
summary(allbiom_sum.solar)




x#  2. SUMMED BIOMASS - FISH
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/km_bm_sum.Rda")

#Day 
fishbiom_sum.day <- gam(log(bm_sum_fish) ~ s(day),data = km_bm_sum)
draw(fishbiom_sum.day, residuals = TRUE) 
summary(fishbiom_sum.day)

#Lunar fraction - illuminated disk 
fishbiom_sum.lunar <- gam(log(bm_sum_fish) ~ s(lunar_fraction),data = km_bm_sum)
draw(fishbiom_sum.lunar, residuals = TRUE) 
summary(fishbiom_sum.lunar)


#Solar angle 
fishbiom_sum.solar <- gam(log(bm_sum_fish) ~ s(altitude),data = km_bm_sum)
draw(fishbiom_sum.solar, residuals = TRUE) 
summary(fishbiom_sum.solar)




# 3. SUMMED BIOMASS - SQUID
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/km_bm_sum.Rda")

#Day
cephbiom_sum.day <- gam(log(bm_sum_ceph) ~ s(day),data = km_bm_sum)
draw(cephbiom_sum.day, residuals = TRUE) 
summary(cephbiom_sum.day)

#Lunar fraction - illuminated disk 
cephbiom_sum.lunar <- gam(log(bm_sum_ceph) ~ s(lunar_fraction),data = km_bm_sum)
draw(cephbiom_sum.lunar, residuals = TRUE) 
summary(cephbiom_sum.lunar)

#Solar angle 
cephbiom_sum.solar <- gam(log(bm_sum_ceph) ~ s(altitude),data = km_bm_sum)
draw(cephbiom_sum.solar, residuals = TRUE) 
summary(cephbiom_sum.solar)



############. BIOMASS SEPARATED BY DEPTH. ##################
#BIOMASS SEPARATED BY DEPTH - EXCLUDING GELATINOUS 
#load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/km_bm_depth.Rda")


#day 
allbiom_depth.day.gam <- gam(log(bm_depth_all_taxa) ~ s(day, by = depth),data = km_bm_depth)
draw(allbiom_depth.day.gam, residuals = TRUE)
summary(allbiom_depth.day.gam)

allbiom_depth.day.re <- gamm(log(bm_depth_all_taxa) ~ s(day, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(allbiom_depth.day.re, residuals = TRUE)
summary(allbiom_depth.day.re$gam)
summary(allbiom_depth.day.re$lme)

#lunar fraction - illuminated disk
allbiom_depth.lunar.gam <- gam(log(bm_depth_all_taxa) ~ s(lunar_fraction, by = depth),data = km_bm_depth)
draw(allbiom_depth.lunar.gam, residuals = TRUE)
summary(allbiom_depth.lunar.gam)

allbiom_depth.lunar.re <- gamm(log(bm_depth_all_taxa) ~ s(lunar_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(allbiom_depth.lunar.re, residuals = TRUE)
summary(allbiom_depth.lunar.re$gam)
summary(allbiom_depth.lunar.re$lme)


#solar angle 
allbiom_depth.solar.gam <- gam(log(bm_depth_all_taxa) ~ s(altitude, by = depth),data = km_bm_depth)
draw(allbiom_depth.solar.gam, residuals = TRUE)
summary(allbiom_depth.solar.gam)

allbiom_depth.solar.re <- gamm(log(bm_depth_all_taxa) ~ s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(allbiom_depth.solar.re, residuals = TRUE)
summary(allbiom_depth.solar.re$gam)
summary(allbiom_depth.solar.re$lme)




#BIOMASS SEPARATED BY DEPTH - FISH

#day 
fish_depth.day.gam <- gam(log(bm_depth_fish) ~ s(day, by = depth),data = km_bm_depth)
draw(fish_depth.day.gam, residuals = TRUE)
summary(fish_depth.day.gam)

fish_depth.day.re <- gamm(log(bm_depth_fish) ~ s(day, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(fish_depth.day.re, residuals = TRUE)
summary(fish_depth.day.re$gam)
summary(fish_depth.day.re$lme)

#lunar fraction - illuminated disk
fish_depth.lunar.gam <- gam(log(bm_depth_fish) ~ s(lunar_fraction, by = depth),data = km_bm_depth)
draw(fish_depth.lunar.gam, residuals = TRUE)
summary(fish_depth.lunar.gam)

fish_depth.lunar.re <- gamm(log(bm_depth_fish) ~ s(lunar_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(fish_depth.lunar.re, residuals = TRUE)
summary(fish_depth.lunar.re$gam)
summary(fish_depth.lunar.re$lme)


#solar angle 
fish_depth.solar.gam <- gam(log(bm_depth_fish) ~ s(altitude, by = depth),data = km_bm_depth)
draw(fish_depth.solar.gam, residuals = TRUE)
summary(fish_depth.solar.gam)

fish_depth.solar.re <- gamm(log(bm_depth_fish) ~ s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(fish_depth.solar.re, residuals = TRUE)
summary(fish_depth.solar.re$gam)
summary(fish_depth.solar.re$lme)


#BIOMASS SEPARATED BY DEPTH - CEPHALOPODS

#day
ceph_depth.day.gam <- gam(log(bm_depth_ceph) ~ s(day, by = depth),data = km_bm_depth)
draw(ceph_depth.day.gam, residuals = TRUE)
summary(ceph_depth.day.gam)

ceph_depth.day.re <- gamm(log(bm_depth_ceph) ~ s(day, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.day.re, residuals = TRUE)
summary(ceph_depth.day.re$gam)
summary(ceph_depth.day.re$lme)

#lunar fraction - illuminated disk
ceph_depth.lunar.gam <- gam(log(bm_depth_ceph) ~ s(lunar_fraction, by = depth),data = km_bm_depth)
draw(ceph_depth.lunar.gam, residuals = TRUE)
summary(ceph_depth.lunar.gam)

ceph_depth.lunar.re <- gamm(log(bm_depth_ceph) ~ s(lunar_fraction, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.lunar.re, residuals = TRUE)
summary(ceph_depth.lunar.re$gam)
summary(ceph_depth.lunar.re$lme)


#solar angle 
ceph_depth.solar.gam <- gam(log(bm_depth_ceph) ~ s(altitude, by = depth),data = km_bm_depth)
draw(ceph_depth.solar.gam, residuals = TRUE)
summary(ceph_depth.solar.gam)

ceph_depth.solar.re <- gamm(log(bm_depth_ceph) ~ s(altitude, by = depth),data = km_bm_depth, random = list(midoc.stn = ~ 1 ))
draw(ceph_depth.solar.re, residuals = TRUE)
summary(ceph_depth.solar.re$gam)
summary(ceph_depth.solar.re$lme)











#### TESTING GAMMA ######
#gamma instead of log for fish and solar angle 
library(gamm4)
m19<- gamm4(bm_g_m3 + 0.0001 ~ s(altitude, by = depth) , data = km_df_depth, family = Gamma(), random = ~(1|midoc.stn))
par(mfrow = c(2, 2))
gam.check(m19$gam)
summary(m19$gam)
draw(m19$gam, residuals = TRUE)

#m15 <- gamm(bm_g_m3 ~ s(altitude, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ), family = Gamma(link = "inverse"))
#draw(m15, residuals = TRUE)
#summary(m15$gam)



#solar angle
m_gaus <- gam(log(bm_g_m3) ~ s(altitude, by = depth) ,data = km_df_depth)
plot(m_gaus, residuals = TRUE,pages=1)
summary(m_gaus)
par(mfrow=c(2,2))
gam.check(m_gaus)

m_gamma <- gam(bm_g_m3+ 0.0001 ~ s(altitude, by = depth),data = km_df_depth, family = Gamma())
plot(m_gamma, residuals = TRUE,type="response")
summary(m_gamma)
par(mfrow=c(2,2))
gam.check(m_gamma)

m_re <- gamm(log(bm_g_m3) ~ s(altitude, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ))
draw(m_re$gam, residuals = TRUE)
summary(m_re$gam)



#PLOTTING RESIDUALS \
library(ggplot2)
library(mgcv)


##SUMMED##
plot_residuals <- function(model, data, color_var) {
  # Extract fitted values and residuals
  fitted_vals <- fitted(model)
  residuals <- residuals(model)
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Fitted = fitted_vals,
    Residuals = residuals,
    Color = data[[color_var]]
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Fitted, y = Residuals, color = Color)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(x = "Fitted Values", y = "Residuals", color = color_var)
  
  return(p)
}



# For model m4 (Day)
plot_residuals(m4, km_df_sum, "midoc.stn")




#FITTED VERSUS OBSERVED
plot_fitted_vs_observed <- function(model, data, title = "Fitted Values vs Observed Values") {
  # Extract fitted values
  fitted_vals <- fitted(model)
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Fitted = fitted_vals,
    Observed = log(data$bm_g_m3),  # Assuming your response variable is log-transformed
    Depth = data$depth
  )
  
  # Define your custom color palette
  depth_colours <- c(
    "0-200m" =  "#FFC000",
    "200-400m" = "#ff7c43",
    "400-600m" = "#C41E3A",
    "600-800m" = "#4A92C6",
    "800-1000m" = "darkblue"
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Fitted, y = Observed, color = Depth)) +
    geom_point(size = 2, alpha = 0.7) +  # Slightly transparent points
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    theme_minimal(base_size = 14) +  # Increase base font size
    labs(x = "Fitted Values", y = "Observed Values (log-transformed)", 
         color = "Depth", title = title) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),  # Center and enlarge title
      legend.position = "right",  # Move legend to the right
      aspect.ratio = 0.8  # Make the plot slightly wider than tall
    ) +
    scale_x_continuous(expand = expansion(mult = 0.1)) +  # Add 10% expansion to x-axis
    scale_y_continuous(expand = expansion(mult = 0.1)) +  # Add 10% expansion to y-axis
    scale_color_manual(values = depth_colours) +  # Use your custom color palette
    coord_fixed(ratio = 1)  # Ensure a 1:1 aspect ratio
  
  return(p)
}

#TOTAL TAXA - GELAT by depth 
plot_fitted_vs_observed(m10, km_df_depth, title = "Fitted Values vs Observed Values (Total taxa - excluding gelat, day)")
plot_fitted_vs_observed(m11, km_df_depth, title = "Fitted Values vs Observed Values (excluding gelat, lunar)")
plot_fitted_vs_observed(m12, km_df_depth, title = "Fitted Values vs Observed Values (excluding gelat, solar angle)")

#FISH - by depth
plot_fitted_vs_observed(m13, km_df_depth, title = "Fitted Values vs Observed Values (Fish - day)")
plot_fitted_vs_observed(m14, km_df_depth, title = "Fitted Values vs Observed Values (Fish - lunar fraction)")
plot_fitted_vs_observed(m15, km_df_depth, title = "Fitted Values vs Observed Values (Fish - solar angle)")

#SQUID - by depth
plot_fitted_vs_observed(m16, km_df_depth, title = "Fitted Values vs Observed Values (Squid - day)")
plot_fitted_vs_observed(m17, km_df_depth, title = "Fitted Values vs Observed Values (Squid - lunar fraction)")
plot_fitted_vs_observed(m18, km_df_depth, title = "Fitted Values vs Observed Values (Squid - solar angle)")

















##### PLOT OF TAXA TOTAL ######
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/km_df_environmental_variables.Rda")  

km_df <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")

label_midoc_stn <- function(x) {
  sub("MIDOC", "", x)
}


km_df <- km_df %>% 
  mutate(tax.grp = na_if(tax.grp, "NA")) %>%
  filter(!is.na(tax.grp))


#remove gelatinous 
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps", "mixed/other invertebrates")
km_df <-  km_df[!km_df$tax.grp %in% exclude_taxa, ]

library(RColorBrewer)

# Define a color-blind friendly palette
cb_palette <- brewer.pal(8, "Paired")

ggplot(km_df, aes(x = midoc.stn, y = bm_g_m3, fill = tax.grp)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_x_discrete(labels = label_midoc_stn) +
  scale_fill_manual(values = cb_palette) +
  labs(x = "Station", y = expression(paste("Biomass (g m"^"-3",")")), fill = "Taxonomic Group")


## Define the custom color palette

custom_palette <- c("#2e4057", "#4A92C6", "#FFC000",  "#ff7c43", "#C41E3A")

custom_palette <- c("#1F4E79", "#4A92C6", "#FFC000", "#7B3C5D","#E68A4F") 

# Create the stacked bar plot
stacked_bar_plot <- ggplot(km_df, aes(x = midoc.stn, y = bm_g_m3, fill = tax.grp)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.line.x = element_line(colour = NA, size = 0.5),
    axis.ticks.x = element_line(colour = "grey80", size = 0.5),
    axis.ticks.length = unit(3, "pt"),
    axis.title.x = element_text(margin = margin(t = 10))  # Increase space above x-axis label
  ) +
  scale_x_discrete(labels = label_midoc_stn, 
                   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(rows = var("depth")) +
  facet_wrap(~factor(depth, levels = c("0-200m", "200-400m", "400-600m", "600-800m", "800-1000m"),
     labels = c("0-200m", "200-400m", "400-600m", "600-800m", "800-1000m")), ncol = 1) +
  scale_fill_manual(values = custom_palette) +
  labs(x = "Midoc Station", y = expression(paste("Biomass (g m"^"-3",")")), fill = "Taxonomic Group")

# Display the plot
print(stacked_bar_plot)



output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_Bar_Chart")
output_filename <- "K4S_Plot_A1_Bar_Chart_Depth.png"
full_output_path <- file.path(output_directory, output_filename)



# Save the plot
ggsave(filename = full_output_path, plot = stacked_bar_plot, width =10, height = 8, dpi = 300, bg = "white")


