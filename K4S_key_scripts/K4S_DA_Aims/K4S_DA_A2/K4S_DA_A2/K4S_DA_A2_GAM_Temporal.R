
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
load("km_df_sum.Rda")

#Day
m1 <- gam(log(bm_g_m3) ~ s(day),data = km_df_sum, random = list(midoc.stn = ~ 1 ))
draw(m1, residuals = TRUE) 
summary(m1)

#Lunar fraction - illuminated disk 
m2 <- gam(log(bm_g_m3) ~ s(lunar_fraction),data = km_df_sum, random = list(midoc.stn = ~ 1 ))
draw(m2, residuals = TRUE) 
summary(m2)

#Solar angle 
m3 <- gam(log(bm_g_m3) ~ s(altitude),data = km_df_sum, random = list(midoc.stn = ~ 1 ))
draw(m3, residuals = TRUE) 
summary(m3)




#  2. SUMMED BIOMASS - FISH
#Load in the dataframe 
load("km_df_sum.Rda")

#Filter only for fish 
include_taxa <- c("fish")
km_df_depth <-  km_df_sum[km_df_sum$tax.grp %in% include_taxa, ]

#Day
m4 <- gam(log(bm_g_m3) ~ s(day),data = km_df_sum, random = list(midoc.stn = ~ 1 ))
draw(m4, residuals = TRUE) 
summary(m4)

#Lunar fraction - illuminated disk 
m5 <- gam(log(bm_g_m3) ~ s(lunar_fraction),data = km_df_sum, random = list(midoc.stn = ~ 1 ))
draw(m5, residuals = TRUE) 
summary(m5)

#Solar angle 
m6 <- gam(log(bm_g_m3) ~ s(altitude),data = km_df_sum, random = list(midoc.stn = ~ 1 ))
draw(m6, residuals = TRUE) 
summary(m6)




# 3. SUMMED BIOMASS - SQUID
#Load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/km_df_sum.Rda")

#Filter only for fish 
include_taxa <- c("cephalopods")
km_df_depth <-  km_df_sum[km_df_sum$tax.grp %in% include_taxa, ]

#Day
m7 <- gam(log(bm_g_m3) ~ s(day),data = km_df_sum, random = list(midoc.stn = ~ 1 ))
draw(m7, residuals = TRUE) 
summary(m7)

#Lunar fraction - illuminated disk 
m8 <- gam(log(bm_g_m3) ~ s(lunar_fraction),data = km_df_sum, random = list(midoc.stn = ~ 1 ))
draw(m8, residuals = TRUE) 
summary(m8)

#Solar angle 
m9 <- gam(log(bm_g_m3) ~ s(altitude),data = km_df_sum, random = list(midoc.stn = ~ 1 ))
draw(m9, residuals = TRUE) 
summary(m9)


############. BIOMASS SEPARATED BY DEPTH. ##################



#BIOMASS SEPARATED BY DEPTH - EXCLUDING GELATINOUS 
#load in the dataframe 
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/km_df_depth.Rda")

#summing the biomass for each depth bin at each midoc station 
km_df_depth <- km_df_depth %>%
 group_by(midoc.stn, depth) %>%
  summarize(
    bm_g_m3 = sum(bm_g_m3, na.rm = TRUE),
    across(c(-bm_g_m3), ~ first(.))
   )

#day 
m10 <- gam(log(bm_g_m3) ~ s(day, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ))
draw(m10, residuals = TRUE)
summary(m10)

#lunar fraction
m11 <- gam(log(bm_g_m3) ~ s(lunar_fraction, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ))
draw(m11, residuals = TRUE)
summary(m11)

#solar angle
m12 <- gam(log(bm_g_m3) ~ s(altitude, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ))
draw(m12, residuals = TRUE)
summary(m12)




#BIOMASS SEPARATED BY DEPTH - FISH
#load in the dataframe 
load("km_df_depth.Rda")

#Filter only for fish 
include_taxa <- c("fish")
km_df_depth <-  km_df_depth[km_df_depth$tax.grp %in% include_taxa, ]

#day
m13 <- gam(log(bm_g_m3) ~ s(day, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ))
draw(m13, residuals = TRUE)
summary(m13)

#lunar fraction
m14 <- gam(log(bm_g_m3) ~ s(lunar_fraction, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ))
draw(m14, residuals = TRUE)
summary(m14)

#solar angle
m15 <- gam(log(bm_g_m3) ~ s(altitude, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ))
draw(m15, residuals = TRUE)
summary(m15)




#BIOMASS SEPARATED BY DEPTH - CEPHALOPODS
#load in the dataframe 
load("km_df_depth.Rda")

#Filter only for squid
include_taxa <- c("cephalopods")
km_df_depth <-  km_df_depth[km_df_depth$tax.grp %in% include_taxa, ]

#day
m16 <- gam(log(bm_g_m3) ~ s(day, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ))
draw(m16, residuals = TRUE)
summary(m16)

#lunar fraction
m17 <- gam(log(bm_g_m3) ~ s(lunar_fraction, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ))
draw(m17, residuals = TRUE)
summary(m17)

#solar angle
m18 <- gam(log(bm_g_m3) ~ s(altitude, by = depth),data = km_df_depth, random = list(midoc.stn = ~ 1 ))
draw(m18, residuals = TRUE)
summary(m18)




#### TESTING GAMMA ######
#gamma instead of log for fish and solar angle 
library(gamm4)
m19<- gamm4(bm_g_m3 + 0.0001 ~ s(altitude, by = depth) , data = km_df_depth, family = Gamma(link = "inverse"), random = ~(1|midoc.stn))
par(mfrow = c(2, 2))
gam.check(m19$gam)
summary(m19$gam)



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

#make a plot showing stacked bar charts of the biomass sum for each station, with the tax.grp as the fill colour dataset is km_df
ggplot(km_df, aes(x = midoc.stn, y = bm_g_m3, fill = tax.grp)) +
  geom_bar(stat = "identity") +#, position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        (background = colour = "white"),
        ) +
  scale_x_discrete(labels = label_midoc_stn) +
  #facet_grid(rows = var("depth")) +
  #facet_wrap(~factor(depth, levels = c("0-200m", "200-400m", "400-600m", "600-800m", "800-1000m"),
  #   labels = c("0-200m", "200-400m", "400-600m", "600-800m", "800-1000m")), ncol = 1) +
  labs(x = "Station", y = "Biomass sum", fill = "tax.grp") + 
    guides(fill = guide_legend(title = "Taxonomy group"))


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


