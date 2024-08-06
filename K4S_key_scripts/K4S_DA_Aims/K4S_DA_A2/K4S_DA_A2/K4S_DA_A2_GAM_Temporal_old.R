#library
library(mgcv)
library(gratia)
library(dplyr)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts")
setwd(d)
dir.exists(d)

#Summed data - one biom data point per station  
load("km_df_environmental_variables.Rda")
  #remove the 0-1000m
km_df <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")


  #remove gelatinous 
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps", "mixed/other invertebrates")
km_df <-  km_df[!km_df$tax.grp %in% exclude_taxa, ]

  #include 
#include_taxa <- c("fish")
#km_df <-  km_df[km_df$tax.grp %in% include_taxa, ]

#summarising the data 
  #didnt work, but now is working again
km_df_sum <- km_df %>%
  group_by(midoc.stn) %>%
  summarize(
    total_biomass = sum(bm_g_m3, na.rm = TRUE),
    across(-bm_g_m3, ~ first(.))
  )

# Adding day 
km_df_sum <- km_df_sum %>%
  mutate(day = as.numeric(as.POSIXct(start_time)) / (60 * 60 * 24))

#
m1 <- gam(log(total_biomass) ~ s(day),data = km_df_sum)
draw(m1, residuals = TRUE) 
summary(m1)

#Lunar fraction 
m2 <- gam(log(total_biomass) ~ s(lunar_fraction),data = km_df_sum)
draw(m2, residuals = TRUE) 
summary(m2)

 #Solar angle 
m3 <- gam(log(total_biomass) ~ s(altitude),data = km_df_sum)
draw(m3, residuals = TRUE) 
summary(m3)



 
#SUMMED DATA WITH DEPTH EXCLUDING GELATINOUS
load("km_df_environmental_variables.Rda")
#remove the 0-1000m
km_df <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")
#remove gelatinous 
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps", "mixed/other invertebrates")
km_df <-  km_df[!km_df$tax.grp %in% exclude_taxa, ]

# Adding day 
km_df <- km_df %>%
  mutate(day = as.numeric(as.POSIXct(start_time)) / (60 * 60 * 24))

km_df_sum_depth <- km_df %>%
  group_by(midoc.stn, depth) %>%
  summarize(
    biomass_sum = sum(bm_g_m3, na.rm = TRUE),
    across(c(-bm_g_m3), ~ first(.))
  )

#day 
m4 <- gam(log(biomass_sum) ~ s(day, by = depth),data = km_df_sum_depth)
draw(m4, residuals = TRUE)
summary(m4)

#lunar fraction
m5 <- gam(log(biomass_sum) ~ s(lunar_fraction, by = depth),data = km_df_sum_depth)
draw(m5, residuals = TRUE)
summary(m5)

#solar angle
m6 <- gam(log(biomass_sum) ~ s(altitude, by = depth),data = km_df_sum_depth)
draw(m6, residuals = TRUE)
summary(m6)






#FISH/SQUID
#Summed data with depth 
load("km_df_environmental_variables.Rda")
#remove the 0-1000m
km_df <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")
#remove gelatinous 
include_taxa <- c("cephalopods")
km_df <-  km_df[km_df$tax.grp %in% include_taxa, ]

# Adding day 
km_df <- km_df %>%
  mutate(day = as.numeric(as.POSIXct(start_time)) / (60 * 60 * 24))

km_df_sum_depth <- km_df %>%
  group_by(midoc.stn, depth) %>%
  summarize(
    biomass_sum = sum(bm_g_m3, na.rm = TRUE),
    across(c(-bm_g_m3), ~ first(.))
  )

#day 
m4 <- gam(log(biomass_sum) ~ s(day, by = depth),data = km_df_sum_depth)
draw(m4, residuals = TRUE)
summary(m4)

#lunar fraction
m5 <- gam(log(biomass_sum) ~ s(lunar_fraction, by = depth),data = km_df_sum_depth, family = poisson)
draw(m5, residuals = TRUE)
summary(m5)

#solar angle
m6 <- gam(log(biomass_sum) ~ s(altitude, by = depth),data = km_df_sum_depth)
draw(m6, residuals = TRUE)
summary(m6)

#GAMM for solar angle 
m7 <- gamm(log(biomass_sum) ~ s(altitude, by = depth) + s(lon_start, lat_start, bs = "sos"),data = km_df_sum_depth correlation = corGaus(form = ~ lon_start + lat_start))
summary(m7$gam)
plot(m7$lme)
par(mfrow = c(2, 2))
gam.check(m7$gam)

m7 <- gamm(log(biomass_sum) ~ s(altitude, by = depth), data = km_df_sum_depth, correlation = corExp(form = ~ depth | lon_start + lat_start, nugget = TRUE))

summary(m7$gam)
draw(m7, residuals = TRUE)

#Jitter
# Load required libraries
library(mgcv)
library(nlme)
\

#make a plot showing stacked bar charts of the biomass sum for each station, with the tax.grp as the fill colour dataset is km_df
ggplot(km_df, aes(x = midoc.stn, y = bm_g_m3, fill = tax.grp)) +
  geom_bar(stat = "identity") +#, position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #facet_grid(rows = var("depth")) +
  #facet_wrap(~factor(depth, levels = c("0-200m", "200-400m", "400-600m", "600-800m", "800-1000m"),
                  #   labels = c("0-200m", "200-400m", "400-600m", "600-800m", "800-1000m")), ncol = 1) +
  labs(x = "Station", y = "Biomass sum", fill = "tax.grp")


# Assuming your dataframe is called 'df' with columns:
# 'biomass', 'lunar_angle', 'lat', 'lon'

# Add a small amount of jitter to coordinates to avoid duplicate points
set.seed(123)  # for reproducibility
km_df_sum_depth$lon_jitter <- km_df_sum_depth$lon_start + runif(nrow(km_df_sum_depth), -0.00001, 0.00001)
km_df_sum_depth$lat_jitter <- km_df_sum_depth$lat_start + runif(nrow(km_df_sum_depth), -0.00001, 0.00001)

# Create a correlation structure
# Here we use an exponential correlation structure
correlation_structure <- corExp(form = ~ lon_jitter + lat_jitter, nugget = TRUE)

# Fit the GAM model with correlation structure
model <- gamm(log(biomass_sum) ~ s(altitude, by = depth),
              data = km_df_sum_depth,
              correlation = correlation_structure,
              method = "REML")

# Summary of the model
summary(model$gam)
summary(model$lme)

# Plot the model
plot(model$gam)
pl



#random effect 
m7 <- gamm(log(biomass_sum) ~ s(altitude, by = depth), data = km_df_sum_depth, random = list(midoc.stn = ~ 1 ))

#gamma instead of log 
m8 <- gam(biomass_sum ~ s(altitude, by = depth), data = km_df_sum_depth, family=Gamma())
par(mfrow = c(2, 2))
gam.check(m8)

#gamma instead of log 
library(gamm4)
m9 <- gamm4(biomass_sum + 0.0001 ~ s(altitude, by = depth) , data = km_df_sum_depth, family = Gamma(link = "inverse"), random = ~(1|midoc.stn))
par(mfrow = c(2, 2))
gam.check(m9$gam)
summary(m9$gam)
m7 <- gamm(log(biomass_sum) ~ s(altitude, by = depth), data = km_df_sum_depth)
summary(m7$gam)
par(mfrow = c(2, 2))
plot(m7$lme)


#Sanity check 
new_df <- km_df_sum_depth[, c("midoc.stn", "biomass_sum", "depth")]
new_df <- km_df[, c("midoc.stn", "bm_g_m3", "depth")]

original_subset <- km_df %>%
  filter(midoc.stn == "MIDOC01" & depth == "200-400m")

# Calculate the manual sum of bm_g_m3 for this subset
manual_sum <- sum(original_subset$bm_g_m3, na.rm = TRUE)

print(manual_sum)
