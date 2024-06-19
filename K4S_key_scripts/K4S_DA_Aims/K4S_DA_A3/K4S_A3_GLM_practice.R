library(graticule)
library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(readr)
library(sf)
library(lme4)
library(MASS)
library(mgcv)

usr <- Sys.info()["user"]
d<- paste0("/Users/", usr, "/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/KPS_symposium_extended_abstract")
setwd(d)
dir.exists(d)

km <- readRDS("../derived data/midoc_stations_checked.rds")
km$midoc.n <- as.numeric(substr(km$midoc.stn, 6,7))
tmp <- read_csv("../source data/midoc_stations_zones.csv")
km <- inner_join(km, tmp); rm(tmp)
tmp <- read_csv(("../source data/midoc_crepuscular.csv"))
km <- inner_join(km, tmp); rm(tmp)
tmp <- readRDS("../derived data/codend_taxa_biomass.rds")
km <- inner_join(km, tmp); rm(tmp)
tmp <- readRDS("../derived data/midoc_stations_envdata.rda")
km <- inner_join(km, tmp); rm(tmp)

# GLM model of biomass and Tmin_depth (poisson)
m1 <- glm(bm ~ Tmin_depth, data=km, family=poisson(link="log"))
summary(m1)

#GLM model of biomass and Smax (negative binomial)
m2 <-glm.nb(bm ~ Smax, data = km)
summary(m2)

#mixed effects model of biomass and Smax with midoc as a random effect 
m3 <- lmer(bm ~ Smax + (1|midoc.n), data = km)
summary(m3)

#looking into residuals of m3
residuals <- resid(m3)
fitted <- fitted(m3)

plot(fitted, residuals, 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red")

#GAM of biomass and Smax with midoc as a random effect
m4 <- gam(bm ~ s(Smax), data = km, family = nb(theta = 1.5))
summary(m4)
plot(m4, pages = 1)


# 1. Plot residuals vs fitted values
plot(m4, residuals = TRUE, pch = 1, cex = 0.7, main = "Residuals vs Fitted")

# 2. Q-Q plot for residuals
qqnorm(resid(m4), main = "Q-Q Plot of Residuals")
qqline(resid(m4), col = "red")

# 3. Histogram of residuals
hist(resid(m4), breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

# 4. Standardized residuals vs fitted values
plot(fitted(m4), rstandard(m4), xlab = "Fitted Values", ylab = "Standardized Residuals", main = "Standardized Residuals vs Fitted")
abline(h = 0, col = "red")


