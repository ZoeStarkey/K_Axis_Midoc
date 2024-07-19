#library
library(mgcv)

#linear model

m1 <- gam(bm_g_m3 ~ s(TSM) + s(CUR) + s(CHLA), data = km_df, family = poisson())
summary(m1)


plot(m1)


#negative binomial gam
m2 <- gam(bm_g_m3 ~ s(TSM), data = km_df, family = gaussian())
summary(m2)
plot(m2)
