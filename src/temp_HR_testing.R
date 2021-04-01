
source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Audrie/ipv-immune-analysis-dataset.RDS"))
colnames(d)

#merge in HH wealth
wealth <- read.csv(paste0(dropboxDir,"Data/Cleaned/Caitlin/real_ids_hhwealth_quart.csv"))
colnames(wealth)

d <- left_join(d, wealth, by = c("dataid", "clusterid",  "block"))
table(is.na(d$HHwealth))

Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
         "floor", "HHwealth", "tr", "cesd_sum_t2", 
         "ari7d_t2", "diar7d_t2", "nose7d_t2", "life_viol_any_t3")

res_adj <- fit_HR_GAM(d=d, X="t3_ratio_th1_th17", Y="sex_viol_12m_t3", age="ageday_at3",  W=Wvars)

#need to update to get HR
preds <- predict_gam_HR(fit=res_adj$fit, d=res_adj$dat, quantile_diff=c(0.25,0.75), Xvar="t3_ratio_th1_th17", Yvar="sex_viol_12m_t3", binaryX=F)
preds$res
# 
# #need to make predict_gam_HE function that exponentiates the difference and CI
# 
# X="t3_ratio_th1_th17"
# Y="sex_viol_12m_t3"
# age="ageday_at3"
# W=Wvars
# forcedW=NULL
# V=NULL
# id="clusterid"
# pval = 0.2
# print=TRUE
# 
# 
# fit=res_adj$fit
# d=res_adj$dat
# quantile_diff=c(0.25,0.75)
# Xvar="t3_ratio_th1_th17"
# Yvar="sex_viol_12m_t3"
# binaryX=F
# 
