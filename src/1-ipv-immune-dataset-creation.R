

rm(list=ls())

source(here::here("0-config.R"))
#devtools::install_github("washb-eed-substudies/washbgam")
#source(here::here("src/0-gam-functions.R"))



d <- read.csv(paste0(dropboxDir,"Data/Cleaned/Audrie/bangladesh-dm-ee-ipv-cesd-pss-covariates-immunelab.csv"))
colnames(d)

#Load and merge cleaned outcomes
Yvars <- readRDS(paste0(dropboxDir,"Data/Cleaned/Audrie/bangladesh-immune-analysis-dataset.rds"))
# sum_score_data <- read.csv(paste0(dropboxDir,"Data/Cleaned/Audrie/child immune sum scores.csv")) %>% select('childid', 'sumscore_t2_Z', 'sumscore_t3_Z')
# Yvars <- left_join(Yvars, sum_score_data, by="childid")
Yvars <- Yvars %>% select(childid, t2_ratio_pro_il10, t2_ratio_il2_il10, t2_ratio_gmc_il10, t2_ratio_th1_il10, t2_ratio_th2_il10,     
                                                                   t2_ratio_th17_il10, t2_ratio_th1_th2, t2_ratio_th1_th17, t2_ln_agp, t2_ln_crp, sumscore_t2_Z, t2_ln_ifn,t3_ratio_pro_il10, t3_ratio_il2_il10, t3_ratio_gmc_il10, t3_ratio_th1_il10, t3_ratio_th2_il10,     
                                                                   t3_ratio_th17_il10, t3_ratio_th1_th2, t3_ratio_th1_th17, t3_ln_crp, t3_ln_agp, sumscore_t3_Z, t3_ln_ifn)
dim(Yvars)
duplicate_vars<-colnames(Yvars)[(colnames(Yvars) %in% colnames(d))]
duplicate_vars <- duplicate_vars[duplicate_vars!="childid"]
colnames(Yvars)[!(colnames(Yvars) %in% colnames(d))]
d <- d[,!(colnames(d) %in% duplicate_vars)]

dim(d)
dfull <- left_join(d, Yvars, by="childid")
dim(dfull)

#Check merge
summary(Yvars$sumscore_t2_Z)
summary(dfull$sumscore_t2_Z)

#Save new dataset
saveRDS(dfull, paste0(dropboxDir,"Data/Cleaned/Audrie/ipv-immune-analysis-dataset.RDS"))
