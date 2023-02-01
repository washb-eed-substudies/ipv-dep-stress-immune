rm(list=ls())

source(here::here("0-config.R"))
library(washb)
library(washbgam)
#devtools::install_github("washb-eed-substudies/washbgam")
#source(here::here("src/0-gam-functions.R"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Audrie/ipv-immune-analysis-dataset.RDS"))
colnames(d)

#merge in HH wealth
wealth <- read.csv(paste0(dropboxDir,"Data/Cleaned/Audrie/hhwealth.csv"))
colnames(wealth)

d <- left_join(d, wealth, by = c("dataid"))

#---------------------------------------------------------------
# post-hoc analyses
#---------------------------------------------------------------

# -Maternal IPV during pregnancy vs. Th1 cytokines (IL-12 and IFN-y) together
# -Maternal IPV during pregnancy vs. Th17 cytokines (IL-17A and IL-21) together
# -Maternal IPV during pregnancy vs. IL-12 individually
# -Maternal IPV during pregnancy vs. IFNy individually
# -Maternal IPV during pregnancy vs. IL-17A individually
# -Maternal IPV during pregnancy vs. IL-21 individually
# 
# -Y1 maternal CES-D (continuous) vs. Th1 cytokines (IL-12 and IFN-y) together 
# -Y1 maternal CES-D (continuous) vs. Th17 cytokines (IL-17A and IL-21) together 
# -Y1 maternal CES-D (continuous) vs. IL-12 individually 
# -Y1 maternal CES-D (continuous) vs. IFNy individually
# -Y1 maternal CES-D (continuous) vs. IL-17A individually 
# -Y1 maternal CES-D (continuous) vs. IL-21 individually




#### Create new outcomes
# Th1 cytokines (IL-12 and IFN-y) together
summary(d$il12_t2)
summary(d$ifng_t2)
summary(scale(d$il12_t2)[,1])
summary(scale(d$ifng_t2)[,1])
summary(scale(d$il12_t2)[,1] + scale(d$ifng_t2)[,1])
d$th1_z_t2 <- scale(d$il12_t2)[,1] + scale(d$ifng_t2)[,1]
d$th1_z_t3 <- scale(d$il12_t3)[,1] + scale(d$ifng_t3)[,1]

d$th2_z_t2 <- scale(d$il4_t2)[,1] + scale(d$il5_t2)[,1] + scale(d$il13_t2)[,1]
d$th2_z_t3 <- scale(d$il4_t3)[,1] + scale(d$il5_t3)[,1] + scale(d$il13_t3)[,1]


# Th17 cytokines (IL-17A and IL-21) together
d$th17_z_t2 <- scale(d$il17_t2)[,1] + scale(d$il21_t2)[,1]
d$th17_z_t3 <- scale(d$il17_t3)[,1] + scale(d$il21_t3)[,1]

#### Loop over exposure-outcome pairs ####


Xvars_t2 <- c("viol_any_preg","cesd_sum_t2","cesd_sum_t2_binary" )            

# Outcomes: 
# Yvars <- c("th1_z_t2", "th17_z_t2", "t2_ln_il12", "t2_ln_il21", "t2_ln_il17", "t2_ln_ifn",
#            "th1_z_t3", "th17_z_t3", "t3_ln_il12", "t3_ln_il21", "t3_ln_il17", "t3_ln_ifn")
Yvars <- c("th1_z_t2", "t2_ln_il12", "t2_ln_ifn", "th2_z_t2", "t2_ln_il4", "t2_ln_il5", "t2_ln_il13",
           "th17_z_t2", "t2_ln_il17", "t2_ln_il21", "th1_z_t3", "t3_ln_il12", "t3_ln_ifn", "th2_z_t3",  
           "t3_ln_il4", "t3_ln_il5", "t3_ln_il13", "th17_z_t3", "t3_ln_il17", "t3_ln_il21")
#Check that Yvars are in the data
Yvars[!(Yvars %in% colnames(d))]


i=Xvars_t2[1]
j=Yvars[1]

#Fit models
posthoc_models <- NULL
for(i in Xvars_t2){
  for(j in Yvars){
    cat(i,"\n")
    cat(j,"\n")
    if(grepl("_t2",j)|grepl("t2_",j)){
      Ws = Wvars_t2_outcomes[[i]]
    }else{
      Ws = Wvars_t3_outcomes[[i]]
    }
    
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    posthoc_models <- bind_rows(posthoc_models, res)
  }
}


#Get primary contrasts
posthoc_res <- NULL
for(i in 1:nrow(posthoc_models)){
  res <- data.frame(X=posthoc_models$X[i], Y=posthoc_models$Y[i])
  preds <- predict_gam_diff(fit=posthoc_models$fit[i][[1]], d=posthoc_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binaryX=TRUE)
  posthoc_res <-  bind_rows(posthoc_res , preds$res)
}

#P-value correction
posthoc_res <- posthoc_res %>% group_by(X) %>% 
  mutate(corrected.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

#Make list of plots
posthoc_plot_list <- NULL
posthoc_plot_data <- NULL
for(i in 1:nrow(posthoc_models)){
  res <- data.frame(X=posthoc_models$X[i], Y=posthoc_models$Y[i])
  simul_plot <- gam_simul_CI(posthoc_models$fit[i][[1]], posthoc_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  posthoc_plot_list[[i]] <-  simul_plot$p
  posthoc_plot_data <-  bind_rows(posthoc_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(posthoc_models, here("models/posthoc_unadj_models.RDS"))

#Save results
saveRDS(posthoc_res, here("results/post-hoc/posthoc_unadj_res.RDS"))
write.csv(posthoc_res, here("results/post-hoc/posthoc_unadj_res.csv"))

#Save plot data
saveRDS(posthoc_plot_data, here("figure-data/posthoc_unadj_spline_data.RDS"))

