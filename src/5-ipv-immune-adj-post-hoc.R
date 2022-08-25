rm(list=ls())

source(here::here("0-config.R"))
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

#---------------------------------------------------------------
# set covariates
#---------------------------------------------------------------

#Set list of adjustment variables
#Make vectors of adjustment variable names
#removed roof because of low variability
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
         "floor", "HHwealth", "tr", "cesd_sum_t2", 
         "ari7d_t2", "diar7d_t2", "nose7d_t2", "life_viol_any_t3")

Wvars[!(Wvars %in% colnames(d))]

Wvars_t2_outcomes = list(
  cesd_sum_t2 = c(Wvars,"ageday_bt2","month_bt2", "cesd_month_t2"),
  viol_any_preg = c(Wvars,"ageday_bt2","month_bt2",	"mhle_month_t3")
)

Wvars_t3_outcomes = list(
  cesd_sum_t2 = c(Wvars, "ageday_bt3",	"cesd_month_t2",	"month_bt3"),
  viol_any_preg = c(Wvars, "ageday_bt3","month_bt3", "mhle_month_t3")
)



#### Create new outcomes
# Th1 cytokines (IL-12 and IFN-y) together
summary(d$il12_t2)
summary(d$ifng_t2)
summary(scale(d$il12_t2)[,1])
summary(scale(d$ifng_t2)[,1])
summary(scale(d$il12_t2)[,1] + scale(d$ifng_t2)[,1])
d$th1_z_t2 <- scale(d$il12_t2)[,1] + scale(d$ifng_t2)[,1]
d$th1_z_t3 <- scale(d$il12_t3)[,1] + scale(d$ifng_t3)[,1]

# Th17 cytokines (IL-17A and IL-21) together
d$th17_z_t2 <- scale(d$il17_t2)[,1] + scale(d$il21_t2)[,1]
d$th17_z_t3 <- scale(d$il17_t3)[,1] + scale(d$il21_t3)[,1]

#### Loop over exposure-outcome pairs ####


Xvars_t2 <- c("viol_any_preg","cesd_sum_t2")            

# Outcomes: 
Yvars <- c("th1_z_t2", "th17_z_t2", "t2_ln_il12", "t2_ln_il21", "t2_ln_il17", "t2_ln_ifn",
           "th1_z_t3", "th17_z_t3", "t3_ln_il12", "t3_ln_il21", "t3_ln_il17", "t3_ln_ifn")


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
    
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Ws)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
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
saveRDS(posthoc_models, here("models/posthoc_adj_models.RDS"))

#Save results
saveRDS(posthoc_res, here("results/posthoc_adj_res.RDS"))

#Save plot data
saveRDS(posthoc_plot_data, here("figure-data/posthoc_adj_spline_data.RDS"))

