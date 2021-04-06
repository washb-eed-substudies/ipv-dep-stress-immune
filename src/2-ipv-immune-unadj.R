rm(list=ls())
 
source(here::here("0-config.R"))
#devtools::install_github("washb-eed-substudies/washbgam")
#source(here::here("src/0-gam-functions.R"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Audrie/ipv-immune-analysis-dataset.RDS"))
colnames(d)
#### Loop over exposure-outcome pairs ####

#### Hypothesis 1:
# 1.	Maternal exposure to IPV during pregnancy, IPV during the child's first year of life, 
#and cumulative lifetime exposure to IPV at Year 2 is associated with child systemic inflammation at 
#Years 1 and 2 (defined as higher concentrations of pro-inflammatory factors and/or lower concentrations 
#of anti-inflammatory/immuno-regulatory factors).
# Exposures: Maternal exposure to IPV during pregnancy, IPV during the child's first year of life, and 
#cumulative lifetime exposure to IPV at Year 2 as measured by the WHO Women's Health and Life Experiences Survey
Xvars_t2 <- c("viol_any_preg","viol_any_t2")            
Xvars_t3 <- c("life_viol_any_t3")            

# Outcomes: Child sum score of systemic inflammation, Th1/Th2, 
#Th1/Th17, Th1/IL-10, Th2/IL-10, Th17/IL-10, 
#GM-CSF/IL-10, and IL-2/IL-10 cytokine ratios, IGF-1, CRP, and AGP at Years 1 and 2.
Yvars <- c("t2_ratio_th1_th2","t3_ratio_th1_th2",
           "t2_ratio_th1_th17","t3_ratio_th1_th17", "t2_ratio_th1_il10","t3_ratio_th1_il10",
           "t2_ratio_th2_il10","t3_ratio_th2_il10", "t2_ratio_th17_il10","t3_ratio_th17_il10",
           "t2_ratio_gmc_il10","t3_ratio_gmc_il10", "t2_ratio_il2_il10","t3_ratio_il2_il10",
           "t2_ln_igf","t3_ln_igf","t2_ln_crp","t3_ln_crp","t2_ln_agp","t3_ln_agp","t2_ln_ifn","t3_ln_ifn", "sumscore_t2_Z", "sumscore_t3_Z")
Yvars_t3 <- c("t3_ratio_th1_th2",
              "t3_ratio_th1_th17","t3_ratio_th1_il10",
              "t3_ratio_th2_il10", "t3_ratio_th17_il10",
              "t3_ratio_gmc_il10", "t3_ratio_il2_il10",
              "t3_ln_igf","t3_ln_crp","t3_ln_agp","t3_ln_ifn", "sumscore_t3_Z")


# igf_t3	life_viol_any_t3 -6.3614348
# agp_t3	viol_any_t2 0.0379352
# t2_ratio_th17_il10	viol_any_preg 0.1023707	
# summary(glm(igf_t3 ~	life_viol_any_t3, data=d))
# summary(glm(agp_t3 ~	viol_any_t2, data=d))
# summary(glm(t2_ratio_th17_il10 ~	viol_any_preg, data=d))
# 
# 
# summary(glm(igf_t3 ~	agp_t3, data=d))
# res_unadj <- fit_RE_gam(d=d, X="agp_t3", Y="igf_t3",  W=NULL)
# preds <- predict_gam_diff(fit=res_unadj$fit, d=res_unadj$dat, quantile_diff=c(0.25,0.75), Xvar="agp_t3", Yvar="igf_t3")
# preds$res


#Fit models
H1_models <- NULL
for(i in Xvars_t2){
  for(j in Yvars){
    cat(i,"\n")
    cat(j,"\n")
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H1_models <- bind_rows(H1_models, res)
  }
}
for(i in Xvars_t3){
  for(j in Yvars_t3){
    cat(i,"\n")
    cat(j,"\n")
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H1_models <- bind_rows(H1_models, res)
  }
}

#Get primary contrasts
H1_res <- NULL
for(i in 1:nrow(H1_models)){
  res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
  preds <- predict_gam_diff(fit=H1_models$fit[i][[1]], d=H1_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binaryX=TRUE)
  H1_res <-  bind_rows(H1_res , preds$res)
}

#Make list of plots
H1_plot_list <- NULL
H1_plot_data <- NULL
for(i in 1:nrow(H1_models)){
  res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
  simul_plot <- gam_simul_CI(H1_models$fit[i][[1]], H1_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1_plot_list[[i]] <-  simul_plot$p
  H1_plot_data <-  rbind(H1_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H1_models, here("models/H1_models.RDS"))

#Save results
saveRDS(H1_res, here("results/unadjusted/H1_res.RDS"))


#Save plots
#saveRDS(H1_plot_list, here("figure-objects/H1_unadj_splines.RDS"))

#Save plot data
saveRDS(H1_plot_data, here("figure-data/H1_unadj_spline_data.RDS"))



#### Hypothesis 2: 
# 2.	Parental stress measured at Year 2 is associated with concurrent child systemic inflammation (defined as higher concentrations of pro-inflammatory factors and/or lower concentrations of anti-inflammatory/immuno-regulatory factors).
# Exposures: Maternal and paternal Perceived Stress Scale (PSS) scores at Year 2
Xvars <- c("pss_sum_mom_t3", "pss_sum_dad_t3")
# Outcomes: Child sum score of systemic inflammation, Th1/Th2, Th1/Th17, Th1/IL-10, Th2/IL-10, Th17/IL-10, GM-CSF/IL-10, and 
#IL-2/IL-10 cytokine ratios, IGF-1, CRP, and AGP at Year 2.

#Fit models
H2_models <- NULL
for(i in Xvars){
  for(j in Yvars_t3){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H2_models <- bind_rows(H2_models, res)
  }
}

#Get primary contrasts
H2_res <- NULL
for(i in 1:nrow(H2_models)){
  res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
  preds <- predict_gam_diff(fit=H2_models$fit[i][[1]], d=H2_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2_res <-  bind_rows(H2_res , preds$res)
}

#Make list of plots
H2_plot_list <- NULL
H2_plot_data <- NULL
for(i in 1:nrow(H2_models)){
  res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
  simul_plot <- gam_simul_CI(H2_models$fit[i][[1]], H2_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2_plot_list[[i]] <-  simul_plot$p
  H2_plot_data <-  rbind(H2_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H2_models, here("models/H2_models.RDS"))

#Save results
saveRDS(H2_res, here("results/unadjusted/H2_res.RDS"))


#Save plots
#saveRDS(H2_plot_list, here("figure-objects/H2_unadj_splines.RDS"))

#Save plot data
saveRDS(H2_plot_data, here("figure-data/H2_unadj_spline_data.RDS"))



#### Hypothesis 3: 
# 3.	Maternal depressive symptoms, measured on a continuous scale at Years 1 and 2, is associated with child systemic inflammation at Years 1 and 2 (defined as higher concentrations of pro-inflammatory factors and/or lower concentrations of anti-inflammatory/immuno-regulatory factors). 
# Exposures: Maternal Center for Epidemiologic Studies (CES-D) Scale score at Years 1 and 2
Xvars_t2 <- c("cesd_sum_t2", "cesd_sum_t2_binary")
Xvars_t3 <- c("cesd_sum_ee_t3", "cesd_sum_ee_t3_binary")
# Outcomes: Child sum score of systemic inflammation, Th1/Th2, Th1/Th17, Th1/IL-10, Th2/IL-10, Th17/IL-10, GM-CSF/IL-10, and IL-2/IL-10 cytokine ratios, IGF-1, CRP, and AGP at Years 1 and 2.

#Fit models
H3_models <- NULL
for(i in Xvars_t2){
  for(j in Yvars){
    cat(i,"\n")
    cat(j,"\n")
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H3_models <- bind_rows(H3_models, res)
  }
}
for(i in Xvars_t3){
  for(j in Yvars_t3){
    cat(i,"\n")
    cat(j,"\n")
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H3_models <- bind_rows(H3_models, res)
  }
}

#Get primary contrasts
H3_res <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  preds <- predict_gam_diff(fit=H3_models$fit[i][[1]], d=H3_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H3_res <-  bind_rows(H3_res , preds$res)
}

#Make list of plots
H3_plot_list <- NULL
H3_plot_data <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  simul_plot <- gam_simul_CI(H3_models$fit[i][[1]], H3_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H3_plot_list[[i]] <-  simul_plot$p
  H3_plot_data <-  rbind(H3_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H3_models, here("models/H3_models.RDS"))

#Save results
saveRDS(H3_res, here("results/unadjusted/H3_res.RDS"))


#Save plots
#saveRDS(H3_plot_list, here("figure-objects/H3_unadj_splines.RDS"))

#Save plot data
saveRDS(H3_plot_data, here("figure-data/H3_unadj_spline_data.RDS"))

