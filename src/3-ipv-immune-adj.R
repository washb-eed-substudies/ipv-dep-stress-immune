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
table(is.na(d$HHwealth))

#---------------------------------------------------------------
# set covariates
#---------------------------------------------------------------

# .	Enrollment characteristics:
#   o	Child sex
# o	Child birth order (first born, second born or greater)
# o	Mother's age (years)
# o	Mother's height (cm)
# o	Mother's education level (no education, primary, secondary)
# o	Household food insecurity (4-level HFIAS categories) 
# o	Number of children < 18 years in the household
# o	Number of individuals living in the compound
# o	Distance (in minutes) to the household's primary drinking water source
# o	Housing materials (floor, walls, roof)
# o	Asset-based household-wealth variable (continuous), calculated from the first principal component of a principal components analysis of the following household assets: electricity, wardrobe, table, chair or bench, khat, chouki, working radio, working black/white or color television, refrigerator, bicycle, motorcycle, sewing machine, mobile phone, land phone, number of cows, number of goats, number of chickens.
# 
# We will also test characteristics measured during follow-up (see directed acyclic graph (DAG) in Figure 1 below):
# .	Month of measurement 
# .	Child age (days)
# .	Treatment arm (control or combined nutrition, water, sanitation, and handwashing intervention (N+WSH)).    



#Set list of adjustment variables
#Make vectors of adjustment variable names
#removed roof because of low variability
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", 
         "floor", "HHwealth", "tr", "cesd_sum_t2", 
         "ari7d_t2", "diar7d_t2", "nose7d_t2", "life_viol_any_t3")

Wvars[!(Wvars %in% colnames(d))]

Wvars_t2_outcomes = list(
  viol_any_t2 = c(Wvars,"ageday_bt2","month_bt2",	"mhle_month_t3"),
  cesd_sum_t2 = c(Wvars,"ageday_bt2","month_bt2", "cesd_month_t2"),
  viol_any_preg = c(Wvars,"ageday_bt2","month_bt2",	"mhle_month_t3")
)

Wvars_t3_outcomes = list(
  life_viol_any_t3 = c(Wvars, "ageday_bt3","month_bt3", "mhle_month_t3"),
  viol_any_preg = c(Wvars, "ageday_bt3","month_bt3", "mhle_month_t3"),
  viol_any_t2 = c(Wvars, "ageday_bt3","month_bt3", "mhle_month_t3"),
  pss_sum_mom_t3 = c(Wvars, "ageday_bt3","month_bt3", "mhle_month_t3"),
  pss_sum_dad_t3 = c(Wvars, "ageday_bt3","month_bt3", "pss_dad_month_t3"),
  cesd_sum_ee_t3 = c(Wvars, "ageday_bt3","month_bt3", "mhle_month_t3"),
  cesd_sum_ee_t3_binary = c(Wvars, "ageday_bt3","month_bt3", "mhle_month_t3"),
  cesd_sum_t2 = c(Wvars, "ageday_bt3",	"cesd_month_t2",	"month_bt3"),
  cesd_sum_t2_binary = c(Wvars, "ageday_bt3",	"cesd_month_t2",	"month_bt3")
)


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
           "t2_ln_igf","t3_ln_igf","t2_ln_crp","t3_ln_crp","t2_ln_agp","t3_ln_agp","t2_ln_ifn","t3_ln_ifn", "sumscore_t2_Z")
Yvars_t3 <- c("t3_ratio_th1_th2",
              "t3_ratio_th1_th17","t3_ratio_th1_il10",
              "t3_ratio_th2_il10", "t3_ratio_th17_il10",
              "t3_ratio_gmc_il10", "t3_ratio_il2_il10",
              "t3_ln_igf","t3_ln_crp","t3_ln_agp","t3_ln_ifn", "sumscore_t3_Z")


#Fit models
H1_models <- NULL
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
    H1_models <- bind_rows(H1_models, res)
  }
}
for(i in Xvars_t3){
  for(j in Yvars_t3){
    cat(i,"\n")
    cat(j,"\n")
    Ws = Wvars_t3_outcomes[[i]]
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Ws)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
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
  H1_plot_data <-  bind_rows(H1_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H1_models, here("models/H1_adj_models.RDS"))

#Save results
saveRDS(H1_res, here("results/H1_adj_res.RDS"))


#Save plots
#saveRDS(H1_plot_list, here("figure-objects/H1_adj_splines.RDS"))

#Save plot data
saveRDS(H1_plot_data, here("figure-data/H1_adj_spline_data.RDS"))



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
    Ws = Wvars_t3_outcomes[[i]]
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Ws)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
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
  H2_plot_data <-  bind_rows(H2_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H2_models, here("models/H2_adj_models.RDS"))

#Save results
saveRDS(H2_res, here("results/H2_adj_res.RDS"))


#Save plots
#saveRDS(H2_plot_list, here("figure-objects/H2_adj_splines.RDS"))

#Save plot data
saveRDS(H2_plot_data, here("figure-data/H2_adj_spline_data.RDS"))



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
    if(grepl("_t2",j)|grepl("t2_",j)){
      Ws = Wvars_t2_outcomes[[i]]
    }else{
      Ws = Wvars_t3_outcomes[[i]]
    }
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Ws)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_models <- bind_rows(H3_models, res)
  }
}
for(i in Xvars_t3){
  for(j in Yvars_t3){
    cat(i,"\n")
    cat(j,"\n")
    Ws = Wvars_t3_outcomes[[i]]
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Ws)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_models <- bind_rows(H3_models, res)
  }
}

#Get primary contrasts
H3_res <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  if (grepl("binary", H3_models$X[i])){bin = TRUE}
  else {bin=FALSE}
  preds <- predict_gam_diff(fit=H3_models$fit[i][[1]], d=H3_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binaryX = bin)
  H3_res <-  bind_rows(H3_res , preds$res)
}

#Make list of plots
H3_plot_list <- NULL
H3_plot_data <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  simul_plot <- gam_simul_CI(H3_models$fit[i][[1]], H3_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H3_plot_list[[i]] <-  simul_plot$p
  H3_plot_data <-  bind_rows(H3_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H3_models, here("models/H3_adj_models.RDS"))

#Save results
saveRDS(H3_res, here("results/H3_adj_res.RDS"))


#Save plots
#saveRDS(H3_plot_list, here("figure-objects/H3_adj_splines.RDS"))

#Save plot data
saveRDS(H3_plot_data, here("figure-data/H3_adj_spline_data.RDS"))

