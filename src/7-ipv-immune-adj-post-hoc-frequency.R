rm(list=ls())

source(here::here("0-config.R"))
library(washb)
library(washbgam)
#devtools::install_github("washb-eed-substudies/washbgam")
#source(here::here("src/0-gam-functions.R"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Audrie/ipv-immune-analysis-dataset.RDS"))
colnames(d)

table(d$ipv_child)
table(d$ipv_afraid)
table(d$ipv_life_freq)


#merge in HH wealth
wealth <- read.csv(paste0(dropboxDir,"Data/Cleaned/Audrie/hhwealth.csv"))
colnames(wealth)

d <- left_join(d, wealth, by = c("dataid"))


#---------------------------------------------------------------
# set covariates
#---------------------------------------------------------------

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



#### Loop over exposure-outcome pairs ####


Xvars_t2 <- c("ipv_child","ipv_afraid","ipv_life_freq" )            

# Outcomes: 
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

#temp
Yvars <- c( "sumscore_t2_Z", "sumscore_t3_Z")


#Check that Yvars are in the data
Yvars[!(Yvars %in% colnames(d))]


i=Xvars_t2[3]
j=Yvars[1]

#Fit models
freq_res <- NULL
for(i in Xvars_t2){
  for(j in Yvars){
    cat(i,"\n")
    cat(j,"\n")
    if(grepl("_t2",j)|grepl("t2_",j)){
      Ws = Wvars_t2_outcomes[[i]]
    }else{
      Ws = Wvars_t3_outcomes[[i]]
    }
    df <- d %>% select(all_of(c("childid",i,j,Ws)))
    res=NULL
    for(k in c("Once","Several times","Most/all of the times")){
      temp<-NULL
      if(sum(df[[i]]==k,na.rm = T)>0){
      temp <- washb_glm(Y=df[[j]], tr=df[[i]], 
                           W = Ws, forcedW = NULL, V = NULL, 
                           id=df$childid, contrast=c("Never",k), family = "gaussian", print = F)
      }
      if(!is.null(temp)){
        temp <- temp$TR
        temp$exposure <- j
        temp$outcome <- i
        temp$contrast <- k
      }
      res=bind_rows(res, temp)
    }

    freq_res <- bind_rows(freq_res, res)
  }
  rownames(freq_res) <- NULL
}


#format for plot data
freq_res <- freq_res %>% mutate(contrast=factor(contrast, levels=c("Never","Once","Several times","Most/all of the times")))

#make plot
p <- ggplot(aes(x=contrast, y=`Coef.`, ymin=`2.5%`, ymax=`97.5%`, color= outcome), data=freq_res) +
  geom_point() +
  geom_errorbar(width=0.1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~exposure + outcome, scales="free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x="Contrast (ref: none)", y="Adjusted mean difference") 

p
  
# #Save models
# saveRDS(freq_models, here("models/freq_adj_models.RDS"))
# 
# #Save results
# saveRDS(freq_res, here("results/post-hoc/freq_adj_res.RDS"))
# write.csv(freq_res, here("results/post-hoc/freq_adj_res.csv"))
# 
# #Save plot data
# saveRDS(freq_plot_data, here("figure-data/freq_adj_spline_data.RDS"))

