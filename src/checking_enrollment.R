rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir,"Data/Cleaned/Audrie/ipv-immune-analysis-dataset.RDS"))

filtering <- function(row){
  any(!is.na(row))
}
y2exp<-d[apply(select(d, c("viol_any_preg","viol_any_t2","life_viol_any_t3",
                           "pss_sum_mom_t3", "pss_sum_dad_t3","cesd_sum_ee_t3")), 1, filtering),]
y1exp<-d[apply(select(d, c("cesd_sum_t2")), 1, filtering),]

y1out<-d[apply(select(d, grep("t2_ln", colnames(d))), 1, filtering),]
y2out<-d[apply(select(d, grep("t3_ln", colnames(d))), 1, filtering),]


nrow(y1exp)
nrow(y2exp)
nrow(y1out)
nrow(y2out)

y1exp[apply(select(y1exp, grep("t2_ln", colnames(d))), 1, filtering),] %>% nrow()
y2exp[apply(select(y2exp, grep("t3_ln", colnames(d))), 1, filtering),] %>% nrow()
  