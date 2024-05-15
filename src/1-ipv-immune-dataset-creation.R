

rm(list=ls())

source(here::here("0-config.R"))
#devtools::install_github("washb-eed-substudies/washbgam")
#source(here::here("src/0-gam-functions.R"))



d <- read.csv(paste0(dropboxDir,"Data/Cleaned/Audrie/bangladesh-dm-ee-ipv-cesd-pss-covariates-immunelab.csv"))
colnames(d)

#IPV raw survey for frequencies
ipv_sv <-  haven::read_dta("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/MHLE_Mother_clean_data_30May16_deidentified.dta") 
head(ipv_sv)
labels <- sapply(ipv_sv, function(x) attr(x, "label"))
labels <- tibble(name = names(labels),
               label = labels)
labels$label<- as.character(labels$label)
labels[grepl("ow often",labels$label),]

# NEVER .................................................... 1
# ONCE .......................................................... 2
# SEVERAL TIMES ................................... 3
# MANY TIMES/MOST OF THE TIME.. 4
# DON'T KNOW ................. 8
# REFUSED/NO ANSWER ..... 9

labels[labels$name=="q_1002",]
table(ipv_sv$q_1002)

labels[labels$name=="q_808",]
table(ipv_sv$q_808)

labels[labels$name=="q_902_a",]
table(ipv_sv$q_902_a)

#Has your current/ most recent husband/partner, ever..
labels[labels$name=="q_805_a",]
table(ipv_sv$q_805_a)

labels[labels$name=="q_805_b",]
table(ipv_sv$q_805_b)

labels[labels$name=="q_805_c",]
table(ipv_sv$q_805_c)

labels[labels$name=="q_805_d",]
table(ipv_sv$q_805_d)
#Note! Not enough variation for these variables to be used


#clean and merge into the main dataset
ipv_sv <- ipv_sv %>% select(dataid, q_1002, q_808, q_902_a) %>%
  rename(ipv_child = q_1002, ipv_afraid = q_808, ipv_life_freq = q_902_a) %>%
  mutate(ipv_child=case_when(ipv_child == 1 ~ "Never", ipv_child == 2 ~ "Once", ipv_child == 3 ~ "Several times", ipv_child == 4 ~"Most/all of the times" , TRUE ~ "Missing"),
         ipv_afraid=case_when(ipv_afraid == 1 ~ "Never", ipv_afraid == 2 ~ "Once", ipv_afraid == 3 ~ "Several times", ipv_afraid == 4 ~"Most/all of the times", TRUE ~ "Missing"),
         ipv_life_freq=case_when(ipv_life_freq == 1 ~ "Never", ipv_life_freq == 2 ~ "Once", ipv_life_freq == 3 ~ "Several times", ipv_life_freq == 4 ~ "Most/all of the times", TRUE ~ "Missing"),
         dataid=as.numeric(dataid))

head(ipv_sv)


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

#merge in frequency variables


dim(d)
dfull <- left_join(d, Yvars, by="childid")
dim(dfull)
table(ipv_sv$ipv_life_freq)
dfull <- left_join(dfull, ipv_sv, by="dataid")
dim(dfull)
table(dfull$ipv_life_freq)

#Check merge
summary(Yvars$sumscore_t2_Z)
summary(dfull$sumscore_t2_Z)

# add variables to turn cesd into binary variables
# classify top 25% of mothers in sample as experiencing high depressive symptoms
cesd_t2_q<-quantile(dfull$cesd_sum_t2, na.rm=T)[4]
cesd_t3_q<-quantile(dfull$cesd_sum_ee_t3, na.rm=T)[4]
dfull$cesd_sum_t2_binary<-ifelse(dfull$cesd_sum_t2 >= cesd_t2_q, 1, 0)
dfull$cesd_sum_ee_t3_binary<-ifelse(dfull$cesd_sum_ee_t3 >= cesd_t3_q, 1, 0)


#Save new dataset
saveRDS(dfull, paste0(dropboxDir,"Data/Cleaned/Audrie/ipv-immune-analysis-dataset.RDS"))
