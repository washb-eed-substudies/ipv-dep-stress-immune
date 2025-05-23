rm(list=ls())

source(here::here("0-config.R"))
library(tidyverse)
library(flextable)
library(officer)

d <- readRDS("/Users/kjung0909/Documents/Research/WASHB/bangladesh-cleaned-master-data.RDS") %>% filter(.$ipv_immune == 1)
#d <- box_read("871638120165") %>% filter(.$pregnancy_immune == 1)

filtering <- function(row){
  any(!is.na(row))
}
d %>% group_by(dataid) %>% summarise(n=n()) %>% filter(n>1)
filter(d, dataid %in% c(23404, 31102, 35105)) %>% select(dataid, childid)
#unique moms
m <- rbind(filter(d, !(dataid %in% c(23404, 31102, 35105))), filter(d, dataid %in% c(23404, 31102, 35105))[1:3,])

#MATERNAL PREGNANCY BIOMARKERS UNCOMMENT AND FILL IN THIS CODE (UNCOMMENT WITH CTRL+SHIFT+C ON PC)
#exp <- c("vitD_nmol_per_L", "logFERR_inf", "logSTFR_inf", "logRBP_inf", "vit_D_def", "vit_A_def", "iron_def", "ln_preg_cort", "logCRP", "logAGP", "mom_t0_ln_ifn", "sumscore_t0_mom_Z", "ln_preg_estri") 
#out <- c("t2_ln_crp", "t2_ln_agp", "t2_ln_ifn", "sumscore_t2_Z", 
#         "t3_ln_crp", "t3_ln_agp", "t3_ln_ifn","sumscore_t3_Z") 
#m <- m[apply(select(d, all_of(exp)), 1, filtering),] # only has rows where we have exposure data for the mom
#d <- d[apply(select(d, all_of(out)), 1, filtering),] # only has rows where we have both some exposure data and some outcome data (all kids included in analyses)

#OTHER ANALYSES WITH T2 AND T3 EXPOSURES AND OUTCOMES UNCOMMENT AND FILL IN THE CODE BELOW (UNCOMMENT WITH CTRL+SHIFT+C ON PC)
exp_t2 <- c("life_viol_any_t3", "cesd_sum_t2", "cesd_sum_t2_binary") 
out_t2 <- c("t2_ln_crp", "t2_ln_agp", "t2_ln_ifn", "sumscore_t2_Z","t2_ln_igf", "t2_ratio_gmc_il10", "t2_ratio_il2_il10", "t2_ratio_th1_il10","t2_ratio_th2_il10","t2_ratio_th17_il10", "t2_ratio_th1_th2", "t2_ratio_th1_th17") 
exp_t3 <- c("pss_sum_mom_t3", "pss_sum_dad_t3", "cesd_sum_ee_t3", "cesd_sum_ee_t3_binary") 
out_t3 <- c("t3_ln_crp", "t3_ln_agp", "t3_ln_ifn","sumscore_t3_Z","t3_ln_igf", "t3_ratio_gmc_il10", "t3_ratio_il2_il10", "t3_ratio_th1_il10", "t3_ratio_th2_il10", "t3_ratio_th17_il10", "t3_ratio_th1_th2", "t3_ratio_th1_th17") 
t2 <- d[apply(select(d, all_of(exp_t2)), 1, filtering),] # only has rows where we have exposure data at t2
t2 <- t2[apply(select(t2, all_of(c(out_t2, out_t3))), 1, filtering),] # only has rows where we have both some exposure data and some outcome data
t3 <- d[apply(select(d, all_of(exp_t3)), 1, filtering),] # only has rows where we have exposure data at t3
t3 <- t3[apply(select(t3, all_of(out_t3)), 1, filtering),] # only has rows where we have both some exposure data and some outcome data
d <- t2 %>% full_join(t3, by=names(d)) # has all children included in this analysis

# YOU SHOULDN'T HAVE TO CHANGE ANYTHING BELOW THIS EXCEPT THE PATH TO SAVE THE TABLE 
# UNLESS YOU WANT TO ADD ADDITIONAL CHARACTERISTICS TO THE ENROLLMENT TABLE
# TO ADD ADDITIONAL CHILD CHARACTERISTICS, PASS IN child_char = c(vector with variable names in the table) 
# and child_char_names = c(vector with names for the characteristics you want to appear in the table) 
# AS ADDITIONAL ARGUMENTS IN LINE 94 AND/OR mom_char and mom_char_names FOR ADDITIONAL MATERNAL CHARACTERISTICS
characteristics <- function(d, child_char = NULL, child_char_names = NULL, mom_char = NULL, mom_char_names = NULL) 
  #  { 
  nperc <- function(vector){
    n <- sum(vector==1, na.rm=T)
    perc <- round(n/sum(!is.na(vector))*100)
    paste(n, " (", perc, "%)", sep="")
  }

mediqr <- function(vector){
  quantiles <- round(quantile(vector, na.rm=T), 2)
  paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
}

child <- c('sex', 'gest_age_weeks','laz_t1','waz_t1','whz_t1','hcz_t1',
           'laz_t2','waz_t2','whz_t2','hcz_t2',
           'laz_t3','waz_t3','whz_t3','hcz_t3','diar7d_t2','diar7d_t3')

mom <- c('momage', 'momheight', 'momeduy', 'cesd_sum_t2', 'cesd_sum_ee_t3', 'pss_sum_mom_t3', 'life_viol_any_t3')

hfiacat_ind <- ifelse(m$hfiacat=="Food Secure", 0, 1)
household <- c('hfiacat_ind')
sum_hfiacat_ind <- sum(na.omit(hfiacat_ind))

n_med_col <- NULL
for (var in c(child, mom)) {
  if (var %in% c('sex', 'diar7d_t2', 'diar7d_t3', 'life_viol_any_t3', 'hfiacat_ind') | is.factor(d[[var]])) {
    if (var == 'sex') {
      n <- sum(d$sex=='female', na.rm=T)
      perc <- round(n/sum(!is.na(d$sex))*100)
      n_med_col <- c(n_med_col, paste(n, " (", perc, "%)", sep=""))
    }else {
      d[[var]] <- na_if(d[[var]], "Missing")
      n_med_col <- c(n_med_col, nperc(d[[var]]))
    }
  }else {
    n_med_col <- c(n_med_col, mediqr(d[[var]]))
  }
}

for (var in c(mom)) {
  if (var %in% c('life_viol_any_t3') | is.factor(m[[var]])) {
    m[[var]] <- na_if(m[[var]], "Missing")
    n_med_col <- c(n_med_col, nperc(m[[var]]))
  }else {
    n_med_col <- c(n_med_col, mediqr(m[[var]]))
  }
}

tbl1 <- data.table("C1" = c("Child", rep("", length(child)-1),
                            "Mother", rep("",length(mom)-1), 
                            "Household", rep("",length(household)-1)),
                   "C2" = c(rep("", length('child_char')), "",
                            "Anthropometry (3 months)","","","",
                            "Anthropometry (14 months)","","","",
                            "Anthropometry (28 months)","","","", 
                            "Diarrhea (14 months)", "Diarrhea (28 months)","",
                            "Anthropometry at enrollment", "Education", 
                            "Depressive Symptoms (14 months)", "Depressive Symptoms (28 months)",
                            "Perceived stress (28 months)", 
                            "Intimate partner violence",
                            "Household Food Insecurity"),
                   "C3" = c("Female",
                            "Gestational Age (weeks)",
                            "Length-for-age Z score", 
                            "Weight-for-age Z score", "Weight-for-length Z score", 
                            "Head circumference-for-age Z score",
                            "Length-for-age Z score", 
                            "Weight-for-age Z score", "Weight-for-length Z score", 
                            "Head circumference-for-age Z score",
                            "Length-for-age Z score", 
                            "Weight-for-age Z score", "Weight-for-length Z score", 
                            "Head circumference-for-age Z score",
                            "Caregiver-reported 7-day recall", "Caregiver-reported 7-day recall", "Age (years)", "Height (cm)", "Schooling completed (years)", "CESD-20* score", "CESD-20* score", "Perceived Stress Scale score", "Any lifetime exposure", "Food-insecure households"),
                   "C4" = n_med_col)

tbl1flex <- flextable(tbl1, col_keys=names(tbl1))
tbl1flex <- set_header_labels(tbl1flex,
                              values = list("C1" = "", "C2" = "", "C3" = "", "C4" = "n (%) or median (IQR)"))
tbl1flex <- hline_top(tbl1flex, part="header", border=fp_border(color="black", width = 1))
tbl1flex <- hline_bottom(tbl1flex, part="all", border=fp_border(color="black", width = 1))
tbl1flex <- autofit(tbl1flex, part = "all")
tbl1flex <- align(tbl1flex, j = c(1, 2, 3), align = "left", part="all")
tbl1flex <- align(tbl1flex, j = 4, align = "center", part="all")
tbl1flex <- fit_to_width(tbl1flex, max_width=8)
tbl1flex
#}


#sum(m$hfiacat_ind)

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait", width=8.5, height=11),
  page_margins = page_mar(bottom=.3, top=.3, right=.3, left=.3, gutter = 0)
)
save_as_docx("Table 1" = tbl1flex, path="/Users/kjung0909/Documents/Research/WASHB/ipv-childimmune/ipv-childimmune/tables/main/ipv-childimmune-enrollment.docx", 
             pr_section = sect_properties) 

#table(d$momedu)