rm(list=ls())

source(here::here("0-config.R"))
source(here::here("table-functions.R"))

# load enrollment characteristics and results
#d <- read.csv(paste0(dropboxDir,"Data/Cleaned/Audrie/bangladesh-dm-ee-ipv-cesd-pss-covariates-immunelab.csv"))
H1 <- readRDS(here('results/unadjusted/H1_res.RDS'))
H2 <- readRDS(here('results/unadjusted/H2_res.RDS'))
H3 <- readRDS(here('results/unadjusted/H3_res.RDS'))
H1adj <- readRDS(here('results/H1_adj_res.RDS'))
H2adj <- readRDS(here('results/H2_adj_res.RDS'))
H3adj <- readRDS(here('results/H3_adj_res.RDS'))

full_res <- rbind(H1, H2, H3)
full_adj_res <- rbind(H1adj, H2adj, H3adj)

#### MAIN TABLES ####
#### Table 1 ####
# Characteristics of participants
# nperc <- function(vector){
#   n <- sum(vector==1, na.rm=T)
#   perc <- round(n/sum(!is.na(vector))*100)
#   paste(n, " (", perc, "%)", sep="")
# }
# 
# mediqr <- function(vector){
#   quantiles <- round(quantile(vector, na.rm=T), 2)
#   paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
# }
# 
# n_med_col <- c(nperc(d$sex), mediqr(d$t2_f2_8ip), mediqr(d$t2_f2_23d), mediqr(d$t2_f2_VI), mediqr(d$t2_f2_12i),
#                mediqr(d$t3_cort_slope), mediqr(d$t3_residual_cort), mediqr(d$t3_saa_slope), mediqr(d$t3_residual_saa),
#                mediqr(d$t3_map), mediqr(d$t3_hr_mean), mediqr(d$t3_gcr_mean), mediqr(d$t3_gcr_cpg12),
#                mediqr(d$laz_t2), mediqr(d$waz_t2), mediqr(d$whz_t2), mediqr(d$hcz_t2),
#                mediqr(d$laz_t3), mediqr(d$waz_t3), mediqr(d$whz_t3), mediqr(d$hcz_t3),
#                nperc(d$diar7d_t2), nperc(d$diar7d_t3), mediqr(d$momage), mediqr(d$momheight), 
#                mediqr(d$momeduy), mediqr(d$cesd_sum_t2), mediqr(d$cesd_sum_ee_t3), mediqr(d$pss_sum_mom_t3), 
#                nperc(d$life_viol_any_t3))
# 
# 
# #immune markers, anthropometry (Y1, Y2)
# tbl1 <- data.table(" " = c("Child","","","","","","","","","","","","","","","","","","","","","","","Mother","","","","","",""),
#                    " " = c("", "Urinary F2-isoprostanes (Year 1)","","","", "Salivary cortisol reactivity (Year 2)","", "sAA reactivity (Year 2)","",
#                            "SAM biomarkers (Year 2)","", "Glucocorticoid receptor","", "Anthropometry (14 months, Year 1)","","","",
#                            "Anthropometry (28 months, Year 2)","","","", "Diarrhea (14 months, Year 1)", "Diarrhea (28 months, Year 2)","",
#                            "Anthropometry at enrollment", "Education", "Depression at Year 1", "Depression at Year 2", "Perceived stress at Year 2", 
#                            "Intimate partner violence"),
#                    " " = c("Female", "iPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a-VI", "8,12-iso-iPF(2a)-VI", 
#                            "Change in slope between pre- and post-stressor cortisol", "Cortisol residualized gain score", 
#                            "Change in slope between pre- and post-stressor sAA change", "sAA residualized gain score",
#                            "Mean arterial pressure", "Resting heart rate", "NR3C1 exon 1F promoter methylation", "NGFI-A transcription factor binding site methylation",
#                            "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
#                            "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
#                            "Caregiver-reported 7-day recall", "Caregiver-reported 7-day recall", "Age (years)", "Height (cm)", "Schooling completed (years)",
#                            "CES-D score", "CES-D score", "Perceived Stress Scale score", "Any lifetime exposure"),
#                    "n (%) or median (IQR)" = n_med_col)


#### Table 2 ####

exposure <- c("viol_any_preg","viol_any_t2","life_viol_any_t3")           
expo_var <- c("Exposure to IPV during pregnancy", "Exposure to IPV during first year of child's life",
              "Cumulative lifetime exposure to IPV")
outcome <- c("sumscore_t2_Z", "t2_ratio_th1_th2", "t2_ratio_th1_th17", "t2_ratio_th1_il10", "t2_ratio_th2_il10",
             "t2_ratio_th17_il10", "t2_ratio_gmc_il10", "t2_ratio_il2_il10", "t2_ln_ifn", "t2_ln_crp", "t2_ln_agp", "t2_ln_igf",
             "sumscore_t3_Z", "t3_ratio_th1_th2", "t3_ratio_th1_th17", "t3_ratio_th1_il10", "t3_ratio_th2_il10",
             "t3_ratio_th17_il10", "t3_ratio_gmc_il10", "t3_ratio_il2_il10", "t3_ln_ifn", "t3_ln_crp", "t3_ln_agp", "t3_ln_igf")
out_var <- c("Sum score of inflammation Year 1", "Ln Th1/Th2 Year 1", "Ln Th1/Th17 Year 1", "Ln Th1/IL-10 Year 1", "Ln Th2/IL-10 Year 1", 
             "Ln Th17/IL-10 Year 1", "Ln GM-CSF/IL-10 Year 1", "Ln IL-2/IL-10 Year 1", "Ln IFN-y Year 1", "Ln CRP Year 1", "Ln AGP Year 1", "Ln IGF-1 Year 1",
             "Sum score of inflammation Year 2", "Ln Th1/Th2 Year 2", "Ln Th1/Th17 Year 2", "Ln Th1/IL-10 Year 2", "Ln Th2/IL-10 Year 2", 
             "Ln Th17/IL-10 Year 2", "Ln GM-CSF/IL-10 Year 2", "Ln IL-2/IL-10 Year 2", "Ln IFN-y Year 2", "Ln CRP Year 2", "Ln AGP Year 2", "Ln IGF-1 Year 2")

exp_name <- "Maternal Exposure to IPV"
tbl2 <- growth_tbl(exp_name, expo_var, out_var, exposure, outcome, H1, H1adj, T)
tbl2flex <- growth_tbl_flex(exp_name, expo_var, out_var, exposure, outcome, H1, H1adj, T, 1.3, 1.3)
tbl1supp <- growth_tbl(exp_name, expo_var, out_var, exposure, outcome, H1, H1adj)
tbl1flexsupp <- growth_tbl_flex(exp_name, expo_var, out_var, exposure, outcome, H1, H1adj)


#### Table 3 ####

exposure <- c("pss_sum_mom_t3","pss_sum_dad_t3")           
expo_var <- c("Maternal stress score", "Paternal stress score")
outcome <- c("sumscore_t3_Z", "t3_ratio_th1_th2", "t3_ratio_th1_th17", "t3_ratio_th1_il10", "t3_ratio_th2_il10",
             "t3_ratio_th17_il10", "t3_ratio_gmc_il10", "t3_ratio_il2_il10", "t3_ln_ifn", "t3_ln_crp", "t3_ln_agp", "t3_ln_igf")
out_var <- c("Sum score of inflammation Year 2", "Ln Th1/Th2 Year 2", "Ln Th1/Th17 Year 2", "Ln Th1/IL-10 Year 2", "Ln Th2/IL-10 Year 2", 
             "Ln Th17/IL-10 Year 2", "Ln GM-CSF/IL-10 Year 2", "Ln IL-2/IL-10 Year 2", "Ln IFN-y Year 2", "Ln CRP Year 2", "Ln AGP Year 2", "Ln IGF-1 Year 2")

exp_name <- "Parental Stress"
tbl3 <- growth_tbl(exp_name, expo_var, out_var, exposure, outcome, H2, H2adj, T)
tbl3flex <- growth_tbl_flex(exp_name, expo_var, out_var, exposure, outcome, H2, H2adj, T, 1, 1.3)
tbl2supp <- growth_tbl(exp_name, expo_var, out_var, exposure, outcome, H2, H2adj)
tbl2flexsupp <- growth_tbl_flex(exp_name, expo_var, out_var, exposure, outcome, H2, H2adj)


#### Table 4 ####

exposure <- c("cesd_sum_t2","cesd_sum_t2_binary")           
expo_var <- c("CES-D Score Year 1", "Binary CES-D Score Year 1")
outcome <- c("sumscore_t2_Z", "t2_ratio_th1_th2", "t2_ratio_th1_th17", "t2_ratio_th1_il10", "t2_ratio_th2_il10",
             "t2_ratio_th17_il10", "t2_ratio_gmc_il10", "t2_ratio_il2_il10", "t2_ln_ifn", "t2_ln_crp", "t2_ln_agp", "t2_ln_igf",
             "sumscore_t3_Z", "t3_ratio_th1_th2", "t3_ratio_th1_th17", "t3_ratio_th1_il10", "t3_ratio_th2_il10",
             "t3_ratio_th17_il10", "t3_ratio_gmc_il10", "t3_ratio_il2_il10", "t3_ln_ifn", "t3_ln_crp", "t3_ln_agp", "t3_ln_igf")
out_var <- c("Sum score of inflammation Year 1", "Ln Th1/Th2 Year 1", "Ln Th1/Th17 Year 1", "Ln Th1/IL-10 Year 1", "Ln Th2/IL-10 Year 1", 
             "Ln Th17/IL-10 Year 1", "Ln GM-CSF/IL-10 Year 1", "Ln IL-2/IL-10 Year 1", "Ln IFN-y Year 1", "Ln CRP Year 1", "Ln AGP Year 1", "Ln IGF-1 Year 1",
             "Sum score of inflammation Year 2", "Ln Th1/Th2 Year 2", "Ln Th1/Th17 Year 2", "Ln Th1/IL-10 Year 2", "Ln Th2/IL-10 Year 2", 
             "Ln Th17/IL-10 Year 2", "Ln GM-CSF/IL-10 Year 2", "Ln IL-2/IL-10 Year 2", "Ln IFN-y Year 2", "Ln CRP Year 2", "Ln AGP Year 2", "Ln IGF-1 Year 2")

exp_name <- "CES-D Year 1"
tbl4 <- growth_tbl(exp_name, expo_var, out_var, exposure, outcome, H3, H3adj, T)
tbl4flex <- growth_tbl_flex(exp_name, expo_var, out_var, exposure, outcome, H3, H3adj, T, 1.3, 1.3)
tbl3supp <- growth_tbl(exp_name, expo_var, out_var, exposure, outcome, H3, H3adj)
tbl3flexsupp <- growth_tbl_flex(exp_name, expo_var, out_var, exposure, outcome, H3, H3adj)

#### Table 5 ####

exposure <- c("cesd_sum_ee_t3","cesd_sum_ee_t3_binary")           
expo_var <- c("CES-D Score Year 2", "Binary CES-D Score Year 2")
outcome <- c("sumscore_t3_Z", "t3_ratio_th1_th2", "t3_ratio_th1_th17", "t3_ratio_th1_il10", "t3_ratio_th2_il10",
             "t3_ratio_th17_il10", "t3_ratio_gmc_il10", "t3_ratio_il2_il10", "t3_ln_ifn", "t3_ln_crp", "t3_ln_agp", "t3_ln_igf")
out_var <- c("Sum score of inflammation Year 2", "Ln Th1/Th2 Year 2", "Ln Th1/Th17 Year 2", "Ln Th1/IL-10 Year 2", "Ln Th2/IL-10 Year 2", 
             "Ln Th17/IL-10 Year 2", "Ln GM-CSF/IL-10 Year 2", "Ln IL-2/IL-10 Year 2", "Ln IFN-y Year 2", "Ln CRP Year 2", "Ln AGP Year 2", "Ln IGF-1 Year 2")

exp_name <- "CES-D Year 2"
tbl5 <- growth_tbl(exp_name, expo_var, out_var, exposure, outcome, H3, H3adj, T)
tbl5flex <- growth_tbl_flex(exp_name, expo_var, out_var, exposure, outcome, H3, H3adj, T, 1.3, 1.3)
tbl4supp <- growth_tbl(exp_name, expo_var, out_var, exposure, outcome, H3, H3adj)
tbl4flexsupp <- growth_tbl_flex(exp_name, expo_var, out_var, exposure, outcome, H3, H3adj)


#### SAVE TABLES ####

write.csv(tbl2, here('tables/main/ipv-immune-table1.csv'))
write.csv(tbl3, here('tables/main/ipv-immune-table2.csv'))
write.csv(tbl4, here('tables/main/ipv-immune-table3.csv'))
write.csv(tbl5, here('tables/main/ipv-immune-table4.csv'))

write.csv(tbl1supp, here('tables/supplementary/ipv-immune-supptable1.csv'))
write.csv(tbl2supp, here('tables/supplementary/ipv-immune-supptable2.csv'))
write.csv(tbl3supp, here('tables/supplementary/ipv-immune-supptable3.csv'))
write.csv(tbl4supp, here('tables/supplementary/ipv-immune-supptable4.csv'))

save_as_docx("Table 1: Association between Exposure to IPV and Child Immune Status" = tbl2flex, 
             "Table 2: Association between Parental Stress and Child Immune Status" = tbl3flex, 
             "Table 3: Association between CES-D Scores at Year 1 and Child Immune Status" = tbl4flex, 
             "Table 4: Association between CES-D Scores at Year 2 and Child Immune Status" = tbl5flex, 
             path='C:/Users/Sophia/Documents/WASH/WASH IPV and Immune/ipv-immune main.docx', 
             pr_section = sect_properties)

save_as_docx("Table S1: Association between Exposure to IPV and Child Immune Status" = tbl1flexsupp, 
             "Table S2: Association between Parental Stress and Child Immune Status" = tbl2flexsupp, 
             "Table S3: Association between CES-D Scores at Year 1 and Child Immune Status" = tbl3flexsupp, 
             "Table S4: Association between CES-D Scores at Year 2 and Child Immune Status" = tbl4flexsupp, 
             path=here('C:/Users/Sophia/Documents/WASH/WASH IPV and Immune/ipv-immune supplementary.docx'))

