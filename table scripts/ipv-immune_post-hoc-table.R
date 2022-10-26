rm(list=ls())

library('flextable')
library('officer')
library('here')
source(here("table-functions.R"))
source(here::here("0-config.R"))

# load enrollment characteristics and results
# d <- readRDS("/Users/gabby/Documents/WASH Benefits/bangladesh-cleaned-master-data.RDS")
#Andrew file path
try(H1ph <- readRDS(paste0(here(),"/results/post-hoc/posthoc_adj_res.RDS")))
H1ph <- readRDS(here("~/WASH Benefits/ipv-dep-stress-immune/results/post-hoc/posthoc_adj_res.RDS"))

#### MAIN TABLES ####
#### Table 1 ####
# Post Hoc Analysis

exposure <- c("viol_any_preg", "cesd_sum_t2", "cesd_sum_t2_binary")
outcome <- c("th1_z_t2", "th2_z_t2", "th17_z_t2", "t2_ln_il12", "t2_ln_ifn", "t2_ln_il4", "t2_ln_il5", "t2_ln_il13", "t2_ln_il21", "t2_ln_il17",  "th1_z_t3", "th2_z_t3", "th17_z_t3", "t3_ln_il12", "t3_ln_ifn", "t3_ln_il4", "t3_ln_il5", "t3_ln_il13", "t3_ln_il21", "t3_ln_il17")
expo_var <- c("Exposure to IPV during pregnancy", "CES-D Score Year 1" )
out_var <- c("Th1 Year 1", "Th2 Year 2", "Th17 Year 1", "Ln IL-12 Year 1", "Ln IFN-y Year 1", "Ln IL-4 Year 1", "Ln IL-5 Year 1", "Ln IL-13 Year 1", "Ln IL-21 Year 1", "Ln IL-17 Year 1", "Th1 Year 2", "Th2 Year 2", "Th17 Year 2", "Ln IL-12 Year 2", "Ln IFN-y Year 2", "Ln IL-4 Year 2", "Ln IL-5 Year 2", "Ln IL-13 Year 2", "Ln IL-21 Year 2", "Ln IL-17 Year 2")
results_adj <- H1ph

tbl1 <- growth_tbl(name="Post Hoc Analysis", expo_var=expo_var, out_var=out_var, exposure=exposure, outcome=outcome, results_adj=H1ph, adj_only=T)
tbl1flex <- growth_tbl_flex(name="Post Hoc Analysis", expo_var=expo_var, out_var=out_var, exposure=exposure, outcome=outcome, results_adj=H1ph, adj_only=T)

#### SAVE TABLES ####

write.csv(tbl1, here('tables/post hoc/ipv-dep-immune_post-hoc-table1.csv'))

save_as_docx("Table 1" = tbl1flex, path='~/WASH Benefits/ipv-dep-stress-immune/tables/post hoc/ipv-dep-immune_post-hoc-table1.docx')
#Andrew file path
#try(save_as_docx("Table 1" = tbl1flex, path=paste0(here(),"/tables/post-hoc/ipv-dep-immune_post-hoc-table1.docx")))