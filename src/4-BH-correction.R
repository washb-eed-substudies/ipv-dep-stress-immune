#### Adjust all pvalues with BH procedure ####
rm(list=ls())

source(here::here("0-config.R"))

# load all results
H1_res <- readRDS(here('results/unadjusted/H1_res.RDS'))
H2_res <- readRDS(here('results/unadjusted/H2_res.RDS'))
H3_res <- readRDS(here('results/unadjusted/H3_res.RDS'))

H1_adj_res <- readRDS(here('results/H1_adj_res.RDS'))
H2_adj_res <- readRDS(here('results/H2_adj_res.RDS'))
H3_adj_res <- readRDS(here('results/H3_adj_res.RDS'))

full_res <- bind_rows(data.frame(H1_res, H=1), 
                      data.frame(H2_res, H=2), 
                      data.frame(H3_res, H=3))
full_adj_res <- bind_rows(data.frame(H1_adj_res, H=1), 
                      data.frame(H2_adj_res, H=2), 
                      data.frame(H3_adj_res, H=3))

full_res <- full_res %>% mutate(
  time = case_when(
    grepl("t2", Y) & !grepl("any_preg", X) ~ "t2",
    (grepl("t3", Y) & grepl("t2", X))|grepl("any_preg", X) ~ "t3S",
    grepl("t3", Y) & grepl("t3", X) ~ "t3C"
  )
)
table(full_res$time)

full_adj_res <- full_adj_res %>% mutate(
  time = case_when(
    grepl("t2", Y)  & !grepl("any_preg", X) ~ "t2",
    (grepl("t3", Y) & grepl("t2", X))|grepl("any_preg", X) ~ "t3S",
    grepl("t3", Y) & grepl("t3", X) ~ "t3C"
  )
)
table(full_adj_res$time)
table(is.na(full_adj_res$time))


full_res <- full_res %>% group_by(H, time) %>% 
  mutate(corrected.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

full_adj_res <- full_adj_res %>% group_by(H, time) %>% 
  mutate(corrected.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

saveRDS(full_res %>% filter(H==1) %>% select(-H, -time), here("results/unadjusted/H1_res.RDS"))
saveRDS(full_res %>% filter(H==2) %>% select(-H, -time), here("results/unadjusted/H2_res.RDS"))
saveRDS(full_res %>% filter(H==3) %>% select(-H, -time), here("results/unadjusted/H3_res.RDS"))

saveRDS(full_adj_res %>% filter(H==1) %>% select(-H), here("results/H1_adj_res.RDS"))
saveRDS(full_adj_res %>% filter(H==2) %>% select(-H), here("results/H2_adj_res.RDS"))
saveRDS(full_adj_res %>% filter(H==3) %>% select(-H), here("results/H3_adj_res.RDS"))