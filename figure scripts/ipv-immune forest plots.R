
rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(patchwork)
library(RColorBrewer)



H1_res <- readRDS(here("results/H1_adj_res.RDS"))
H2_res <- readRDS(here("results/H2_adj_res.RDS"))
H3_res <- readRDS(here("results/H3_adj_res.RDS"))

head(H1_res)

unique(H1_res$X)
unique(H2_res$X)
unique(H3_res$X)
unique(H1_res$Y)

H1_res <- H1_res %>% mutate(X=factor(X, levels=c("viol_any_t2", "life_viol_any_t3", "viol_any_preg")),
                            sig=factor(1*(Pval<0.05)))
H2_res <- H2_res %>% mutate(X=factor(X, levels=c("pss_sum_mom_t3", "pss_sum_dad_t3")),
                            sig=factor(1*(Pval<0.05)))
H3_res <- H3_res %>% mutate(X=factor(X, levels=c( "cesd_sum_t2", "cesd_sum_t2_binary" , "cesd_sum_ee_t3", "cesd_sum_ee_t3_binary")),
                            sig=factor(1*(Pval<0.05)))


#forest plot
ggplot(H1_res, aes(x = Y, y = point.diff, ymin = lb.diff, ymax = ub.diff,
                   color=sig, shape=sig)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~X, scales = "free") +
  coord_flip() +
  scale_color_manual(values=c("black", "blue")) +
  scale_shape_manual(values=c(19, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Mean difference in immune marker Z-score") +
  ggtitle("H1: IPV in pregnancy and infant immune markers") +
  theme(strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

ggplot(H2_res, aes(x = Y, y = point.diff, ymin = lb.diff, ymax = ub.diff,
                   color=sig, shape=sig)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~X, scales = "free") +
  coord_flip() +
  scale_color_manual(values=c("black", "blue")) +
  scale_shape_manual(values=c(19, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Mean difference in immune marker Z-score") +
  ggtitle("H2: Maternal stress and infant immune markers") +
  theme(strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

ggplot(H3_res, aes(x = Y, y = point.diff, ymin = lb.diff, ymax = ub.diff,
                   color=sig, shape=sig)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~X, scales = "free") +
  coord_flip() +
  scale_color_manual(values=c("black", "blue")) +
  scale_shape_manual(values=c(19, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Mean difference in immune marker Z-score") +
  ggtitle("H2: Maternal depression and infant immune markers") +
  theme(strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
