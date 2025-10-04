
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
unique(H2_res$Y)
unique(H3_res$Y)

H1_res <- H1_res %>% mutate(X=factor(X, levels=c("viol_any_t2", "life_viol_any_t3", "viol_any_preg")),
                            sig=factor(1*(Pval<0.05)))
H2_res <- H2_res %>% mutate(X=factor(X, levels=c("pss_sum_mom_t3", "pss_sum_dad_t3")),
                            sig=factor(1*(Pval<0.05)))
H3_res <- H3_res %>% mutate(X=factor(X, levels=c( "cesd_sum_t2", "cesd_sum_t2_binary" , "cesd_sum_ee_t3", "cesd_sum_ee_t3_binary")),
                            sig=factor(1*(Pval<0.05)))


#forest plot

# Create label mapping for Y variables (immune markers)
y_labels <- c(
  "t2_ratio_th1_th2" = "Th1/Th2",
  "t3_ratio_th1_th2" = "Th1/Th2",
  "t2_ratio_th1_th17" = "Th1/Th17",
  "t3_ratio_th1_th17" = "Th1/Th17",
  "t2_ratio_th1_il10" = "Th1/Interleukin-10",
  "t3_ratio_th1_il10" = "Th1/Interleukin-10",
  "t2_ratio_th2_il10" = "Th2/Interleukin-10",
  "t3_ratio_th2_il10" = "Th2/Interleukin-10",
  "t2_ratio_th17_il10" = "Th17/Interleukin-10",
  "t3_ratio_th17_il10" = "Th17/Interleukin-10",
  "t2_ratio_gmc_il10" = "GMC/Interleukin-10",
  "t3_ratio_gmc_il10" = "GMC/Interleukin-10",
  "t2_ratio_il2_il10" = "Interleukin-2/Interleukin-10",
  "t3_ratio_il2_il10" = "Interleukin-2/Interleukin-10",
  "t2_ln_igf" = "IGF-1",
  "t3_ln_igf" = "IGF-1",
  "t2_ln_crp" = "C-reactive protein",
  "t3_ln_crp" = "C-reactive protein",
  "t2_ln_agp" = "??-1-acid glycoprotein",
  "t3_ln_agp" = "??-1-acid glycoprotein",
  "t2_ln_ifn" = "Interferon-??",
  "t3_ln_ifn" = "Interferon-??",
  "sumscore_t2_Z" = "Composite immune score",
  "sumscore_t3_Z" = "Composite immune score"
)

# Create label mapping for X variables (exposures)
x_labels_h1 <- c(
  "viol_any_t2" = "IPV at 14 weeks",
  "life_viol_any_t3" = "IPV at 28 weeks",
  "viol_any_preg" = "IPV during pregnancy"
)

x_labels_h2 <- c(
  "pss_sum_mom_t3" = "Maternal stress",
  "pss_sum_dad_t3" = "Paternal stress"
)

x_labels_h3 <- c(
  "cesd_sum_t2" = "Maternal depression (continuous)",
  "cesd_sum_t2_binary" = "Maternal depression (binary)",
  "cesd_sum_ee_t3" = "Maternal depression at 28w (continuous)",
  "cesd_sum_ee_t3_binary" = "Maternal depression at 28w (binary)"
)

# H1 plot with formatted labels
p1 = ggplot(H1_res, aes(x = Y, y = point.diff, ymin = lb.diff, ymax = ub.diff,
                   color=sig, shape=sig)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~X, scales = "free", labeller = labeller(X = x_labels_h1)) +
  coord_flip() +
  scale_x_discrete(labels = y_labels) +
  scale_color_manual(values=c("black", "blue")) +
  scale_shape_manual(values=c(19, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Mean difference in immune marker Z-score") +
  ggtitle("H1: IPV in pregnancy and infant immune markers") +
  theme(strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# H2 plot with formatted labels
p2 = ggplot(H2_res, aes(x = Y, y = point.diff, ymin = lb.diff, ymax = ub.diff,
                   color=sig, shape=sig)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~X, scales = "free", labeller = labeller(X = x_labels_h2)) +
  coord_flip() +
  scale_x_discrete(labels = y_labels) +
  scale_color_manual(values=c("black", "blue")) +
  scale_shape_manual(values=c(19, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Mean difference in immune marker Z-score") +
  ggtitle("H2: Maternal stress and infant immune markers") +
  theme(strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# H3 plot with formatted labels
p3 = ggplot(H3_res, aes(x = Y, y = point.diff, ymin = lb.diff, ymax = ub.diff,
                   color=sig, shape=sig)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~X, scales = "free", labeller = labeller(X = x_labels_h3)) +
  coord_flip() +
  scale_x_discrete(labels = y_labels) +
  scale_color_manual(values=c("black", "blue")) +
  scale_shape_manual(values=c(19, 1)) +
  theme_bw() +
  xlab("") +
  ylab("Mean difference in immune marker Z-score") +
  ggtitle("H3: Maternal depression and infant immune markers") +
  theme(strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

#save the plots 
ggsave(p1, file=here("figures/figure-H1-ipv-immune-forest.png"), width=8, height=6)
ggsave(p2, file=here("figures/figure-H2-stress-immune-forest.png"), width=8, height=4)
ggsave(p3, file=here("figures/figure-H3-depression-immune-forest.png"), width=8, height=6)


