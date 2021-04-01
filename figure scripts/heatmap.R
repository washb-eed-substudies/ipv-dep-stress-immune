
rm(list=ls())
source(here::here("0-config.R"))
library(cowplot)
library(patchwork)
library(RColorBrewer)



H3_res <- readRDS(here("results/H3_adj_res.RDS"))

plotdf <- H3_res

plotdf <- plotdf %>% mutate(
  pos = ifelse(point.diff>0, 1, 0),
  pval_cat_dir = case_when(
    pos==1 & Pval < 0.05 ~ "Pos, sig",
    pos==1 & Pval < 0.10 & Pval >= 0.05 ~ "Pos, near sig",
    pos==1 & Pval > 0.10 ~ "Pos, insig",
    pos==0 & Pval < 0.05 ~ "Neg, sig",
    pos==0 & Pval < 0.10 & Pval >= 0.05 ~ "Neg, near sig",
    pos==0 & Pval > 0.10 ~ "Neg, insig"
    ),
  est= paste0(
    round(point.diff, 2), " (",
    round(lb.diff, 2), ", ",
    round(ub.diff, 2), ")"
  )
)

textcol = "grey20"
cols = rev(brewer.pal(n = 6, name = "BrBG"))


hm <- ggplot(plotdf, aes(x=X, y=Y, fill=pval_cat_dir)) +
  geom_tile(colour="grey80",size=0.25) +
  #scale_x_discrete(limits = rev(levels(plotdf$X)))+
  scale_y_discrete(expand=c(0,0))+
  theme_minimal(base_size=10) +
  scale_fill_manual(labels = levels(plotdf$pval_cat_dir),
                    values = c(cols, "gray80")) +
  geom_text(aes(label=est)) +
  theme(
    aspect.ratio = 1,
    legend.title=element_text(color=textcol,size=8),
    legend.margin = margin(grid::unit(0.1,"cm")),
    legend.text=element_text(colour=textcol,size=7,face="bold"),
    legend.key.height=grid::unit(0.2,"cm"),
    legend.key.width=grid::unit(1,"cm"),
    legend.position = "right",
    axis.text.x=element_text(size=8,colour=textcol,angle=45,hjust=1),
    axis.text.y=element_text(size=8,vjust = 0.2,colour=textcol),
    axis.ticks=element_line(size=0.4),
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    strip.text.x = element_text(size=10),
    strip.text.y = element_text(angle=0,size=10),
    plot.background=element_blank(),
    panel.border=element_blank(),
    strip.background = element_blank(),
    panel.background=element_rect(fill="grey80", colour="grey80"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ) + 
  guides(fill = guide_legend("P-value strength", ncol=1)) + 
  labs(x="Outcome",y="Exposure",title="")
hm









# hm <- ggplot(plotdf, aes(x=xvar, y=agecat, fill=pval_cat)) +
#   facet_grid(.~outcome_variable, scales = "free", space="free") +
#   geom_tile(colour="grey80",size=0.25) +
#   scale_x_discrete(limits = rev(levels(pooled_data$xvar)))+
#   scale_y_discrete(expand=c(0,0))+
#   theme_grey(base_size=10) +
#   scale_fill_manual(labels = levels(pooled_data$pval_cat),
#                     values = c(cols, "gray80"))+
#   theme(
#     #aspect.ratio = 1,
#     legend.title=element_text(color=textcol,size=8),
#     legend.margin = margin(grid::unit(0.1,"cm")),
#     legend.text=element_text(colour=textcol,size=7,face="bold"),
#     legend.key.height=grid::unit(0.2,"cm"),
#     legend.key.width=grid::unit(1,"cm"),
#     legend.position = "right",
#     axis.text.x=element_text(size=8,colour=textcol,angle=45,hjust=1),
#     axis.text.y=element_text(size=8,vjust = 0.2,colour=textcol),
#     axis.ticks=element_line(size=0.4),
#     plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
#     strip.text.x = element_text(size=10),
#     strip.text.y = element_text(angle=0,size=10),
#     plot.background=element_blank(),
#     panel.border=element_blank(),
#     strip.background = element_blank(),
#     panel.background=element_rect(fill="grey80", colour="grey80"),
#     panel.grid.major = element_blank(), panel.grid.minor = element_blank()
#   ) + 
#   guides(fill = guide_legend("P-value strength", ncol=1)) + 
#   labs(x="Exposure",y="Age category",title="") +
#   coord_flip()
