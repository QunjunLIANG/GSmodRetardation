library(tidyverse)
library(ggeasy)
library(ggsignif)
library(patchwork)

# load the data
dat_sbj <- rio::import("inputs/Pip5_addCONNfunc.xlsx")

# plot settings
textSize <- 15
dotSize <- 2
jitterSize <- .01
plotAlpha <- 0.6

############################ figure 2 ########################################

# ALPS difference -------------------------------------------------------------
p_2a <- dat_sbj %>% 
  ggplot(aes(x = group, y = ALPS)) +
  geom_violin(aes(fill = group), alpha = plotAlpha, width = 0.55,
              trim = F, scale = "width",
              adjust=0.6,
              draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_point(size = 1, position = position_jitter(jitterSize)) +
  geom_signif(
    annotations = c("*"),
    textsize = 7, vjust = .7,
    y_position = c(2.4),
    xmin = c(1),
    xmax = c(2),
    tip_length = 0
  ) +
  scale_fill_manual(values = c("grey","steelblue","#EE0000B2")) +
  ylab("ALPS") +
  theme_classic() +
  easy_text_size(textSize) +
  easy_remove_x_axis(what = "title") +
  easy_remove_legend() 
p_2a

# ALPS, C protein and retardation correlation -------------------------------

p_2b <- dat_sbj %>% filter(group== "MDD") %>%
  ggplot(aes(x = ALPS, y = log_Cprotin, size = HAMD_item8)) +
  geom_point(alpha = 0.8) +
  xlim(c(1.1, 1.8)) +
  ylab("log hsCRP") + xlab("ALPS") +
  scale_size_continuous(range = c(3,7)) +
  theme_classic() +
  easy_add_legend_title("Retardation") +
  easy_text_size(textSize) + easy_move_legend(to = "top")
p_2b

p_2 <- p_2a + p_2b +
  patchwork::plot_layout(widths = c(1,1.3))
p_2


############################ figure 3 ########################################

# modulation effect of ALPS on retardation -------------------------------------
dat_mod_alps <- rio::import("onputs/Pip3_ALPSmodRetardation.xlsx") %>%
  mutate(logp = -log(pval)) %>%
  mutate(effectDirect = ifelse(Effect >= 0, "Positive", "Negative")) %>%
  mutate(effectSize = abs(Effect))
colnames(dat_mod_alps)[1] <- "ALPS"
dat_mod_alps$ALPS <- as.numeric(dat_mod_alps$ALPS)

p_3a <- ggplot(dat_mod_alps) +
  geom_point(aes(x = ALPS, y = logp, size = effectSize, color = effectDirect)) +
  geom_hline(yintercept = 3, linetype = 2) +
  #scale_x_continuous(breaks = seq(1.1, 1.75, by = 0.1)) +
  ylab("-log(p)") + xlab("ALPS demean") +
  ggtitle("Effect of hsCRP on Retardation") +
  ggstatsplot::theme_ggstatsplot() +
  easy_text_size(10) +
  easy_add_legend_title(size = "Effect size (A)",col = "Direction")
p_3a

# modulation effect of ALPS on functional connectivity -------------------------
dat_mod_alps_fc <- rio::import("onputs/Pip5_ALPSmodCONN.xlsx") %>%
  mutate(logp = -log(pval)) %>%
  mutate(effectDirect = ifelse(Effect >= 0, "Positive", "Negative")) %>%
  mutate(effectSize = abs(Effect))
colnames(dat_mod_alps_fc)[1] <- "ALPS"
dat_mod_alps_fc$ALPS <- as.numeric(dat_mod_alps_fc$ALPS)

p_3b <- ggplot(dat_mod_alps_fc) +
  geom_point(aes(x = ALPS, y = logp, size = effectSize, color = effectDirect)) +
  geom_hline(yintercept = 3, linetype = 2) +
  #scale_x_continuous(breaks = seq(1.1, 1.75, by = 0.1)) +
  ylab("-log(p)") + xlab("ALPS demean") +
  ggtitle("Effect of hsCRP on functional connectivity") +
  ggstatsplot::theme_ggstatsplot() +
  easy_text_size(10) +
  easy_add_legend_title(size = "Effect size (B)",col = "Direction")
p_3b

p_3 <- p_3a / p_3b +
  patchwork::plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(plot.tag = element_text(face = "bold"))
p_3
