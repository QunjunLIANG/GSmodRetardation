library(tidyverse)
library(ggeasy)
library(ggsci)

############################# preparing the data ###############################

# load the data
dat_use <- rio::import("inputs/Pip5_addCONNfunc.xlsx")

############################# Age and Gender Differences #######################

# table summarize
dat_use %>% mutate(HAMD_item8 = factor(HAMD_item8)) %>%
  table1::table1(data = ., ~ age + gender + 
                   Cprotin +  HAMD_total + HAMD_item8 | group)

# age difference 
t.test(data = dat_use, age ~ group)
# Welch Two Sample t-test
# data:  age by group
# t = -0.82876, df = 127.62, p-value = 0.4088
# alternative hypothesis: true difference in means between group HC and group MDD is not equal to 0
# 95 percent confidence interval:
#   -5.410032  2.216002
# sample estimates:
#   mean in group HC mean in group MDD 
# 33.46269          35.05970 

# gender difference 
chisq.test(dat_use$group, dat_use$gender)
# Pearson's Chi-squared test with Yates' continuity correction
# data:  dat_hamd_sub_log_group$group and dat_hamd_sub_log_group$gender
# X-squared = 4.5585, df = 1, p-value = 0.03276

############################# ALPS difference ##################################

bruceR::MANOVA(data = dat_use, subID = "ID",
               dv = "ALPS", between = "group", covariate = c("age","gender")) %>%
  bruceR::EMMEANS(effect = "group")
# ────────────────────────────────────────────────────────────────────
# MS   MSE df1 df2     F     p     η²p [90% CI of η²p]  η²G
# ────────────────────────────────────────────────────────────────────
# group   0.233 0.034   1 130 6.943  .009 **    .051 [.007, .124] .051
# age     0.056 0.034   1 130 1.681  .197       .013 [.000, .063] .013
# gender  0.135 0.034   1 130 4.031  .047 *     .030 [.000, .093] .030
# ────────────────────────────────────────────────────────────────────
# ───────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df      t     p     Cohen’s d [95% CI of d]
# ───────────────────────────────────────────────────────────────────────
# MDD - HC   -0.085 (0.032) 130 -2.635  .009 **  -0.466 [-0.815, -0.116]
# ───────────────────────────────────────────────────────────────────────

############################# ALPS, Cprotin and symptom ########################

# Cprotein & HAMD score
cor.test(data = dat_use, ~ log_Cprotin + HAMD_total)
# data:  log_Cprotin and HAMD_total
# t = -0.82326, df = 65, p-value = 0.4134
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.3336521  0.1420907
# sample estimates:
#   cor 
# -0.1015851  

# Cprotein & retardation correlation
cor.test(data = dat_use, ~ log_Cprotin + HAMD_item8)
# Pearson's product-moment correlation
# 
# data:  log_Cprotin and HAMD_item8
# t = 0.28855, df = 65, p-value = 0.7738
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.2062126  0.2736255
# sample estimates:
#        cor 
# 0.03576777 

# ALPS & Cprotein correlation
cor.test(data = dat_use, ~ ALPS + log_Cprotin)
# Pearson's product-moment correlation
# data:  ALPS and log_Cprotin
# t = 1.9295, df = 65, p-value = 0.05803
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.007896049  0.447919683
# sample estimates:
#      cor 
# 0.232754 

# ALPS & retardation correlation
cor.test(data = dat_use, ~ ALPS + HAMD_item8)
# Pearson's product-moment correlation
# data:  ALPS and HAMD_item8
# t = 0.90566, df = 65, p-value = 0.3685
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.1321205  0.3426514
# sample estimates:
#       cor 
# 0.1116309 

