library(tidyverse)
library(ggeasy)
# library(emmeans)
# library(interactions)
# library(sjPlot)
# library(lsmeans)

############################# preparing the data ###########################

# load the data
dat_use <- rio::import("inputs/Pip5_addCONNfunc.xlsx") 

############################# ALPS differences ###########################

dat_use %>%
  bruceR::MANOVA(subID = "ID", between = "group", dv = "ALPS",
                 covariate = c("gender","age")) %>%
  bruceR::EMMEANS(effect = "group")
# ────────────────────────────────────────────────────────────────────
# MS   MSE df1 df2     F     p     η²p [90% CI of η²p]  η²G
# ────────────────────────────────────────────────────────────────────
# group   0.233 0.034   1 130 6.943  .009 **    .051 [.007, .124] .051
# gender  0.135 0.034   1 130 4.031  .047 *     .030 [.000, .093] .030
# age     0.056 0.034   1 130 1.681  .197       .013 [.000, .063] .013
# ────────────────────────────────────────────────────────────────────
# ───────────────────────────────────────────────────────────────────────
# Contrast Estimate    S.E.  df      t     p     Cohen’s d [95% CI of d]
# ───────────────────────────────────────────────────────────────────────
# MDD - HC   -0.085 (0.032) 130 -2.635  .009 **  -0.466 [-0.815, -0.116]
# ───────────────────────────────────────────────────────────────────────

############################# Modulation of ALPS  ###########################
# extract MDD's data
dat_mdd <- dat_use %>% filter(group =="MDD") %>%
  mutate(ALPSdemean = ALPS - mean(ALPS)) %>%
  mutate(log_Cprotindemean = log_Cprotin - mean(log_Cprotin)) %>%
  mutate(agedemean = age - mean(age))

# modulation effect

model_lm <- lm(formula = HAMD_item8 ~ ALPSdemean*log_Cprotindemean + agedemean + gender, data = dat_mdd)

car::Anova(model_lm, type = 3)
# Response: HAMD_item8
# Sum Sq Df  F value Pr(>F)    
# (Intercept)                  169.932  1 140.0348 <2e-16 ***
# ALPSdemean                     0.455  1   0.3746 0.5428    
# log_Cprotindemean              0.428  1   0.3526 0.5548    
# agedemean                      0.938  1   0.7734 0.3826    
# gender                         0.636  1   0.5244 0.4717    
# ALPSdemean:log_Cprotindemean   6.725  1   5.5421 0.0218 *  
# Residuals                     74.023 61                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

bruceR::GLM_summary(model_lm)
# Model Fit:
# F(5, 61) = 1.49, p = 0.206    
# R² = 0.10895 (Adjusted R² = 0.03592)
#
# Standardized Coefficients (β):
# Outcome Variable: HAMD_item8
# N = 67
# ─────────────────────────────────────────────────────────────────────────────────────────────────
# β    S.E.      t     p        [95% CI of β] r(partial) r(part)
# ─────────────────────────────────────────────────────────────────────────────────────────────────
# ALPSdemean                     0.077 (0.126)  0.612  .543     [-0.175,  0.330]      0.078   0.074
# log_Cprotindemean              0.078 (0.131)  0.594  .555     [-0.184,  0.339]      0.076   0.072
# agedemean                      0.108 (0.123)  0.879  .383     [-0.138,  0.354]      0.112   0.106
# gendermale                    -0.091 (0.126) -0.724  .472     [-0.342,  0.160]     -0.092  -0.088
# ALPSdemean:log_Cprotindemean  -0.294 (0.125) -2.354  .022 *   [-0.544, -0.044]     -0.289  -0.285
# ─────────────────────────────────────────────────────────────────────────────────────────────────
interactions::sim_slopes(model_lm, johnson_neyman = T, 
                         pred = "log_Cprotindemean", modx = "ALPSdemean")
# JOHNSON-NEYMAN INTERVAL 
# When ALPSdemean is OUTSIDE the interval [-0.23, 0.32], the slope of log_Cprotindemean is p < .05.
# Note: The range of observed values of ALPSdemean is [-0.36, 0.27]

