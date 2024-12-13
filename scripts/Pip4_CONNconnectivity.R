library(tidyverse)

# prepare the data ---------------------------------------
dat_use <- rio::import("inputs/Pip5_addCONNfunc.xlsx")

# modualtion effect -------------------------------------
dat_use <- dat_use %>% filter(group == "MDD") %>%
  mutate(ALPSdemean = ALPS - mean(ALPS)) %>%
  mutate(log_Cprotindemean = log_Cprotin - mean(log_Cprotin)) %>%
  mutate(agedemean = age - mean(age))
model_lm <- lm(formula = ROIconSMArCereb45l ~ ALPSdemean*log_Cprotindemean + agedemean + gender, data = dat_use)
bruceR::GLM_summary(model_lm)
# Model Fit:
#   F(5, 61) = 4.18, p = 0.002 ** 
#   R² = 0.25518 (Adjusted R² = 0.19413)
# ─────────────────────────────────────────────────────────────────────────────────────
# β    S.E.      t     p        [95% CI of β] r(partial) r(part)
# ─────────────────────────────────────────────────────────────────────────────────────
# ALPS              -0.512 (0.144) -3.542 <.001 *** [-0.801, -0.223]     -0.413  -0.391
# log_Cprotin        4.043 (1.048)  3.858 <.001 *** [ 1.948,  6.138]      0.443   0.426
# age               -0.112 (0.112) -1.001  .321     [-0.337,  0.112]     -0.127  -0.111
# gendermale         0.101 (0.115)  0.875  .385     [-0.129,  0.330]      0.111   0.097
# ALPS:log_Cprotin  -3.814 (1.028) -3.710 <.001 *** [-5.870, -1.758]     -0.429  -0.410
# ─────────────────────────────────────────────────────────────────────────────────────

car::Anova(model_lm, type = 3)
# Response: ROIconSMArCereb45l
# Sum Sq Df F value    Pr(>F)    
# (Intercept)                  0.00274  1  0.1179 0.7324750    
# ALPSdemean                   0.07600  1  3.2703 0.0754751 .  
# log_Cprotindemean            0.13360  1  5.7483 0.0195810 *  
# agedemean                    0.02329  1  1.0023 0.3207035    
# gender                       0.01780  1  0.7659 0.3849315    
# ALPSdemean:log_Cprotindemean 0.31992  1 13.7654 0.0004499 ***
# Residuals                    1.41768 61                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

interactions::sim_slopes(model_lm, johnson_neyman = T, 
                         pred = "log_Cprotindemean", modx = "ALPSdemean")
# JOHNSON-NEYMAN INTERVAL 
# When ALPSdemean is OUTSIDE the interval [0.02, 0.27], the slope of log_Cprotindemean is p < .05.
# Note: The range of observed values of ALPSdemean is [-0.36, 0.27]

