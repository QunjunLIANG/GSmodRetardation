library(tidyverse)

# load the data -----------------------------
dat_raw <- rio::import("inputs/Pip5_addCONNfunc.xlsx")
dat_use <- dat_raw %>% filter(group == "MDD") %>%
  mutate(ALPSdemean = ALPS - mean(ALPS)) %>%
  mutate(log_Cprotindemean = log_Cprotin - mean(log_Cprotin)) %>%
  mutate(agedemean = age - mean(age))

##################### ALPS moderation effect to anhedonia ###################
model_lm <- lm(formula = HAMD_item7 ~ ALPSdemean*log_Cprotindemean + agedemean + gender, data = dat_use)
car::Anova(model_lm, type = 3)
# Response: HAMD_item7
# Sum Sq Df  F value Pr(>F)    
# (Intercept)                  345.20  1 378.1598 <2e-16 ***
# ALPSdemean                     0.00  1   0.0023 0.9619    
# log_Cprotindemean              0.00  1   0.0016 0.9681    
# agedemean                      0.06  1   0.0696 0.7928    
# gender                         0.17  1   0.1862 0.6676    
# ALPSdemean:log_Cprotindemean   2.53  1   2.7734 0.1010    
# Residuals                     55.68 61                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##################### ALPS moderation effect to insomnia ###################
model_lm <- lm(formula = HAMD_sleep ~ ALPSdemean*log_Cprotindemean + agedemean + gender, data = dat_use)
car::Anova(model_lm, type = 3)
# Response: HAMD_sleep
# Sum Sq Df  F value Pr(>F)    
# (Intercept)                  634.17  1 145.5859 <2e-16 ***
# ALPSdemean                     2.19  1   0.5025 0.4811    
# log_Cprotindemean              8.55  1   1.9635 0.1662    
# agedemean                      0.45  1   0.1036 0.7487    
# gender                         0.00  1   0.0004 0.9834    
# ALPSdemean:log_Cprotindemean   1.57  1   0.3605 0.5504    
# Residuals                    265.71 61                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

##################### ALPS moderation effect to anxiety ###################
model_lm <- lm(formula = HAMD_anxiety ~ ALPSdemean*log_Cprotindemean + agedemean + gender, data = dat_use)
car::Anova(model_lm, type = 3)
# Response: HAMD_anxiety
# Sum Sq Df  F value  Pr(>F)    
# (Intercept)                  1770.82  1 307.5480 < 2e-16 ***
# ALPSdemean                      0.54  1   0.0934 0.76099    
# log_Cprotindemean              31.55  1   5.4787 0.02254 *  
# agedemean                       1.29  1   0.2233 0.63820    
# gender                          1.78  1   0.3089 0.58038    
# ALPSdemean:log_Cprotindemean    0.02  1   0.0036 0.95221    
# Residuals                     351.23 61                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##################### ALPS moderation effect to agitation ###################
model_lm <- lm(formula = HAMD_item9 ~ ALPSdemean*log_Cprotindemean + agedemean + gender, data = dat_use)
car::Anova(model_lm, type = 3)
# Response: HAMD_item9
# Sum Sq Df F value    Pr(>F)    
# (Intercept)                  44.491  1 46.6700 4.509e-09 ***
# ALPSdemean                    0.036  1  0.0376    0.8468    
# log_Cprotindemean             1.312  1  1.3760    0.2453    
# agedemean                     0.688  1  0.7218    0.3989    
# gender                        0.089  1  0.0929    0.7616    
# ALPSdemean:log_Cprotindemean  0.654  1  0.6862    0.4107    
# Residuals                    58.152 61                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1        