library(tidyverse)
library(interactions)

############################# preparing the data ###########################

# load the data
dat_use <- rio::import("inputs/Pip5_addCONNfunc.xlsx")

########################## pick the area for motor circuits ####################
colnames(dat_use)
col_motor <- c(32:39) 

# scale the morphology of motor circuits
dat_scale <- caret::preProcess(x = dat_use %>%  filter(group == "MDD") %>% 
                    dplyr::select(col_motor), method = c("center", "scale"))
dat_scale2 <- predict(dat_scale, dat_use %>%  filter(group == "MDD") %>% 
                        dplyr::select(col_motor))

# combine and make a longer data for advanced modelling
dat_motor <- dat_use %>% filter(group == "MDD") %>%
  dplyr::select(1:31) %>% cbind(dat_scale2) %>%
  mutate(ALPSdemean = ALPS - mean(ALPS)) %>%
  mutate(log_Cprotindemean = log_Cprotin - mean(log_Cprotin)) %>%
  mutate(agedemean = age - mean(age))
dat_motor <-  pivot_longer(data = dat_motor, cols = col_motor, 
                           names_to = "ROI", values_to = "morphology") %>%
  mutate(ROI = factor(ROI, level = c("precentral_thickness","postcentral_thickness","superiorparietal_thickness",
                                     "Caudate","Pallidum","Putamen","Thalamus","Cerebellum")))

########################## moderation model #################################

# building the three-variable interaction model
model_lm <- lm(formula = morphology ~ ALPSdemean*log_Cprotindemean*ROI + agedemean + gender, 
               data = dat_motor)

bruceR::GLM_summary(model_lm)
# Model Fit:
#   F(33, 502) = 1.32, p = 0.115    
# R² = 0.07966 (Adjusted R² = 0.01916)

# check the significance of the interacted effect
car::Anova(model_lm, type = 3)
# Response: morphology
# Sum Sq  Df F value  Pr(>F)  
# (Intercept)                        0.95   1  0.9798 0.32272  
# ALPSdemean                         0.12   1  0.1267 0.72203  
# log_Cprotindemean                  3.91   1  4.0366 0.04506 *
# ROI                                0.73   7  0.1075 0.99787  
# agedemean                          3.86   1  3.9854 0.04644 *
# gender                             5.98   1  6.1823 0.01323 *
# ALPSdemean:log_Cprotindemean       3.79   1  3.9152 0.04840 *
# ALPSdemean:ROI                     3.40   7  0.5014 0.83361  
# log_Cprotindemean:ROI              7.69   7  1.1343 0.33999  
# ALPSdemean:log_Cprotindemean:ROI  13.86   7  2.0461 0.04787 *
# Residuals                        485.94 502                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# simple slop test
sim_slopes(model_lm, pred = log_Cprotindemean, modx = ALPSdemean, mod2 = ROI)
# ████████████████████████████████ While ROI (2nd moderator) = precentral_thickness ████████████████████████████████ 
# 
# JOHNSON-NEYMAN INTERVAL
# 
# When ALPSdemean is OUTSIDE the interval [0.00, 19.50], the slope of log_Cprotindemean is p < .05.
# 
# Note: The range of observed values of ALPSdemean is [-0.36, 0.27]
# 
# ████████████████████████████████ While ROI (2nd moderator) = postcentral_thickness ███████████████████████████████ 
# 
# JOHNSON-NEYMAN INTERVAL
# 
# When ALPSdemean is INSIDE the interval [-0.24, -0.01], the slope of log_Cprotindemean is p < .05.
# 
# Note: The range of observed values of ALPSdemean is [-0.36, 0.27]
# 
# █████████████████████████████ While ROI (2nd moderator) = superiorparietal_thickness █████████████████████████████ 
# 
# JOHNSON-NEYMAN INTERVAL
# 
# When ALPSdemean is INSIDE the interval [-0.67, -0.06], the slope of log_Cprotindemean is p < .05.
# 
# Note: The range of observed values of ALPSdemean is [-0.36, 0.27]
# 
# █████████████████████████████████████ While ROI (2nd moderator) = Cerebellum █████████████████████████████████████ 
# 
# JOHNSON-NEYMAN INTERVAL
# 
# When ALPSdemean is OUTSIDE the interval [-2.78, 0.05], the slope of log_Cprotindemean is p < .05.
# 
# Note: The range of observed values of ALPSdemean is [-0.36, 0.27]




p_interac <- interact_plot(model_lm,
                           pred = log_Cprotindemean, 
                           modx = ALPSdemean, 
                           modx.values = c(-0.36, 0, 0.27),
                           modx.labels = c("min","mean","max"),
                           mod2 = ROI, 
                           plot.points = TRUE) +
  xlab("log hsCRP (demean)") + ylab("Morphology (z-scored)")
p_interac


