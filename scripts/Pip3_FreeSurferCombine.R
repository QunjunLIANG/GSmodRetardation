library(tidyverse)
library(interactions)

############################# preparing the data ###########################

# load the data
dat_ori <- rio::import("inputs/Pip2_subjectData.xlsx") %>%
  dplyr::select(-c(38:105))

# load the freesurfer results - volumne
dat_vol <- rio::import("inputs/FreeSurferResults/aseg_stats.csv") %>%
  dplyr::select(1,5:9,23:27) 
colnames(dat_vol)[1] <- "particpant_id"
dat_vol$particpant_id <- str_replace(dat_vol$particpant_id, pattern = "_T1w", replacement = "")
dat_vol_motorcircuit_lh <- dat_vol %>% dplyr::select(2:6)
dat_vol_motorcircuit_rh <- dat_vol %>% dplyr::select(7:ncol(dat_vol))
dat_vol_motorcircuit <- (dat_vol_motorcircuit_lh + dat_vol_motorcircuit_rh)/2
colnames(dat_vol_motorcircuit) <- str_replace(colnames(dat_vol_motorcircuit), pattern = "Left-", replacement = '')
colnames(dat_vol_motorcircuit)[1] <- "Cerebellum"

# load the freesurfer results - DKT atlas
dat_dk_lh <- rio::import("inputs/FreeSurferResults/lh.DKTatlas.thickness.csv") %>%
  dplyr::select(2:32)
dat_dk_rh <- rio::import("inputs/FreeSurferResults/rh.DKTatlas.thickness.csv") %>%
  dplyr::select(2:32)
dat_dk <- (dat_dk_lh + dat_dk_rh)/2
colnames(dat_dk) <- str_replace(colnames(dat_dk), pattern = "lh_", replacement = "")

# merge the data tabel
dat_use <- cbind(dat_ori, dat_dk, dat_vol_motorcircuit)

########################## pick the area for motor circuits ####################
colnames(dat_use)
col_motor <- c(66, 68, 73, 78:82) 

# scale the morphology of motor circuits
dat_scale <- caret::preProcess(x = dat_use %>%  filter(group == "MDD") %>% 
                    dplyr::select(col_motor), method = c("center", "scale"))
dat_scale2 <- predict(dat_scale, dat_use %>%  filter(group == "MDD") %>% 
                        dplyr::select(col_motor))

# combine and make a longer data for advanced modelling
dat_motor <- dat_use %>% filter(group == "MDD") %>%
  dplyr::select(1:46) %>% cbind(dat_scale2) %>%
  mutate(ALPSdemean = ALPS - mean(ALPS)) %>%
  mutate(log_Cprotindemean = log_Cprotin - mean(log_Cprotin)) %>%
  mutate(agedemean = age - mean(age))
dat_motor <-  pivot_longer(data = dat_motor, cols = 47:54, 
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
ggsave(filename = "onputs/Fig4_ALPSmod_structure.png", width = 9, height = 8)
ggsave(filename = "onputs/Fig4_ALPSmod_structure.tiff", width = 9, height = 8)

# export the data
rio::export(dat_use, file = "inputs/Pip4_addFreeSurfer.xlsx")
