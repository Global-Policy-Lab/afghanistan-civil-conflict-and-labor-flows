# tables S1-S3
########################### REGS START HERE ############################
rm(list = ls()); gc()
library(stargazer)
# robustness checks:
# - A. original # fit1
# - B. continuous poppy # fit2
# - C. instead of days 15-35 can use days 1-45 # outcome2, fit3
# - D. instead of days 15-35 can use days 1-45 with 14-day window # outcome3, fit4
# - E. in-migrant stays outcome # outcome4, fit5
# - F. measurement: prop1 > .5 # fit6
# - G. measurement: prop1 <= .5 # fit7
# - H. tmpPoppy < 5000 # fit8
# in-migrant goes back: outcome5, fit10
# in-migrant goes back 90-day version: outcome6, fit11
# 15-day reference period instead of k = 30: outcome7, fitk15
# 45-day reference period instead of k = 30: outcome8, fitk45
# subtract 14 days: outcome9, fitm14
# add 14 days: outcome10, fitp14

### 12/20/24
# update so that supplementary table S1's columns 2 and 3 are k = 15 and k = 45 
# supplementary table S1's original columns 4 and 5 should go to table S2 as columns 3 and 4
# supplementary table S2 columns 1 and 2 are new: peak NDVI + 14, - 14

# mdls1 <- list(fit1, fitk15, fitk45, fit3, fit4, fit8, fit2)
# mdls2 <- list(fit5, fit10, fit11) # these are for return of seasonal migrants
# mdls3 <- list(fitm14, fitp14, fit6, fit7)

outcome1 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/6-5-23inMigRegOutcome_2020.rds")
outcome2 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/9-26-23inMigRegOutcome_1-45_2020.rds")
outcome3 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/9-26-23inMigRegOutcome_1-45_14days_2020.rds")
outcome4 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/10-16-23inMigStaysRegOutcome_2020.rds")
outcome5 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/12-19-23inMigGoesBackRegOutcome_2020.rds") # 12/19/23: in migrant goes back
outcome6 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/1-23-24inMigGoesBackRegOutcome_90.rds") # 1/23/24: in migrant goes back 

outcome7 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/11-14-24inMigRegOutcome_2020_k15.rds")
outcome8 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/11-14-24inMigRegOutcome_2020_k45.rds")
outcome9 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/8-9-24review1/outcome-14days.rds")
outcome10 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/8-9-24review1/outcome14days.rds")


covariates <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/3-13-23covariates.rds") %>%
  select(-geometry)
# this version should have poppyCat, talibanCurrent and inaccessibleCurrent

outDTFM4 <- covariates %>%
  left_join(outcome1, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) 

# categorical version
fit1 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn)) )
# summary(fit1)
# table(outDTFM4$poppyCat[!is.na(outDTFM4$maxIn)])
# N   L   H 
# 469 144  70 

fit1$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

# lmtest::coeftest(fit1, vcov. = sandwich::vcovCL(fit1, cluster = fit1$model$`as.factor(prov)`, type = "HC1")) # takes less than a minute!

# continuous version
fit2 <- lm(maxIn ~ log(tmpPoppy+1) + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, data = outDTFM4 %>%
             filter(!is.na(maxIn)))
fit2$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
# summary(fit2)

# lmtest::coeftest(fit2, vcov. = sandwich::vcovCL(fit2, cluster = fit2$model$prov, type = "HC1")) # takes less than a minute!

# days 1-45
outDTFM4 <- covariates %>%
  left_join(outcome2, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) 
fit3 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, data = outDTFM4 %>%
             filter(!is.na(maxIn)))
fit3$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
# summary(fit3)

# lmtest::coeftest(fit3, vcov. = sandwich::vcovCL(fit3, cluster = fit3$model$prov, type = "HC1")) # takes less than a minute!

# days 1-45, 14 day window
outDTFM4 <- covariates %>%
  left_join(outcome3, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) 
fit4 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, data = outDTFM4 %>%
             filter(!is.na(maxIn)))
fit4$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
# summary(fit4)

# lmtest::coeftest(fit4, vcov. = sandwich::vcovCL(fit4, cluster = fit4$model$prov, type = "HC1")) # takes less than a minute!

# in-migrant stays
outDTFM4 <- covariates %>%
  left_join(outcome4, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100))
fit5 <- lm(maxIn ~ poppyCat + log(areakm2) +
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, data = outDTFM4 %>%
             filter(!is.na(maxIn)))
fit5$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
# summary(fit5)

# lmtest::coeftest(fit5, vcov. = sandwich::vcovCL(fit5, cluster = fit5$model$prov, type = "HC1")) # takes less than a minute!

# restrict data set to district years with prop1 > .5
bestDatesLong <- readRDS("/data/afg_satellite/bestdates/6-22-22bestDatesLong_M4.rds") 
sureDYs <- bestDatesLong %>%
  filter(prop1 > .5) %>% 
  # filter(prop1 <= .5) %>%
  mutate(distYear = paste0(distIDs, "_", year)) # 658 out of 2737

outDTFM4 <- covariates %>%
  left_join(outcome1, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) %>%
  mutate(distYear = paste0(distid, "_", year)) %>%
  filter(distYear %in% sureDYs$distYear)

fit6 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn)))
fit6$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

# summary(fit6)
# table(outDTFM4$poppyCat[!is.na(outDTFM4$maxIn)])
# 
# N   L   H 
# 237  57  25 

# lmtest::coeftest(fit6, vcov. = sandwich::vcovCL(fit6, cluster = fit6$model$prov, type = "HC1")) # takes less than a minute!

outDTFM4 <- covariates %>%
  left_join(outcome1, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) %>%
  mutate(distYear = paste0(distid, "_", year)) %>%
  filter(!(distYear %in% sureDYs$distYear))

fit7 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn)))
fit7$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
# summary(fit7)
# table(outDTFM4$poppyCat[!is.na(outDTFM4$maxIn)])
# 
# N   L   H 
# 232  87  45 

outDTFM4 <- covariates %>%
  left_join(outcome1, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) 
fit8 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn) & tmpPoppy < 5000) )
fit8$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac) & outDTFM4$tmpPoppy < 5000]


# in-migrant goes back
outDTFM4 <- covariates %>%
  left_join(outcome5, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100))
fit10 <- lm(maxIn ~ poppyCat + log(areakm2) +
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, data = outDTFM4 %>%
             filter(!is.na(maxIn)))
fit10$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

# in-migrant goes back 90-day version
outDTFM4 <- covariates %>%
  left_join(outcome6, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100))
fit11 <- lm(maxIn ~ poppyCat + log(areakm2) +
              log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
              provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
              trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
              barrenAreaFrac, data = outDTFM4 %>%
              filter(!is.na(maxIn)))
fit11$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

####################### 1/23/24 new version of returns ####################### 
# aside: this is the calculation in the paper that 62% of seasonal migrants go back
# Coefficients:
#                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    2.267e-01  1.149e-01   1.973 0.048726 *  
# poppyCatL                      1.583e-02  8.442e-03   1.876 0.060940 .  
# poppyCatH                      6.698e-02  1.378e-02   4.860 1.32e-06 ***

#### baseline percentage in-migrants that went back 
# median(outDTFM4$baseline[outDTFM4$poppyCat == "H"])
# 0.4619544

(0.4619544+6.698e-02)*(0.0361361+.0271)-(0.4619544)*(0.0361361) # 0.01675452
0.01675452/.0271 # 62% of new in-migrants go back 

##############################################################################

outDTFM4 <- covariates %>%
  left_join(outcome7, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) 
# categorical version
fitk15 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn)) )
fitk15$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

outDTFM4 <- covariates %>%
  left_join(outcome8, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) 
fitk45 <- lm(maxIn ~ poppyCat + log(areakm2) + 
               log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
               provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
               trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
               barrenAreaFrac, 
             data = outDTFM4 %>%
               filter(!is.na(maxIn)) )
fitk45$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]


outDTFM4 <- covariates %>%
  left_join(outcome9, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) 
# categorical version
fitm14 <- lm(maxIn ~ poppyCat + log(areakm2) + 
               log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
               provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
               trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
               barrenAreaFrac, 
             data = outDTFM4 %>%
               filter(!is.na(maxIn)) )
fitm14$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

outDTFM4 <- covariates %>%
  left_join(outcome10, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) 
fitp14 <- lm(maxIn ~ poppyCat + log(areakm2) + 
               log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
               provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
               trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
               barrenAreaFrac, 
             data = outDTFM4 %>%
               filter(!is.na(maxIn)) )
fitp14$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]


mdls1 <- list(fit1, fitk15, fitk45, fit3, fit4, fit8, fit2)
mdls2 <- list(fit5, fit10, fit11) # these are for return of seasonal migrants
mdls3 <- list(fitm14, fitp14, fit6, fit7)

# Calculate robust confidence intervals
se_robust <- function(x, dist = FALSE) {
  if (dist == FALSE) {
    lmtest::coeftest(x, vcov. = sandwich::vcovCL(x, cluster = as.factor(x$tmpDistID), type = "HC1"))[, "Std. Error"]
  } else if (dist == TRUE) {
    lmtest::coeftest(x, vcov. = sandwich::vcovCL(x, cluster = x$model$`as.factor(distid)`, type = "HC1"))[, "Std. Error"]
  }
}

seList1 <- lapply(mdls1, se_robust)
seList2 <- lapply(mdls2, se_robust)
seList3 <- lapply(mdls3, se_robust)

stargazer::stargazer(fit1, fitk15, fitk45, fit3, fit4, fit8, fit2,
                     se = seList1,
                     # se = lapply(mdls, se_robust),
                     omit.stat=c("ser","f"), 
                     digits = 4, 
                     font.size = "scriptsize",
                     keep = c("poppyCatH", "poppyCatL", "*tmpPoppy"), 
                     covariate.labels = c("Poppy Low (1-999)", 
                                          "Poppy High ($>=$1000ha)",
                                          "Continuous poppy (ha, log)"
                     ),
                     omit = "as.factor")


stargazer::stargazer(fit5, fit10, fit11,
                     se = seList2,
                     # se = lapply(mdls, se_robust),
                     omit.stat=c("ser","f"), 
                     digits = 4, 
                     font.size = "scriptsize",
                     keep = c("poppyCatH", "poppyCatL", "*tmpPoppy"), 
                     covariate.labels = c("Poppy Low (1-999)", 
                                          "Poppy High ($>=$1000ha)",
                                          "Continuous poppy (ha, log)"
                     ),
                     omit = "as.factor")

stargazer::stargazer(fitm14, fitp14, fit6, fit7,
                     se = seList3,
                     # se = lapply(mdls, se_robust),
                     omit.stat=c("ser","f"), 
                     digits = 4, 
                     font.size = "scriptsize",
                     keep = c("poppyCatH", "poppyCatL", "*tmpPoppy"), 
                     covariate.labels = c("Poppy Low (1-999)", 
                                          "Poppy High ($>=$1000ha)",
                                          "Continuous poppy (ha, log)"
                     ),
                     omit = "as.factor")

######################################## END ########################################
