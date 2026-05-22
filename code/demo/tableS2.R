rm(list = ls()); gc()
library(dplyr); library(stargazer)

load("./demo/data/tableS2.Rdata") # loads outDTFM4, a subset of the original data with 120 obs

fitm14 <- lm(m14 ~ poppyCat + log(areakm2) + 
               log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
               provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
               trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
               barrenAreaFrac, 
             data = outDTFM4 %>%
               filter(!is.na(m14)) )
fitm14$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$m14) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]


fitp14 <- lm(p14 ~ poppyCat + log(areakm2) + 
               log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
               provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
               trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
               barrenAreaFrac, 
             data = outDTFM4 %>%
               filter(!is.na(p14)) )
fitp14$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$p14) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

majorityData <- outDTFM4 %>%
  mutate(distYear = paste0(distid, "_", year)) %>%
  filter(distYear %in% sureDYs$distYear)

fit6 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = majorityData %>%
             filter(!is.na(maxIn)))
fit6$tmpDistID <- majorityData$distid[!is.na(majorityData$maxIn) & !is.na(majorityData$diversity) & !is.na(majorityData$trackPathFrac)]

minorityData <- outDTFM4 %>%
  mutate(distYear = paste0(distid, "_", year)) %>%
  filter(!(distYear %in% sureDYs$distYear))

fit7 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = minorityData %>%
             filter(!is.na(maxIn)))
fit7$tmpDistID <- minorityData$distid[!is.na(minorityData$maxIn) & !is.na(minorityData$diversity) & !is.na(minorityData$trackPathFrac)]

mdls3 <- list(fitm14, fitp14, fit6, fit7)

# Calculate robust confidence intervals
se_robust <- function(x, dist = FALSE) {
  if (dist == FALSE) {
    lmtest::coeftest(x, vcov. = sandwich::vcovCL(x, cluster = as.factor(x$tmpDistID), type = "HC1"))[, "Std. Error"]
  } else if (dist == TRUE) {
    lmtest::coeftest(x, vcov. = sandwich::vcovCL(x, cluster = x$model$`as.factor(distid)`, type = "HC1"))[, "Std. Error"]
  }
}


seList3 <- lapply(mdls3, se_robust)
# seList[[10]] <- se_robust(fit9, dist = TRUE) # doesn't actually do anything 


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
                     omit = "as.factor",
                     report = ('vcsp'), # 5/13/26 this is new
                     significance.stars = FALSE # 5/13/26 this is new 
)

######################################## END ########################################
