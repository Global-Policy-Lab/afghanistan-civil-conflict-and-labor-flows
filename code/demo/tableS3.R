########################### REGS START HERE ############################
rm(list = ls()); gc()
library(dplyr); library(stargazer)


outDTFM4 <- readRDS("./demo/data/tableS3.rds") # this is a subset of the original data with 120 obs

fit5 <- lm(same30 ~ poppyCat + log(areakm2) +
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, data = outDTFM4 %>%
             filter(!is.na(same30)))
fit5$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$same30) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

# col 2
fit10 <- lm(prev30 ~ poppyCat + log(areakm2) +
              log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
              provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
              trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
              barrenAreaFrac, data = outDTFM4 %>%
              filter(!is.na(prev30)))
fit10$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$prev30) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

# col 3 in-migrant goes back
fit11 <- lm(prev90 ~ poppyCat + log(areakm2) +
              log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
              provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
              trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
              barrenAreaFrac, data = outDTFM4 %>%
              filter(!is.na(prev90)))
fit11$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$prev90) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]


mdls2 <- list(fit5, fit10, fit11) # these are for return of seasonal migrants

# Calculate robust confidence intervals
se_robust <- function(x, dist = FALSE) {
  if (dist == FALSE) {
    lmtest::coeftest(x, vcov. = sandwich::vcovCL(x, cluster = as.factor(x$tmpDistID), type = "HC1"))[, "Std. Error"]
  } else if (dist == TRUE) {
    lmtest::coeftest(x, vcov. = sandwich::vcovCL(x, cluster = x$model$`as.factor(distid)`, type = "HC1"))[, "Std. Error"]
  }
}

seList2 <- lapply(mdls2, se_robust)

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
                     omit = "as.factor",
                     report = ('vcsp'), # 5/13/26 this is new
                     significance.stars = FALSE # 5/13/26 this is new 
)

######################################## END ########################################
