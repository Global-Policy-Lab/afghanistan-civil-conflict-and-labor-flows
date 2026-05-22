########################### REGS START HERE ############################
rm(list = ls()); gc()
library(dplyr); library(stargazer)

outDTFM4 <- readRDS("./demo/data/tableS1.rds") # this is a subset of the original data with 120 obs
# col 1
fit1 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn)) )

fit1$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

# col 2
fitk15 <- lm(k15 ~ poppyCat + log(areakm2) + 
               log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
               provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
               trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
               barrenAreaFrac, 
             data = outDTFM4 %>%
               filter(!is.na(k15)) )
fitk15$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$k15) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

# col 3 
fitk45 <- lm(k45 ~ poppyCat + log(areakm2) + 
               log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
               provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
               trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
               barrenAreaFrac, 
             data = outDTFM4 %>%
               filter(!is.na(k45)) )
fitk45$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$k45) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

# col 4: days 1-45
fit3 <- lm(window45 ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, data = outDTFM4 %>%
             filter(!is.na(window45)))
fit3$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$window45) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

# col 5: days 1-45, 14 day continuous
fit4 <- lm(cont14 ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, data = outDTFM4 %>%
             filter(!is.na(cont14)))
fit4$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$cont14) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
# summary(fit4)

# col 6
fit8 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn) & tmpPoppy < 5000) )
fit8$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac) & outDTFM4$tmpPoppy < 5000]

# col 7: continuous version
fit2 <- lm(maxIn ~ log(tmpPoppy+1) + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, data = outDTFM4 %>%
             filter(!is.na(maxIn)))
fit2$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]


mdls1 <- list(fit1, fitk15, fitk45, fit3, fit4, fit8, fit2)

# Calculate robust confidence intervals
se_robust <- function(x, dist = FALSE) {
  if (dist == FALSE) {
    lmtest::coeftest(x, vcov. = sandwich::vcovCL(x, cluster = as.factor(x$tmpDistID), type = "HC1"))[, "Std. Error"]
  } else if (dist == TRUE) {
    lmtest::coeftest(x, vcov. = sandwich::vcovCL(x, cluster = x$model$`as.factor(distid)`, type = "HC1"))[, "Std. Error"]
  }
}

seList1 <- lapply(mdls1, se_robust)

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
                     omit = "as.factor",
                     report = ('vcsp'), # 5/13/26 this is new
                     significance.stars = FALSE # 5/13/26 this is new 
)
