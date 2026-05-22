rm(list = ls()); gc()

## Table S4
outDTFM4 <- readRDS("./demo/data/fig2dDemo.rds") # note that this is a subset of the original data with 120 obs

tmp <- outDTFM4 %>%
  filter(!is.na(maxIn)) %>%
  select(poppyCat, areakm2, 
         nonPoppy2, TOTAL,# year, prov,
         provincialCapital, cultivatedFrac, roadDensity, builtUpFracPop,
         trackPathFrac, healthPer100000, diversity, majority,
         barrenAreaFrac
  )

stargazer::stargazer(tmp, 
                     title = "Descriptive statistics for covariates",
                     digits = 2,
                     covariate.labels = c(#"Increase in in-migration",
                       "Area (squared km)",
                       "Non-poppy cultivation (ha)",
                       "Population",
                       "Provincial capital",
                       "Fraction living in cultivated areas",
                       "Road density",
                       "Fraction living in built-up areas",
                       "Fraction of roads that are tracks and paths",
                       "Health facilities per 100,000",
                       "Ethnic diversity",
                       "Proportion of barren land")
)

############################### Table S5 ############################### 
outDTFM4 <- readRDS("./demo/data/tableS5.rds") # this is a subset of the original data with 120 obs

tmp <- outDTFM4 %>%
  select(tmpPoppy,
         poppyH, poppyL,# year, prov,
         violence, taliban,
         maxIn, starts_with("outcome")
  )

stargazer::stargazer(tmp, 
                     title = "Descriptive statistics for variables of interest",
                     digits = 2,
                     covariate.labels = c(
                       "Poppy cultivation (ha)",
                       "High poppy cultivation",
                       "Low poppy cultivation",
                       "Violence",
                       "Taliban presence",
                       "Increase in in-migration",
                       "Increase in proportion from high cultivation districts",
                       "Increase in proportion from low cultivation districts",
                       "Increase in proportion from high, violent, Taliban districts",
                       "Increase in proportion from high, violent, non-Taliban districts",
                       "Increase in proportion from high, non-violent, Taliban districts",
                       "Increase in proportion from high, non-violent, non-Taliban districts"
                     )
)


################################# Table S6 #################################
# fig 2c (reg results)
rm(list = ls()); gc()
outDTFM4 <- readRDS("./demo/data/tableS5.rds") # this is a subset of the original data with 120 obs

# categorical version
fit1 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn)) )

fit1$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]


################ fig 3b
new3 <- lm(maxIn ~ poppyCat*violence*taliban + log(areakm2) + log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac
           , 
           data = outDTFM4 %>%
             filter(!is.na(maxIn) 
             ))

new3$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

##### HIGH/LOW results
# categorical version
fit3 <- lm(outcomeHigh ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(outcomeHigh) 
                    # & year < 2017
                    # & year > 2017
                    # & tmpPoppy < 5000
             ) )

fit3$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$outcomeHigh) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

fit4 <- lm(outcomeLow ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(outcomeLow)
             ))

fit4$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$outcomeLow) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

###### source results 
fit5 <- lm(outcomeHVT ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(outcomeHVT) 
             ) )

fit5$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$outcomeHVT) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

# categorical version
fit6 <- lm(outcomeHVnonT ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(outcomeHVnonT)
             ))

fit6$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$outcomeHVnonT) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

fit7 <- lm(outcomeHnonVT ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(outcomeHnonVT)
                    # & year != 2016
                    # & year > 2014
                    # & tmpPoppy < 5000
             ))

fit7$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$outcomeHnonVT) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

fit8 <- lm(outcomeHnonVnonT ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(outcomeHnonVnonT)
             ))

fit8$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$outcomeHnonVnonT) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]

## eradication 
eradicationData <- read.csv("./demo/data/eradication_2014-2016.csv")
eradicationLong <- eradicationData %>%
  tidyr::pivot_longer(cols = starts_with("X"), 
                      names_to = "year", 
                      names_pattern = "X(.*)_eradication",
                      names_transform = list(year = as.numeric),
                      values_to = "eradication") 

checkEradication <- outDTFM4 %>%
  left_join(eradicationLong, 
            by = c("distid", "year")) %>%
  mutate(eradication = ifelse(!is.na(eradication), eradication, 0)) %>%
  filter(year %in% 2014:2016) %>%
  dplyr::select(distid, year, tmpPoppy, poppyCat, eradication) %>%
  mutate(eradicationYN = ifelse(eradication > 0, 1, 0))

checkEradication <- checkEradication %>%
  mutate(totalPoppy = tmpPoppy + eradication,
         eradPercent = eradication/totalPoppy) %>%
  mutate(eradPercent = ifelse(is.na(eradPercent), 0, eradPercent))

outDTFM4 <- outDTFM4 %>%
  left_join(checkEradication %>%
              dplyr::select(distid, year, eradication, eradicationYN, eradPercent),
            by = c("distid", "year")) %>%
  filter(year %in% 2014:2016)

eradication1 <- lm(maxIn ~ poppyCat*eradPercent + log(areakm2) + log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
                     provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
                     trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
                     barrenAreaFrac
                   , 
                   data = outDTFM4 %>%
                     filter(!is.na(maxIn) 
                            # & tmpPoppy < 12000
                     ))

eradication1$tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]





mdls <- list(fit1, new3, fit3, fit4, fit5, fit7, fit6, fit8, eradication1) # switch 6 and 7
# Calculate robust confidence intervals
se_robust <- function(x) {
  lmtest::coeftest(x, vcov. = sandwich::vcovCL(x, cluster = as.factor(x$tmpDistID), type = "HC1"))[, "Std. Error"]
}
stargazer::stargazer(fit1, new3, fit3, fit4, fit5, fit7, fit6, fit8, eradication1,
                     se = lapply(mdls, se_robust),
                     omit.stat=c("ser","f"), 
                     digits = 4, 
                     font.size = "scriptsize",
                     keep = c("poppyCatH", "poppyCatL", "*tmpPoppy", "*areakm2", "*nonPoppy2", "TOTAL", "provincialCapital", "cultivatedFrac", "roadDensity", "builtUpFracPop", "trackPathFrac", "healthPer100000", "diversity", "barrenAreaFrac", "*poppy", "*violence", "*taliban", "erad*"), 
                     column.sep.width = "0pt",
                     no.space = TRUE,
                     covariate.labels = c("Poppy Low (1-999)",
                                          "Poppy High ($>$1000ha)",
                                          "Violent events",
                                          "Taliban presence",
                                          "eradPercent",
                                          "Land area (km$^2$, log)",
                                          "Other cultivation (ha, log)",
                                          "Population",
                                          "Provincial Capital",
                                          "Fraction of population\\\\ in cultivated areas",
                                          "Road density",
                                          "Fraction of population\\\\ in built-up areas",
                                          "Fraction of roads that\\\\ are tracks and paths",
                                          "Health facilities\\\\ per 100,000",
                                          "Ethnic diversity",
                                          "Fraction of barren land area",
                                          "Poppy Low:Violence",
                                          "Poppy High:Violence",
                                          "Poppy Low:Taliban",
                                          "Poppy High:Taliban",
                                          "Violence:Taliban",
                                          "Low:Violence:Taliban",
                                          "High:Violence:Taliban",
                                          "poppyCatL:eradPercent",
                                          "poppyCatH:eradPercent"
                                          
                     ),
                     # dep.var.labels = "",
                     # column.labels = c(
                     #   # "Violence",
                     #   # "Intense violence",
                     #   # "Taliban",
                     #   "Violence*Taliban",
                     #   # "Intense violence*Taliban",
                     #   "Removing outliers" # "(4) removing outliers"
                     # ),
                     # dep.var.caption = "Dependent variable: excess in-migration during harvest",
                     omit = "as.factor")

