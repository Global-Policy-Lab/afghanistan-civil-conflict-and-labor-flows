rm(list = ls()); gc()
library(ggplot2); library(dplyr)

### covariates of interest: areakm2, nonPoppy (agHectares - poppy), TOTAL
# new variables to add:
# 2. area
# 3. agricultural area
# 4. derived vars: population density, cultivation density, fraction of poppy/agricultural areas 

#### input data sets 
districtInfo <- readRDS("/data/afg_anon/displacement_analysis/district_ids_with_info.rds")
unodcData <- read.csv("/home/xtai/climate/data/poppy_1994-2020.csv")
agHectaresLong <- readRDS("/home/xtai/climate/data/7-14-22agHectares.rds")
load("/data/afg_satellite/xtai/afghanShapeAllInfo.Rdata")
agrPixels <- readRDS("/home/xtai/tmp/agrPixels.Rds") %>% # this has areakm2 that is calculated using sf (see screenshot) 
  select(DISTID, areakm2)


################ wide to long
# poppy data
poppyLong <- unodcData %>%
  tidyr::pivot_longer(cols = starts_with("X"), 
                      names_to = "year", 
                      names_prefix = "X", 
                      names_transform = list(year = as.numeric),
                      values_to = "poppy") %>%
  filter(year %in% 2014:2020) 
# 398*7 years = 2786 rows --- correct

### check: 1000 is roughly top decile 
tmp <- poppyLong$poppy
tmp[is.na(tmp)] <- 0
quantile(tmp, .9)
#    90% 
# 1065.5 

##########################################
covariates <- data.frame(distid = districtInfo$distid, 
                         year = c(rep(2014, 398), rep(2015, 398), rep(2016, 398),
                                  rep(2017, 398), rep(2018, 398), rep(2019, 398),
                                  rep(2020, 398)))
# 2786 obs

########## join everything 
covariates <- covariates %>%
  left_join(districtInfo %>%
              select(distid, provincialCapital, TOTAL, region), by = c("distid")) %>%
  left_join(poppyLong, 
            by = c("distid", "year")) %>%
  mutate(tmpPoppy = ifelse(!is.na(poppy), poppy, 0)) %>%
  left_join(agHectaresLong,
            by = c("distid" = "distIDs",
                   "year")) %>%
  mutate(nonPoppy2 = agHectares - tmpPoppy) %>%
  mutate(nonPoppy2 = ifelse(nonPoppy2 <= 0, 1, nonPoppy2)) %>%
  left_join(afghanShape %>% 
              select(DISTID, diversity, cultivatedFrac, roadDensity, builtUpFracPop, trackPathFrac, healthPer100000, majority, barrenAreaFrac),
            by = c("distid" = "DISTID")) %>%
  mutate(poppyCat = ifelse(tmpPoppy >= 1000, "H", ifelse(tmpPoppy > 0, "L", "N"))) %>%
  left_join(agrPixels, 
            by = c("distid" = "DISTID"))

covariates$poppyCat <- factor(covariates$poppyCat, levels = c("N", "L", "H"))

sigarControl <- read.csv("/data/afg_anon/displacement_analysis/SIGARcontrol.csv") %>%
  mutate(talibanConInf = ifelse(Oct.2017.Assessment %in% c("INS Control", "INS In!uence"), 1, 0),
         notGovt = ifelse(Oct.2017.Assessment %in% c("INS Control", "INS In!uence", "Contested"), 1, 0))

covariates <- covariates %>%
  left_join(sigarControl %>%
              select(distid, notGovt, talibanConInf))
saveRDS(covariates, file = "/home/xtai/climate/3-8-23migrationCleanCode/output/3-13-23covariates.rds")



