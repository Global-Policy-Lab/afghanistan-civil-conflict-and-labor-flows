rm(list=ls()); gc()
library(sf)
library(dplyr)
library(ggplot2)

## for file ./demo/data/figS1.Rdata
# load("/data/afg_satellite/xtai/afghanShapeAllInfo.Rdata")
# 
# covariates <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/3-13-23covariates.rds") %>%
#   select(-geometry)
# 
# afghanShape <- afghanShape %>%
#   left_join(covariates %>% 
#               filter(year == 2018) %>%
#               select(distid, poppyCat, notGovt),
#             by = c("DISTID" = "distid"))
# 
# afghanShape <- afghanShape %>%
#   select(poppyCat, notGovt)
# save(afghanShape, file = "./demo/data/figS1.Rdata")

load("./demo/data/figS1.Rdata")

viridisThreeColor <- c("#fde72566", "#21918c66", "#44015466") # 66 for 40% transparency

ggplot() + 
  geom_sf(data = afghanShape, aes(fill = poppyCat)) +
  scale_fill_manual(values = viridisThreeColor,
                    labels = c("None", "Low", "High"),
                    name = "Poppy Cultivation\nIntensity"
  ) #+
  # geom_sf(data = sites, size = .6) #+ NOTE: commented out for privacy


ggplot() + 
  geom_sf(data = afghanShape, aes(fill = as.factor(notGovt))) + 
  scale_fill_viridis_d(alpha = .5,
                       name = "Taliban\npresence"
  ) #+
  # geom_sf(data = sites, size = .6) #+ NOTE: commented out for privacy

