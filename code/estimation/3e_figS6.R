rm(list = ls()); gc()
outcome1 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/6-5-23inMigRegOutcome_2020.rds")
covariates <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/3-13-23covariates.rds") %>%
  dplyr::select(-geometry) # this version should have poppyCat, talibanCurrent and inaccessibleCurrent

newViolence <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/6-5-23violenceDest_2020.rds")

outDTFM4 <- covariates %>%
  left_join(outcome1, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) %>%
  left_join(newViolence %>%
              mutate(violence_monthBefore = ifelse(numEvents_monthBeforePeak > 0, 1, 0)) #%>%
            # dplyr::select(-starts_with("numEvents"))
            ,
            by = c("distid", "year")) %>%
  filter(!is.na(maxIn)) # %>%

####### FIG S5: loess curves EDA 
pdf(paste0("/home/xtai/climate/3-8-23migrationCleanCode/output/general/1-10-24figA2.pdf"), width = 7, height = 5)

outDTFM4 %>%
  # filter(tmpPoppy < 5000) %>%
  ggplot(aes(tmpPoppy, violence_monthBefore)) +
  geom_point() +
  geom_smooth(method = loess) +
  labs(
    x = "Number of hectares of opium poppy cultivation",
    y = "Violent events in the month before peak NDVI"
  ) +
  theme(#plot.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14),
  )

outDTFM4 %>%
  mutate(taliban = notGovt) %>%
  # mutate(taliban = ifelse(scale4 == TRUE | scale5 == TRUE, 1, 0)) %>%
  # filter(tmpPoppy < 5000) %>%
  ggplot(aes(tmpPoppy, taliban)) +
  geom_point() +
  geom_smooth(method = loess) +
  labs(
    x = "Number of hectares of opium poppy cultivation",
    y = "Taliban presence"
  )+
  theme(#plot.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14),
  )


outDTFM4 %>%
  # filter(tmpPoppy < 5000) %>%
  ggplot(aes(tmpPoppy, maxIn)) +
  geom_point() +
  geom_smooth(method = loess) +
  labs(
    x = "Number of hectares of opium poppy cultivation",
    y = "Difference between harvest and \nbaseline in-migration rates"
  )+
  theme(#plot.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14),
  )

dev.off()
