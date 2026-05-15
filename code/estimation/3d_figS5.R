######################## other two-way EDA ########################
# updated 9/27/23
rm(list = ls()); gc()
library(dplyr)
outcome1 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/6-5-23inMigRegOutcome_2020.rds")
covariates <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/3-13-23covariates.rds") %>%
  dplyr::select(-geometry)# this version should have poppyCat, talibanCurrent and inaccessibleCurrent

newViolence <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/6-5-23violenceDest_2020.rds")

outDTFM4 <- covariates %>%
  left_join(outcome1, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) %>%
  left_join(newViolence %>%
              mutate(violence_monthBefore = ifelse(numEvents_monthBeforePeak > 0, 1, 0)) #%>%
            ,
            by = c("distid", "year")) %>%
  filter(!is.na(maxIn))

# pdf(paste0("/home/xtai/climate/output/5-6-23twoWayEDA.pdf"), width = 10, height = 4)
pdf(paste0("/home/xtai/climate/3-8-23migrationCleanCode/output/general/9-26-23twoWayEDA.pdf"), width = 10, height = 4)
plot1 <- ggplot(outDTFM4, aes(x = notGovt, fill = as.factor(violence_monthBefore))) +
  geom_bar() +
  labs(fill = "Violence",
       x = "Taliban presence",
       y = "Count") +
  viridis::scale_fill_viridis(discrete = TRUE)

plot2 <- ggplot(outDTFM4, aes(x = notGovt, fill = as.factor(violence_monthBefore))) +
  geom_bar(position = "fill") +
  labs(fill = "Violence",
       x = "Taliban presence",
       y = "Proportion") +
  viridis::scale_fill_viridis(discrete = TRUE)

gridExtra::grid.arrange(plot1, plot2, nrow = 1, top = "Districts with Taliban presence are more likely to have violence")

plot1 <- ggplot(outDTFM4, aes(x = violence_monthBefore, fill = as.factor(notGovt))) +
  geom_bar() +
  labs(fill = "Taliban presence",
       x = "Violence",
       y = "Count") +
  viridis::scale_fill_viridis(discrete = TRUE)

plot2 <- ggplot(outDTFM4, aes(x = violence_monthBefore, fill = as.factor(notGovt))) +
  geom_bar(position = "fill") +
  labs(fill = "Taliban presence",
       x = "Violence",
       y = "Proportion") +
  viridis::scale_fill_viridis(discrete = TRUE)

gridExtra::grid.arrange(plot1, plot2, nrow = 1, top = "Districts with violence are more likely to have Taliban presence")

dev.off()

