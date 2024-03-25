################################## FIGURES ################################
library(dplyr); library(ggplot2)

############### fig 2a ###############
rm(list = ls()); gc()
setwd("./demo")
outDTF <- readRDS("./data/fig2aDemo.rds") # note that this is a subset of the original data
distYears <- readRDS("./data/6-5-23distYearsIncluded_2020.rds") # for missing data handling

forPlot <- outDTF %>%
  mutate(distidYear = paste0(distid, "_", year)) %>% # new for missing data
  filter(distidYear %in% distYears) %>% # new for missing data
  group_by(poppyCat, daysFromPeak) %>%
  summarize(meanEffect = mean(estimate),
            # count = n(),
            var = sum(se^2, na.rm = TRUE)/(n()^2)) %>%
  mutate(conf.low = meanEffect - qnorm(.975)*sqrt(var),
         conf.high = meanEffect + qnorm(.975)*sqrt(var))

viridisThreeColor <- c("#fde725", "#21918c", "#440154")

pdf(paste0("fig2a.pdf"), width = 10, height = 5)
forPlot %>%
  ggplot(aes(daysFromPeak, meanEffect, col = poppyCat))+
  geom_point()+
  geom_pointrange(aes(ymin = (conf.low),
                      ymax = (conf.high),
                      col = poppyCat),
                  position = position_dodge(width = .5)) +
  labs(
    x = "Days since district's peak NDVI (16-day MODIS)",
    y = "Daily increase in in-migration rate from baseline",
    # y = "Daily increase in out-migration rate from baseline",
    col = "Poppy Cultivation\nIntensity") +
  geom_vline(xintercept = 0, color = "red3", linetype = 2) + 
  geom_hline(yintercept = 0, color = "red3") +
  scale_x_continuous(breaks = seq(-180, 180, 10)) +
  # scale_y_continuous(breaks = seq(-1, 1.75, .25)) +
  theme_classic(base_size = 14) +
  scale_color_manual(values = viridisThreeColor,
                     labels = c("None", "Low", "High")
  ) +
  theme(
    legend.position = c(1, 1.02),
    legend.justification = c("right", "top"),
    # legend.title = element_blank(),
    legend.text = element_text(size=14),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
    axis.text.x = element_text(size = 14)) +
  guides(col = guide_legend(override.aes = list(shape = 15, size = 1.7)))
dev.off()

############### fig 2b ###############
outDTF <- readRDS("./data/fig2bDemo.rds") # note that this is a subset of the original data
forPlot <- outDTF %>%
  mutate(distidYear = paste0(distid, "_", year)) %>% # new for missing data
  filter(distidYear %in% distYears) %>% # new for missing data
  group_by(poppyCat, daysFromPeak) %>%
  summarize(meanEffect = mean(estimate),
            # count = n(),
            var = sum(se^2, na.rm = TRUE)/(n()^2)) %>%
  mutate(conf.low = meanEffect - qnorm(.975)*sqrt(var),
         conf.high = meanEffect + qnorm(.975)*sqrt(var))

viridisThreeColor <- c("#fde725", "#21918c", "#440154")

pdf(paste0("fig2b.pdf"), width = 10, height = 5)
forPlot %>%
  ggplot(aes(daysFromPeak, meanEffect, col = poppyCat))+
  geom_point()+
  geom_pointrange(aes(ymin = (conf.low),
                      ymax = (conf.high),
                      col = poppyCat),
                  position = position_dodge(width = .5)) +
  labs(
    x = "Days since district's peak NDVI (16-day MODIS)",
    # y = "Daily increase in in-migration rate from baseline",
    y = "Daily increase in out-migration rate from baseline",
    col = "Poppy Cultivation\nIntensity") +
  geom_vline(xintercept = 0, color = "red3", linetype = 2) + 
  geom_hline(yintercept = 0, color = "red3") +
  scale_x_continuous(breaks = seq(-180, 180, 10)) +
  # scale_y_continuous(breaks = seq(-1, 1.75, .25)) +
  theme_classic(base_size = 14) +
  scale_color_manual(values = viridisThreeColor,
                     labels = c("None", "Low", "High")
  ) +
  theme(
    legend.position = c(1, 1.02),
    legend.justification = c("right", "top"),
    # legend.title = element_blank(),
    legend.text = element_text(size=14),
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
    axis.text.x = element_text(size = 14)) +
  guides(col = guide_legend(override.aes = list(shape = 15, size = 1.7)))
dev.off()

############### fig 2c ###############
rm(list = ls()); gc()
outDTFM4 <- readRDS("./data/fig2cDemo.rds")

viridisThreeColor <- c("#440154E6", "#21908C80", "#FDE72580") # first is purple

# position = "identity" overlaps the bars (default is to stack, which we don't want)
plot1 <- outDTFM4 %>%
  ggplot(aes(x = baseline, fill = poppyCat)) +
  geom_histogram(
    position = "identity"#,
    # binwidth = 5000,
    # alpha = .6
  ) + 
  # theme_classic() +
  # scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    # values = rev(c("#440154", "#21908C", "#FDE725")),
    values = rev(viridisThreeColor),
    labels = c("None", "Low", "High")
  ) +
  xlim(c(0, .23)) +
  geom_vline(xintercept = c(median(outDTFM4$baseline[outDTFM4$poppyCat == "N"]), # 0.04390979
                            median(outDTFM4$baseline[outDTFM4$poppyCat == "L"]), # 0.04326012
                            median(outDTFM4$baseline[outDTFM4$poppyCat == "H"])), # 0.0361361
             color = rev(scales::viridis_pal()(3)),
             lwd = 1.3) +
  geom_vline(xintercept = c(median(outDTFM4$baseline[outDTFM4$poppyCat == "N"]),
                            median(outDTFM4$baseline[outDTFM4$poppyCat == "L"]),
                            median(outDTFM4$baseline[outDTFM4$poppyCat == "H"])),
             color = "black",
             lty = 3) +
  theme_classic(base_size = 14) +
  labs(x = "Baseline in-migration rate",
       fill = "Poppy Cultivation\nIntensity",
       # y = "Percentage"
       y = "Count"
  ) +
  theme(legend.position = c(1, 1.02),
        legend.justification = c("right", "top"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y = element_text(
          # angle = 90, 
          # vjust = 0,
          vjust = .5,
          hjust = 1)
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) # this last row makes the transparency 100


plot2 <- outDTFM4 %>%
  ggplot(aes(x = harvest, fill = poppyCat)) +
  geom_histogram(
    position = "identity"#,
    # binwidth = 5000,
    # alpha = 0.5
  ) +
  scale_fill_manual(
    # values = rev(c("#440154", "#21908C", "#FDE725")),
    values = rev(viridisThreeColor),
    labels = c("None", "Low", "High")
  )+ 
  labs(x = "In-migration rate during harvest",
       fill = "Poppy Cultivation\nIntensity",
       y = "Count"
  )+
  xlim(c(0, .23)) +
  geom_vline(xintercept = c(median(outDTFM4$harvest[outDTFM4$poppyCat == "N"]),
                            median(outDTFM4$harvest[outDTFM4$poppyCat == "L"]),
                            median(outDTFM4$harvest[outDTFM4$poppyCat == "H"])),
             color = rev(scales::viridis_pal()(3)),
             lwd = 1.3)+
  geom_vline(xintercept = c(median(outDTFM4$harvest[outDTFM4$poppyCat == "N"]),
                            median(outDTFM4$harvest[outDTFM4$poppyCat == "L"]),
                            median(outDTFM4$harvest[outDTFM4$poppyCat == "H"])),
             color = "black",
             lty = 3) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = c(1, 1.02),
    legend.justification = c("right", "top"),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14),
    # legend.position = "none",
    axis.text=element_text(size=14),
    axis.title=element_text(size=14),
    axis.text.y = element_text(
      # angle = 90, 
      # vjust = 0,
      vjust = .5,
      hjust = 1)
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) 

pdf(paste0("fig2d.pdf"), width = 8, height = 7)
gridExtra::grid.arrange(plot1, plot2, nrow = 2)
dev.off()



############### fig 2d ###############
rm(list = ls()); gc()
outDTFM4 <- readRDS("./data/fig2dDemo.rds") # note that this is a subset of the original data

# categorical version
fit1 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn)) )

tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
fit1coefs <- lmtest::coeftest(fit1, vcov. = sandwich::vcovCL(fit1, cluster = as.factor(tmpDistID), type = "HC1")) # takes less than a minute!

# for plot
tmp <- fit1coefs[c("poppyCatL", "poppyCatH"), c("Estimate", "Std. Error")]
plotDTF <- data.frame(model = "Overall", label = c("Low-cultivation", "High-cultivation"), estimate = tmp[, "Estimate"], CIlow = tmp[, "Estimate"] - 1.96*tmp[, "Std. Error"], CIhigh = tmp[, "Estimate"] + 1.96*tmp[, "Std. Error"])

plot1 <- plotDTF %>%
  ggplot(aes(label, estimate)) +
  theme_classic()+
  geom_pointrange(aes(ymin = CIlow, ymax = CIhigh),
                  # position = position_dodge(width = .4),
                  # size = .4, fatten = 2) +
                  # size = 1.4, fatten = 2
  ) + # fatten changes the circles only; size changes bars and legend circles
  coord_flip() + 
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) + 
  guides(color = guide_legend(override.aes = list(linetype = "blank")), 
         # linetype = guide_legend(override.aes = list(shape = NA))
         linetype = FALSE # suppress line for first plot; only do color
  ) +  # shape for the type of dot
  # scale_y_continuous(limits = c(-.002, .032)) +
  theme(axis.text = element_text(size = 14),
        axis.title=element_text(size=14)
  ) +
  labs(#title = paste0("Effect of violence on destination of movers: non-capitals"),
    y = "Increase in in-migration from baseline during harvest",
    x = "",
  ) 

pdf(paste0("fig2c.pdf"), width = 6.5, height = 1.5)
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()

# sample code to get p-values 
# 2*pnorm(tmp[2, 1]/tmp[2, 2], lower.tail = FALSE) # 0.001256662

# sample code to translate percentage to number of people, as reported in main text
# sum(covariates$TOTAL[covariates$year == 2016 & covariates$poppyCat == "H"])*.0271 # 75321.39
# 
# covariates %>% 
#   select(distid, TOTAL, poppyCat, year) %>%
#   filter(poppyCat == "H" & year == 2016) %>%
#   arrange(desc(TOTAL))
#    distid  TOTAL poppyCat year
# 1    1804 145446        H 2016
# 42   2415  13680        H 2016

