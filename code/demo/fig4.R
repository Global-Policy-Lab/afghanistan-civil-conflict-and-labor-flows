rm(list = ls()); gc()
library(dplyr); library(ggplot2)

################## fig 4a
plotDTF <- readRDS("./data/1-12-24fig4a.rds") # plot data

plotDTF$label <- c("2_Violence Y, Taliban Y", 
                   "4_Violence Y, Taliban N",
                   "3_Violence N, Taliban Y",
                   "5_Violence N, Taliban N",
                   "1_Low")

myColors <- c("lightgray", "azure4",
              "#55c667", "#1f968b",
              "#fde725", "orange" 
              
)

# median version
plot1 <- rbind(plotDTF, data.frame(model = "High-growing", 
                                   label = "0_No growing", 
                                   baselineMedian = 1 - sum(plotDTF$baselineMedian),
                                   harvestMedian = 1 - sum(plotDTF$harvestMedian)
)) %>%
  tidyr::pivot_longer(cols = c("baselineMedian", "harvestMedian")) %>%
  mutate(name = ifelse(name == "baselineMedian", "baseline", ".harvest")) %>%
  ggplot(aes(x = as.factor(name), y = value, fill = label)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(
    # breaks = c("2_Violence Y, Taliban Y", 
    #                            "4_Violence Y, Taliban N",
    #                            "3_Violence N, Taliban Y",
    #                            "5_Violence N, Taliban N"),
    values = myColors,
    # c(myColors[3:6], myColors[1:2]),
    labels = c(#"", "",
      "No poppy", "Low-growing",
      "Taliban = Yes\nViolence = Yes",
      "Taliban = Yes\nViolence = No",
      "Taliban = No\nViolence = Yes",
      "Taliban = No\nViolence = No"
    ),
    guide = guide_legend(reverse = TRUE)
  ) +
  coord_flip() +
  theme_void() +
  labs(fill = "Source of in-migrants\n(district type)",
       y = "",
       x = "",
       # title = "Composition of in-migrants to high-growing districts"
  ) +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.position = "bottom",
        legend.text = element_text(size=10),
        legend.title = element_text(size=10, face = "bold")
  ) +
  scale_x_discrete(labels = c("Harvest", "Baseline")) +
  scale_y_continuous(labels = scales::label_percent()) #+
# guides(fill = guide_legend(override.aes = list(values = c("white",
#                                                  "#fde725", "orange", 
#                                                  "#55c667", "#1f968b"
# )))) # this last row makes the transparency 100

pdf(paste0("fig4a.pdf"), width = 8, height = 4)
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()

################## fig 4b
rm(list = ls()); gc()

outDTFM4 <- readRDS("./data/fig4bcDemo_high.rds") # note that this is a subset of the original data

# categorical version
fit1 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn) 
             ) )

tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
fit1coefs <- lmtest::coeftest(fit1, vcov. = sandwich::vcovCL(fit1, cluster = as.factor(tmpDistID), type = "HC1")) # takes less than a minute!

outDTFM4 <- readRDS("./data/fig4bcDemo_low.rds") # note that this is a subset of the original data

# categorical version
fit2 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn)
             ))

tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
fit2coefs <- lmtest::coeftest(fit2, vcov. = sandwich::vcovCL(fit2, cluster = as.factor(tmpDistID), type = "HC1")) # takes less than a minute!

outDTF <- data.frame(label = c("High-growing",
                               "Low-growing"
),
estimate = NA,
CIlow = NA,
CIhigh = NA)


for (i in 1:2) {
  tmp <- get(paste0("fit", i, "coefs"))
  outDTF[i, "estimate"] <- tmp["poppyCatH", "Estimate"]
  outDTF[i, "CIlow"] <- tmp["poppyCatH", "Estimate"] - qnorm(.975)*tmp["poppyCatH", "Std. Error"]
  outDTF[i, "CIhigh"] <- tmp["poppyCatH", "Estimate"] + qnorm(.975)*tmp["poppyCatH", "Std. Error"]
}

outDTF$variance <- ((outDTF$CIhigh - outDTF$estimate)/qnorm(.975))^2
outDTF$p <- 2*pnorm(abs(outDTF$estimate)/sqrt(outDTF$variance), lower.tail = FALSE)


plot1 <- outDTF %>%
  ggplot(aes(label, estimate))+
  geom_pointrange(aes(ymin = CIlow, ymax = CIhigh) 
  ) + # fatten changes the circles only; size changes bars and legend circles
  theme_classic() + 
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) + 
  guides(color = guide_legend(override.aes = list(linetype = "blank")), 
         linetype = FALSE # suppress line for first plot; only do color
  ) +  # shape for the type of dot
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
  ) +
  labs(#title = paste0("Effect of violence on destination of movers: non-capitals"),
    # title = "Post-2017",
    # title = "Pre-2017",
    y = "Increase in in-migration, by type of source district",
    # y = "Increase in-migrant proportion from source districts\nwith specified characteristics (harvest compared to baseline)",
    x = ""
  ) +
  # labs(tag = "Violence = Yes \n\n\n\n\n Violence = No") +
  # theme(plot.tag.position = c(-.07, .57),
  #       text = element_text(size = 12),
  #       plot.margin = theme_get()$plot.margin + unit(c(0, 0, 0, 3), "cm")
  # ) +
  coord_flip(
    # clip = "off"
    # , ylim = c(0, 0.04)
  ) + 
  scale_x_discrete(labels = c("High-cultivation\nsources", "Low-cultivation\nsources")) + # bottom to top
  theme(axis.text.y  = element_text(hjust=0.5))

pdf(paste0("fig4b.pdf"), width = 7, height = 2)
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()

###### source results (fig 4c)
# violence H T
rm(list = ls()); gc()
outDTFM4 <- readRDS("./data/fig4cDemo_HVT.rds") # note that this is a subset of the original data

# categorical version
fit1 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn) 
                    # & year != 2016
                    # & year > 2014
                    # & tmpPoppy < 5000
             ) )

tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
fit1coefs <- lmtest::coeftest(fit1, vcov. = sandwich::vcovCL(fit1, cluster = as.factor(tmpDistID), type = "HC1")) # takes less than a minute!

### H_V_NonT
outDTFM4 <- readRDS("./data/fig4cDemo_H_V_NonT.rds") # note that this is a subset of the original data

# categorical version
fit2 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn)
                    # & year != 2016
                    # & year > 2014
                    # & tmpPoppy < 5000
             ))

tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
fit2coefs <- lmtest::coeftest(fit2, vcov. = sandwich::vcovCL(fit2, cluster = as.factor(tmpDistID), type = "HC1")) # takes less than a minute!


### H_V_NonT
outDTFM4 <- readRDS("./data/fig4cDemo_H_NonV_T.rds") # note that this is a subset of the original data

# categorical version
fit3 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn)
             ))

tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
fit3coefs <- lmtest::coeftest(fit3, vcov. = sandwich::vcovCL(fit3, cluster = as.factor(tmpDistID), type = "HC1")) # takes less than a minute!

### H_V_NonT
outDTFM4 <- readRDS("./data/fig4cDemo_H_NonV_NonT.rds") # note that this is a subset of the original data

# categorical version
fit4 <- lm(maxIn ~ poppyCat + log(areakm2) + 
             log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac, 
           data = outDTFM4 %>%
             filter(!is.na(maxIn)
             ))

tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
fit4coefs <- lmtest::coeftest(fit4, vcov. = sandwich::vcovCL(fit4, cluster = as.factor(tmpDistID), type = "HC1")) # takes less than a minute!

outDTF <- data.frame(model = rep("High-growing", 4), 
                     label = c("Violence Y, Taliban Y", ### NOTE CHANGED ORDER
                               "Violence Y, Taliban N",
                               "Violence N, Taliban Y",
                               "Violence N, Taliban N"
                     ),
                     estimate = NA,
                     CIlow = NA,
                     CIhigh = NA)

for (i in 1:4) {
  tmp <- get(paste0("fit", i, "coefs"))
  outDTF[i, "estimate"] <- tmp["poppyCatH", "Estimate"]
  outDTF[i, "CIlow"] <- tmp["poppyCatH", "Estimate"] - qnorm(.975)*tmp["poppyCatH", "Std. Error"]
  outDTF[i, "CIhigh"] <- tmp["poppyCatH", "Estimate"] + qnorm(.975)*tmp["poppyCatH", "Std. Error"]
}

outDTF$variance <- ((outDTF$CIhigh - outDTF$estimate)/qnorm(.975))^2
outDTF$p <- 2*pnorm(abs(outDTF$estimate)/sqrt(outDTF$variance), lower.tail = FALSE)

plot1 <- outDTF %>%
  mutate(label = paste0(c(1, 3, 2, 4), label)) %>%
  ggplot(aes(label, estimate))+
  geom_pointrange(aes(ymin = CIlow, ymax = CIhigh), 
                  color = c("#55c667", "#fde725", "#1f968b", "orange") # 5/14/23: add this to match barchart
  ) + # fatten changes the circles only; size changes bars and legend circles
  theme_classic() + 
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) + 
  guides(color = guide_legend(override.aes = list(linetype = "blank")), 
         linetype = FALSE # suppress line for first plot; only do color
  ) +  # shape for the type of dot
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
  ) +
  labs(#title = paste0("Effect of violence on destination of movers: non-capitals"),
    # title = "Post-2017",
    # title = "Pre-2017",
    # y = "Increase in-migrant proportion from source districts\nwith specified characteristics (harvest compared to baseline)",
    y = "Increase in in-migration, by type of source district",
    x = ""
  ) +
  labs(tag = "Taliban = No \n\n\n\n\n\n Taliban = Yes") +
  # labs(tag = "Violence = Yes \n\n\n\n\n Violence = No") +
  theme(plot.tag.position = c(-.07, .58),
        text = element_text(size = 12),
        plot.margin = theme_get()$plot.margin + unit(c(0, 0, 0, 3), "cm")
  ) +
  coord_flip(clip = "off"
             , ylim = c(-.005, 0.042)
  ) +
  annotate(x = 2.5, xend = 2.5, 
           y = -.025, yend = .042,
           geom = "segment",
           colour = "#a8a5a5", size = 1.4, alpha = .4) +
  # geom_vline(xintercept = 2.5, colour = "grey60", linetype = 2) +
  # scale_x_discrete(labels = c("Taliban = No", "Taliban = Yes", "Taliban = No", "Taliban = Yes")) # bottom to top 
  scale_x_discrete(labels = c("Violence = Yes", "Violence = No", "Violence = Yes", "Violence = No")) # bottom to top 

pdf(paste0("fig4c.pdf"), width = 7, height = 3.5)
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()






