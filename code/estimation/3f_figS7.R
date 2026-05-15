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

# remove > 5000 for results at destination
################ fig 3b
new3 <- lm(maxIn ~ poppyCat*violence*taliban + log(areakm2) + log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac
           , 
           data = outDTFM4 %>%
             filter(!is.na(maxIn) 
                    & tmpPoppy < 5000
             )%>%
             rename(violence = violence_monthBefore) %>%
             # mutate(taliban = ifelse(scale4 == TRUE | scale5 == TRUE, 1, 0)))
             mutate(taliban = ifelse(notGovt, 1, 0)))
# mutate(taliban = ifelse(talibanConInf, 1, 0)))

# fit2coefs <- lmtest::coeftest(new3, vcov. = sandwich::vcovCL(new3, cluster = new3$model$prov, type = "HC1")) # takes less than a minute!
tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac) & outDTFM4$tmpPoppy < 5000]
fit2coefs <- lmtest::coeftest(new3, vcov. = sandwich::vcovCL(new3, cluster = as.factor(tmpDistID), type = "HC1")) # takes less than a minute!


keepNames <- c("poppyCatH", "violence", "poppyCatH:violence", "taliban", "poppyCatH:taliban", "violence:taliban", "poppyCatH:violence:taliban")
estimates <- fit2coefs[keepNames, "Estimate"] # keeps the order of names too 
clusteredVcov <- sandwich::vcovCL(new3, cluster = as.factor(tmpDistID), type = "HC1")
clusteredVcov <- clusteredVcov[keepNames, keepNames]

# vecs of interest:
getCI <- function(myVec, estimates, vCov){
  # estimate is vec*estimates;  variance is vec*clusteredVcov*vec
  estimate <- myVec %*% estimates
  variance <- myVec %*% vCov %*% myVec
  ciLow <- estimate - qnorm(.975)*sqrt(variance)
  ciHigh <- estimate + qnorm(.975)*sqrt(variance)
  ret <- list(estimate = estimate, variance = variance, ciLow = ciLow, ciHigh = ciHigh)
  return(ret)
}

outDTF <- data.frame(model = rep("High-growing", 4), 
                     label = c("Violence N, Taliban N",
                               "Violence Y, Taliban N",
                               "Violence N, Taliban Y",
                               "Violence Y, Taliban Y"),
                     estimate = NA,
                     CIlow = NA,
                     CIhigh = NA)

vecs <- list(c(1, 0, 0, 0, 0, 0, 0),
             c(1, 1, 1, 0, 0, 0, 0),
             c(1, 0, 0, 1, 1, 0, 0),
             c(1, 1, 1, 1, 1, 1, 1))
for (i in 1:4) {
  tmp <- getCI(myVec = vecs[[i]], estimates = estimates, vCov = clusteredVcov)
  outDTF[i, "estimate"] <- tmp$estimate
  outDTF[i, "CIlow"] <- tmp$ciLow
  outDTF[i, "CIhigh"] <- tmp$ciHigh
}


plot1 <- outDTF %>%
  mutate(label = paste0(4:1, label)) %>%
  ggplot(aes(label, estimate))+
  geom_pointrange(aes(ymin = CIlow, ymax = CIhigh),
                  color = c("orange", "#fde725", "#1f968b", "#55c667")
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
    y = "Increase in in-migration from baseline during harvest",
    x = ""
  ) +
  labs(tag = "Taliban = No \n\n\n\n\n Taliban = Yes") +
  # labs(tag = "Violence = Yes \n\n\n\n\n Violence = No") +
  theme(plot.tag.position = c(-.07, .57),
        text = element_text(size = 12),
        plot.margin = theme_get()$plot.margin + unit(c(0, 0, 0, 3), "cm")
  ) +
  coord_flip(clip = "off"
             , ylim = c(-.01, 0.075)
  ) +
  annotate(x = 2.5, xend = 2.5, y = -.063, yend = .075,
           geom = "segment",
           colour = "#a8a5a5", size = 1.4, alpha = .4) +
  # geom_vline(xintercept = 2.5, colour = "grey60", linetype = 2) +
  # scale_x_discrete(labels = c("Taliban = No", "Taliban = Yes", "Taliban = No", "Taliban = Yes")) # bottom to top 
  scale_x_discrete(labels = c("Violence = Yes", "Violence = No", "Violence = Yes", "Violence = No")) # bottom to top 

      
# remove > 5000 for results at destination
# pdf(paste0("/home/xtai/climate/3-8-23migrationCleanCode/output/general/9-26-23fig3b_robust.pdf"), width = 8, height = 3)
pdf(paste0("/home/xtai/climate/3-8-23migrationCleanCode/output/general/1-10-24fig3b_robust.pdf"), width = 8, height = 3)
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()
# rsync -P -e 'ssh -J xtai@hilbert.ucdavis.edu' xtai@fati.ischool.berkeley.edu:/home/xtai/climate/3-8-23migrationCleanCode/output/general/* /Users/xtai/Desktop/seasonalMigration/paper/general
