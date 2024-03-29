######### fig 3a
rm(list = ls()); gc()
library(dplyr); library(ggplot2)
outcome1 <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/6-5-23inMigRegOutcome_2020.rds")
covariates <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/3-13-23covariates.rds") %>%
  dplyr::select(-geometry) # this version should have poppyCat, notGovt

newViolence <- readRDS("/home/xtai/climate/3-8-23migrationCleanCode/output/6-5-23violenceDest_2020.rds")

outDTFM4 <- covariates %>%
  left_join(outcome1, by = c("distid", "year")) %>%
  mutate(prov = floor(distid/100)) %>%
  left_join(newViolence %>%
              # mutate(violence_monthBefore = ifelse(numEvents_monthBeforePeak > 0, 1, 0)) #%>% # ORIGINAL
            # mutate(violence_monthBefore = ifelse(numEvents_monthBeforePeak > 2, 1, 0)) #%>% # more events
            # mutate(violence_monthBefore = ifelse(numCas_monthBeforePeak > 9, 1, 0)) #%>% # more cas
            mutate(violence_monthBefore = ifelse(numEvents_monthBeforePeak > 2 & numCas_monthBeforePeak > 9, 1, 0)) #%>% # more cas
            # dplyr::select(-starts_with("numEvents"))
            ,
            by = c("distid", "year")) %>%
  filter(!is.na(maxIn)) # %>%

### quantiles for SI
quantile(outDTFM4$numCas_monthBeforePeak, seq(.1, .9, by = .1))
# 10% 20% 30% 40% 50% 60% 70% 80% 90% 
#   0   0   0   0   0   0   0   2   9 

quantile(outDTFM4$numEvents_monthBeforePeak, seq(.1, .9, by = .1))
# 10% 20% 30% 40% 50% 60% 70% 80% 90% 
# 0.0 0.0 0.0 0.0 0.0 0.0 0.3 1.0 2.0 


# xtabs(~ poppyCat + violence_monthBefore, data = outDTFM4)
#         violence_monthBefore
# poppyCat   0   1
#        N 709 238
#        L 233 119
#        H  52  69

pdf(paste0("/home/xtai/climate/3-8-23migrationCleanCode/output/general/1-9-24fig3a.pdf"), width = 8, height = 4)
outDTFM4 %>%
  filter(poppyCat == "H") %>%
  mutate(cat = case_when(
    notGovt == 1 & violence_monthBefore == 1 ~ "Taliban = Yes, Violence = Yes",
    notGovt == 1 & violence_monthBefore == 0 ~ "Taliban = Yes, Violence = No",
    notGovt == 0 & violence_monthBefore == 1 ~ "Taliban = No, Violence = Yes",
    notGovt == 0 & violence_monthBefore == 0 ~ "Taliban = No, Violence = No"
  )
  ) %>%
  ggplot(aes(x = maxIn, fill = as.factor(cat))) +
  geom_density() +
  # geom_histogram(alpha = .8, 
  #                position = "identity",
  #                bins = 50
  # ) +
  labs(
    # title = "",
    fill = "Destination district",
    x = "Difference between harvest and baseline in-migration rates",
    y = "Density"
  ) +
  theme_classic(base_size = 14) +
  scale_fill_manual(values = c("#FFA50080", "#fde72580", "#1f968b80", "#55c66780")) +
  theme(legend.position = c(1, .6),
        legend.justification = c("right", "bottom"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        plot.title = element_text(hjust = 0.5, face = 2),
        axis.text=element_text(size=14),
        axis.title = element_text(size=14)
  ) 
dev.off()

# 121 obs total 
table(tmp$cat)
#   Taliban = No, Violence = No  Taliban = No, Violence = Yes  Taliban = Yes, Violence = No 
#                            29                            29                            23 
# Taliban = Yes, Violence = Yes 
#                            40

                           
################ fig 3b
new3 <- lm(maxIn ~ poppyCat*violence*taliban + log(areakm2) + log(nonPoppy2) + TOTAL + as.factor(year) + as.factor(prov) +
             provincialCapital + cultivatedFrac + roadDensity + builtUpFracPop +
             trackPathFrac + healthPer100000 + diversity + as.factor(majority)+
             barrenAreaFrac
           , 
           data = outDTFM4 %>%
             filter(!is.na(maxIn) 
                    # & tmpPoppy < 12000
             )%>%
             rename(violence = violence_monthBefore) %>%
             mutate(taliban = ifelse(notGovt, 1, 0)))

# fit2coefs <- lmtest::coeftest(new3, vcov. = sandwich::vcovCL(new3, cluster = new3$model$`as.factor(prov)`, type = "HC1")) # takes less than a minute!

tmpDistID <- outDTFM4$distid[!is.na(outDTFM4$maxIn) & !is.na(outDTFM4$diversity) & !is.na(outDTFM4$trackPathFrac)]
fit2coefs <- lmtest::coeftest(new3, vcov. = sandwich::vcovCL(new3, cluster = as.factor(tmpDistID), type = "HC1")) # takes less than a minute!


keepNames <- c("poppyCatH", "violence", "poppyCatH:violence", "taliban", "poppyCatH:taliban", "poppyCatH:violence:taliban")
# keepNames <- c("poppyCatL", "violence", "poppyCatL:violence", "taliban", "poppyCatL:taliban", "poppyCatL:violence:taliban")
estimates <- fit2coefs[keepNames, "Estimate"] # keeps the order of names too 
# clusteredVcov <- sandwich::vcovCL(new3, cluster = new3$model$`as.factor(prov)`, type = "HC1")
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

vecs <- list(c(1, 0, 0, 0, 0, 0),
             c(1, 1, 1, 0, 0, 0),
             c(1, 0, 0, 1, 1, 0),
             c(1, 1, 1, 1, 1, 1))
for (i in 1:4) {
  tmp <- getCI(myVec = vecs[[i]], estimates = estimates, vCov = clusteredVcov)
  outDTF[i, "estimate"] <- tmp$estimate
  outDTF[i, "CIlow"] <- tmp$ciLow
  outDTF[i, "CIhigh"] <- tmp$ciHigh
  outDTF[i, "variance"] <- tmp$variance
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
    # title = "Violence definition: any violent event", 
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
             # , ylim = c(-.03, 0.09) # for low-growing
  ) +
  annotate(x = 2.5, xend = 2.5, y = -.063, yend = .075,
           geom = "segment",
           colour = "#a8a5a5", size = 1.4, alpha = .4) +
  # geom_vline(xintercept = 2.5, colour = "grey60", linetype = 2) +
  scale_x_discrete(labels = c("Violence = Yes", "Violence = No", "Violence = Yes", "Violence = No")) # bottom to top 
# scale_x_discrete(labels = c("Taliban = No", "Taliban = Yes", "Taliban = No", "Taliban = Yes")) # bottom to top 

pdf(paste0("/home/xtai/climate/3-8-23migrationCleanCode/output/general/10-24-23fig3b.pdf"), width = 8, height = 3)
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()
# rsync -P -e 'ssh -J xtai@hilbert.ucdavis.edu' xtai@fati.ischool.berkeley.edu:/home/xtai/climate/3-8-23migrationCleanCode/output/general/* /Users/xtai/Desktop/seasonalMigration/paper/general

### 10/15/23
#          model                 label    estimate        CIlow     CIhigh
# 1 High-growing Violence N, Taliban N 0.009800947 -0.006935413 0.02653731
# 2 High-growing Violence Y, Taliban N 0.008594161 -0.007304609 0.02449293
# 3 High-growing Violence N, Taliban Y 0.041531303  0.009666228 0.07339638
# 4 High-growing Violence Y, Taliban Y 0.040763568  0.020022173 0.06150496

outDTF$p <- 2*pnorm(abs(outDTF$estimate)/sqrt(outDTF$variance), lower.tail = FALSE)

### check significance of coefficients
diffDTF <- data.frame(model = rep("High-growing", 2), 
                     label = c("Violence N", "Violence Y"),
                     estimate = NA,
                     CIlow = NA,
                     CIhigh = NA,
                     variance = NA)

vecs <- list(c(0, 0, 0, 1, 1, 0), # violence N, diff between Taliban Y and N
             c(0, 0, 0, 1, 1, 1)) # violence Y, diff between Taliban Y and N

vecs <- list(c(0, 1, 1, 0, 0, 0), # Taliban N, diff between violence Y and N
             c(0, 1, 1, 0, 0, 1)) # Taliban Y, diff between violence Y and N
for (i in 1:2) {
  tmp <- getCI(myVec = vecs[[i]], estimates = estimates, vCov = clusteredVcov)
  diffDTF[i, "estimate"] <- tmp$estimate
  diffDTF[i, "CIlow"] <- tmp$ciLow
  diffDTF[i, "CIhigh"] <- tmp$ciHigh
  diffDTF[i, "variance"] <- tmp$variance
}

diffDTF
#          model      label   estimate         CIlow     CIhigh     variance
# 1 High-growing Violence N 0.03173036 -0.0008694806 0.06433019 0.0002766525
# 2 High-growing Violence Y 0.03216941  0.0098042272 0.05453459 0.0001302113

# p-value
2*pnorm(diffDTF$estimate[1]/sqrt(diffDTF$variance[1]), lower.tail = FALSE) # 0.0564314
2*pnorm(diffDTF$estimate[2]/sqrt(diffDTF$variance[2]), lower.tail = FALSE) # 0.004815045

################# 12/14/23: Figure S6
# different definitions of violence: more than two events --- need to change x-axis limits
plot4 <- outDTF %>%
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
  labs(
    # title = "Violence definition: more than 2 events",
    # title = "Violence definition: 10+ casualties",
    title = "Violence definition: > 2 events and 10+ casualties",
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
             # , ylim = c(-.02, 0.08) # CHANGE this line: plot2
             , ylim = c(-.02, 0.1) # CHANGE this line 
  ) +
  annotate(x = 2.5, xend = 2.5, y = -.063, yend = .1,
           geom = "segment",
           colour = "#a8a5a5", size = 1.4, alpha = .4) +
  # geom_vline(xintercept = 2.5, colour = "grey60", linetype = 2) +
  scale_x_discrete(labels = c("Violence = Yes", "Violence = No", "Violence = Yes", "Violence = No")) # bottom to top 
# scale_x_discrete(labels = c("Taliban = No", "Taliban = Yes", "Taliban = No", "Taliban = Yes")) # bottom to top 

# pdf(paste0("/home/xtai/climate/3-8-23migrationCleanCode/output/general/10-24-23fig3b.pdf"), width = 8, height = 3)
pdf(paste0("/home/xtai/climate/3-8-23migrationCleanCode/output/general/12-14-23fig3b_violenceDef.pdf"), width = 8, height = 3)
gridExtra::grid.arrange(plot1, nrow = 1)
gridExtra::grid.arrange(plot2, nrow = 1)
gridExtra::grid.arrange(plot3, nrow = 1)
gridExtra::grid.arrange(plot4, nrow = 1)
dev.off()
