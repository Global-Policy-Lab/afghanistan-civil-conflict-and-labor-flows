rm(list = ls()); gc()

# for file ./demo/data/figS10.Rdata
# see code 3i_figS10.R
# save outDTF for each year dropped as outDTF_2014 to outDTF_2020
# figS10.Rdata has outDTF_2014-2016, 2018-2020.

load("./demo/data/figS10.Rdata")
# replace outDTF_2014 with relevant year  
plot1 <- outDTF_2014 %>%
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
  labs(
    # title = paste0("Removing year ", tmpYear),
    # title = "Post-2017",
    # title = "Pre-2017",
    # y = "Increase in-migrant proportion from source districts\nwith specified characteristics (harvest compared to baseline)",
    y = "Increase in in-migration, by type of source district",
    x = ""
  ) +
  labs(tag = "Taliban = No \n\n\n\n\n\n\n\n\n Taliban = Yes") +
  # labs(tag = "Violence = Yes \n\n\n\n\n\n\n\n\n Violence = No") +
  theme(plot.tag.position = c(-.03, .53),
        text = element_text(size = 12),
        plot.margin = unit(c(0, 0, 0, 3), "cm")
  ) +
  coord_flip(clip = "off", 
             # ylim = c(min(0, min(outDTF$CIlow)), max(outDTF$CIhigh))
             , ylim = c(-.012, 0.042)
  ) +
  annotate(x = 2.5, xend = 2.5, 
           # y = -.023, yend = .032,
           y = -.038, yend = .042,
           # y = -.023, yend = max(outDTF$CIhigh),
           geom = "segment",
           colour = "#a8a5a5", size = 1.4, alpha = .4) +
  # geom_vline(xintercept = 2.5, colour = "grey60", linetype = 2) +
  # scale_x_discrete(labels = c("Taliban = No", "Taliban = Yes", "Taliban = No", "Taliban = Yes")) # bottom to top 
  scale_x_discrete(labels = c("Violence = Yes", "Violence = No", "Violence = Yes", "Violence = No")) # bottom to top 

gridExtra::grid.arrange(plot1, nrow = 1)


knitr::kable(outDTF_2014 %>%
               select(label, estimate, variance, p, n) %>%
               mutate(SD = as.numeric(sqrt(variance)),
                      p = as.numeric(p)) %>%
               rename(Coefficient = label,
                      Estimate = estimate) %>%
               select(Coefficient, Estimate, SD, p, n),
             format = "latex",
             booktabs = TRUE,  # nicer lines
             digits = c(NA, 4, 4, 3, 1))
