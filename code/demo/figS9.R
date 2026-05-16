######### fig 3a
rm(list = ls()); gc()
library(dplyr); library(ggplot2)
 

load("./demo/data/figS9.Rdata")
# this will load outDTFv1, outDTFv2, outDTFv3 for the three panels. Replace outDTFv1 in lines 9 and 48 for the second and third panel. 

plot4 <- outDTFv1 %>%
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
    # title = tmpTitle,
    y = "Increase in in-migration from baseline during harvest",
    x = ""
  ) +
  labs(tag = "Taliban = No \n\n\n\n\n Taliban = Yes") +
  # labs(tag = "Violence = Yes \n\n\n\n\n Violence = No") +
  theme(plot.tag.position = c(-.07, .57),
        text = element_text(size = 12),
        plot.margin = unit(c(0, 0, 0, 3), "cm")
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

gridExtra::grid.arrange(plot4, nrow = 1)

knitr::kable(outDTFv1 %>%
               select(label, estimate, variance, p, n) %>%
               mutate(SD = as.numeric(sqrt(variance)),
                      p = as.numeric(p)) %>%
               rename(Coefficient = label,
                      Estimate = estimate) %>%
               select(Coefficient, Estimate, SD, p, n),
             format = "latex",
             booktabs = TRUE,  # nicer lines
             digits = c(NA, 4, 4, 3, 1))
