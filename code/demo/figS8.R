rm(list = ls()); gc()

load("./demo/data/figS8.Rdata")

plot1 <- outDTFpre %>%
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
    title = "Pre-2017", # pre
    # title = "Post-2017", # post
    y = "Increase in in-migration from baseline during harvest",
    x = ""
  ) +
  labs(tag = "Taliban = No \n\n\n\n\n\n\n\n\n Taliban = Yes") +
  # labs(tag = "Violence = Yes \n\n\n\n\n\n\n\n\n Violence = No") +
  theme(plot.tag.position = c(-.025, .53),
        text = element_text(size = 12),
        plot.margin =  unit(c(0, 0, 0, 3), "cm")
  ) +
  coord_flip(clip = "off"
             , ylim = c(-.015, 0.06) # for pre
             # , ylim = c(-.015, 0.16) # post
  ) +
  annotate(x = 2.5, xend = 2.5, 
           y = -.05, yend = .06, # pre
           # y = -.1, yend = .16, # post
           geom = "segment",
           colour = "#a8a5a5", size = 1.4, alpha = .4) +
  # geom_vline(xintercept = 2.5, colour = "grey60", linetype = 2) +
  # scale_x_discrete(labels = c("Taliban = No", "Taliban = Yes", "Taliban = No", "Taliban = Yes")) # bottom to top 
  scale_x_discrete(labels = c("Violence = Yes", "Violence = No", "Violence = Yes", "Violence = No")) # bottom to top 

gridExtra::grid.arrange(plot1, nrow = 1)


plot1 <- outDTFpost %>%
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
    # title = "Pre-2017", # pre
    title = "Post-2017", # post
    y = "Increase in in-migration from baseline during harvest",
    x = ""
  ) +
  labs(tag = "Taliban = No \n\n\n\n\n\n\n\n\n Taliban = Yes") +
  # labs(tag = "Violence = Yes \n\n\n\n\n\n\n\n\n Violence = No") +
  theme(plot.tag.position = c(-.025, .53),
        text = element_text(size = 12),
        plot.margin =  unit(c(0, 0, 0, 3), "cm")
  ) +
  coord_flip(clip = "off"
             # , ylim = c(-.015, 0.06) # for pre
             , ylim = c(-.015, 0.16) # post
  ) +
  annotate(x = 2.5, xend = 2.5, 
           # y = -.05, yend = .06, # pre
           y = -.1, yend = .16, # post
           geom = "segment",
           colour = "#a8a5a5", size = 1.4, alpha = .4) +
  # geom_vline(xintercept = 2.5, colour = "grey60", linetype = 2) +
  # scale_x_discrete(labels = c("Taliban = No", "Taliban = Yes", "Taliban = No", "Taliban = Yes")) # bottom to top 
  scale_x_discrete(labels = c("Violence = Yes", "Violence = No", "Violence = Yes", "Violence = No")) # bottom to top 

gridExtra::grid.arrange(plot1, nrow = 1)

#### remove individual years: replace outDTF_2014 with other years
plot1 <- outDTF_2014 %>%
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
    y = "Increase in in-migration from baseline during harvest",
    x = ""
  ) +
  labs(tag = "Taliban = No \n\n\n\n\n\n\n\n\n Taliban = Yes") +
  # labs(tag = "Violence = Yes \n\n\n\n\n\n\n\n\n Violence = No") +
  theme(plot.tag.position = c(-.03, .53),
        text = element_text(size = 12),
        plot.margin = unit(c(0, 0, 0, 3), "cm")
  ) +
  coord_flip(clip = "off"
             , ylim = c(-.015, 0.08) ### ADJUST THIS: this for yearly
  ) +
  annotate(x = 2.5, xend = 2.5, 
           y = -.06, yend = .08, ### ADJUST THIS: this line for yearly
           geom = "segment",
           colour = "#a8a5a5", size = 1.4, alpha = .4) +
  # geom_vline(xintercept = 2.5, colour = "grey60", linetype = 2) +
  # scale_x_discrete(labels = c("Taliban = No", "Taliban = Yes", "Taliban = No", "Taliban = Yes")) # bottom to top 
  scale_x_discrete(labels = c("Violence = Yes", "Violence = No", "Violence = Yes", "Violence = No")) # bottom to top 

gridExtra::grid.arrange(plot1, nrow = 1)


# replace outDTF below with relevant dataframe
knitr::kable(outDTF %>%
               select(label, estimate, variance, p, n) %>%
               mutate(SD = as.numeric(sqrt(variance)),
                      p = as.numeric(p)) %>%
               rename(Coefficient = label,
                      Estimate = estimate) %>%
               select(Coefficient, Estimate, SD, p, n), 
             format = "latex", 
             booktabs = TRUE,  # nicer lines
             digits = c(NA, 4, 4, 3, 1))

