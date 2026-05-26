# FIGURE 1c
rm(list = ls()); gc()
library(ggplot2); library(gridExtra); library(dplyr)

# for file ./demo/data/fig1c.RData:
# from code 2a_fig1.R
# save(tmp, relevantRows, conflictDist, file = "./demo/data/fig1c.RData")

load("./demo/data/fig1c.RData")

plot1 <- tmp %>%
  ggplot2::ggplot() +
  geom_line(aes(x = date, y = percentage_migrated, col = "Percentage out")) +
  geom_line(aes(x = date, y = percentage_in, col = "Percentage in")) + 
  labs(
  x = "",
  y = "",
  col = "") +
  scale_x_date(labels=scales::date_format(format = "%Y-%m-%d"), 
               breaks = scales::date_breaks(width = "1 month"), 
               expand = c(.02, .02),
               # limits = c(as.Date("2014-01-01"), as.Date("2017-01-01"))) +
               limits = c(as.Date("2014-01-01"), as.Date("2016-01-01"))) +
  ylim(-.03, .25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right") +
  geom_vline(xintercept = as.Date(relevantRows$date), color = "black", lwd = .1)  +
  geom_point(data = conflictDist %>%
               rename(Casualties = best), 
             aes(x = as.Date(date_start), y = -.01, size = Casualties)) 

pdf("./demo/output/fig1c.pdf", width = 10, height = 4)
gridExtra::grid.arrange(plot1, nrow = 1)
dev.off()
