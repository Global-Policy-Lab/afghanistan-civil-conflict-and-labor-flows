library(dplyr); library(ggplot2)
rm(list = ls()); gc()

load("./demo/data/figS4.Rdata")

plot1 <- data.frame(randomCoefs = collectCoefs) %>%
  ggplot(aes(x = randomCoefs)) +
  geom_histogram() +
  geom_vline(xintercept = originalCoef) +
  labs(x = "Estimated coefficient for high-cultivation districts",
       y = "Count") +
  scale_x_continuous(#labels = ,
    breaks = seq(from = -.01, to = .03, by = .005)) +
  annotate("text", x = .0248, y = 15.3, label = "Observed",
           size = 3) 

gridExtra::grid.arrange(plot1, nrow = 1)





