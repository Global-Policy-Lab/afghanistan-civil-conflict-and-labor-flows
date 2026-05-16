rm(list = ls()); gc()

load("./demo/data/figS5.Rdata")

plot1 <- ggplot(outDTFM4, aes(x = notGovt, fill = as.factor(violence_monthBefore))) +
  geom_bar() +
  labs(fill = "Violence",
       x = "Taliban presence",
       y = "Count") #+
  viridis::scale_fill_viridis(discrete = TRUE)

plot2 <- ggplot(outDTFM4, aes(x = notGovt, fill = as.factor(violence_monthBefore))) +
  geom_bar(position = "fill") +
  labs(fill = "Violence",
       x = "Taliban presence",
       y = "Proportion") +
  viridis::scale_fill_viridis(discrete = TRUE)

gridExtra::grid.arrange(plot1, nrow = 1, top = "Districts with Taliban presence are more likely to have violence")

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

