rm(list = ls()); gc()

load("./demo/data/figS6.Rdata")

outDTFM4 %>%
  # filter(tmpPoppy < 5000) %>%
  ggplot(aes(tmpPoppy, maxIn)) +
  geom_point() +
  geom_smooth(method = loess) +
  labs(
    x = "Number of hectares of opium poppy cultivation",
    y = "Difference between harvest and \nbaseline in-migration rates"
  )+
  theme(#plot.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14),
  )

outDTFM4 %>%
  # filter(tmpPoppy < 5000) %>%
  ggplot(aes(tmpPoppy, violence_monthBefore)) +
  geom_point() +
  geom_smooth(method = loess) +
  labs(
    x = "Number of hectares of opium poppy cultivation",
    y = "Violent events in the month before peak NDVI"
  ) +
  theme(#plot.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14),
  )

outDTFM4 %>%
  mutate(taliban = notGovt) %>%
  # mutate(taliban = ifelse(scale4 == TRUE | scale5 == TRUE, 1, 0)) %>%
  # filter(tmpPoppy < 5000) %>%
  ggplot(aes(tmpPoppy, taliban)) +
  geom_point() +
  geom_smooth(method = loess) +
  labs(
    x = "Number of hectares of opium poppy cultivation",
    y = "Taliban presence"
  )+
  theme(#plot.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14),
  )


