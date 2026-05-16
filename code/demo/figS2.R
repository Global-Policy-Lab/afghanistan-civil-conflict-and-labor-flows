rm(list = ls()); gc()
load("./demo/data/figS2.Rdata")

afghanShape %>%
  # bind_cols(tmp) %>%
  # mutate(X = ifelse(DISTID %in% topDistIDs, X, NA)) %>%
  ggplot() + 
  # theme_void() +
  geom_sf() #+

plotDTF %>%
  mutate(distid = ifelse(distid == 1115, 0, distid)) %>% # hack so that 1115 appears below
  mutate(val = ifelse(is.na(year), 0, 1),
         distid = as.factor(distid)) %>%
  ggplot(aes(date, distid, fill = as.factor(val))) + 
  geom_tile() +
  scale_x_date(labels=scales::date_format(format = "%m/%Y"), breaks = scales::date_breaks(width = "2 months"), expand = c(.02, .02)) +
  # theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, size = 8),
        legend.position = "none") +
  scale_fill_manual(values = c("white", "black")) +
  geom_vline(xintercept = seq(as.Date("2015-01-01"), as.Date("2020-01-01"), "1 year")) +
  labs(x = "", y = "") +
scale_y_discrete(labels = c("2416" = "Zhari",
                            "2407" = "Maywand",
                            "2307" = "Washer",
                            "2302" = "Nahri Sarraj",
                            "2301" = "Lashkar Gah",
                            "2205" = "Khash Rod",
                            "1906" = "Bala Murghab",
                            "807" = "Pachier Agam",
                            "805" = "Khogayani",
                            "0" = "Argo" # hack: this is 1115
))



