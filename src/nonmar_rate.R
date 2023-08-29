library(tidyverse)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

nonmar_ESP <- read.csv("data/Spain_nonmaritalrate_Eurostat.csv")
tfr_ESP <- read.csv("data/Spain_TFR_Eurostat.csv")

nonmar_ESP <- nonmar_ESP %>% 
  select(Year = TIME_PERIOD, indic_de, value = OBS_VALUE) %>% 
  spread(key = indic_de, value = value) %>% 
  mutate(nonmar_rate = round(NMAR / LBIRTH * 100, 1)) %>% 
  select(-c(LBIRTH, NMAR))

tfr_ESP <- tfr_ESP %>% 
  select(Year = TIME_PERIOD, tfr = OBS_VALUE) %>% 
  left_join(nonmar_ESP, by = "Year")

tfr_ESP %>% 
  ggplot() +
  geom_line(aes(x = Year, y = tfr / 0.03), col = Mycol[3], size = 2) +
  geom_line(aes(x = Year, y = nonmar_rate), col = Mycol[2], size = 2) +
  scale_y_continuous(name = "Proportion of nonmarital births (%)",
                     sec.axis = sec_axis(~.*0.03, name = "Total fertility rate")) +
  theme_minimal() +
  theme(axis.title.y.left = element_text(colour = Mycol[2]),
        axis.title.y.right = element_text(color = Mycol[3]))
ggsave("out/TFR_NMC_ESP.png", width = 6, height = 4, bg = "white")
