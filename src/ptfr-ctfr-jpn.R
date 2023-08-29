library(tidyverse)
library(openxlsx)
#library(gghighlight)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")


# read data on period total fertility rate
# data is from Human Fertility Database. Accessed on 25/05/2023
ptfr <- read.xlsx("data/TFR_nordic.xlsx", sheet = "Total fertility rates", startRow = 2)[-1, ]

# clean data and make a figure
ptfr %>% 
  as.data.frame() %>% 
  rename(year = COUNTRY) %>% 
  gather(key = country, value = tfr, -year) %>% 
  mutate(tfr = as.numeric(as.character(tfr)),
         tfr = ifelse(tfr == 0, NA, tfr),
         year = as.numeric(as.character(year))) %>% 
  filter(country == "Japan") %>% 
  # add the newly updated TFR (2022) from the vital statistics:
  # https://www.mhlw.go.jp/toukei/saikin/hw/jinkou/geppo/nengai22/dl/kekka.pdf
  mutate(tfr = ifelse(year == 2022, 1.26, tfr)) %>% 
  ggplot(aes(x = year, y = tfr)) +
  geom_hline(yintercept = 2.08, linetype = "dashed") +
  geom_line(size = 1.2, colour = Mycol[3]) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database and Japanese vital statistics. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("out/jpn-ptfr.png", width = 7.5, height = 5, bg = "white")


# read data on cohort total fertility rate
# data is from Human Fertility Database. Accessed on 25/05/2023
ctfr <- read.xlsx("../data/CCF.xlsx", sheet = "Completed cohort fertility", startRow = 2)[-1, ]

# clean data and make a figure
ctfr %>% 
  as.data.frame() %>% 
  rename(cohort = COUNTRY) %>% 
  gather(key = country, value = tfr, -cohort) %>% 
  mutate(tfr = as.numeric(as.character(tfr)),
         tfr = ifelse(tfr == 0, NA, tfr),
         cohort = as.numeric(as.character(cohort))) %>% 
  filter(country == "Japan") %>% 
  ggplot(aes(x = cohort, y = tfr)) +
  geom_hline(yintercept = 2.08, linetype = "dashed") +
  geom_line(size = 1.2, colour = Mycol[2]) +
  xlim(1935, 1980) +
  labs(x = "出生年", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("out/jpn-ctfr.png", width = 7.5, height = 5, bg = "white")

# read data on tempo-adjusted period total fertility rate
# data is from Human Fertility Database. Accessed on 27/07/2023
adjtfr <- read.xlsx("../data/adjTFR.xlsx", sheet = "Tempo-adjusted TFR", startRow = 2)[-1, ]

# clean data and make a figure
ptfr_d <- ptfr %>% 
  as.data.frame() %>% 
  rename(year = COUNTRY) %>% 
  gather(key = country, value = tfr, -year) %>% 
  mutate(tfr = as.numeric(as.character(tfr)),
         tfr = ifelse(tfr == 0, NA, tfr),
         year = as.numeric(as.character(year))) %>% 
  filter(country == "Japan") %>% 
  # add the newly updated TFR (2022) from the vital statistics:
  # https://www.mhlw.go.jp/toukei/saikin/hw/jinkou/geppo/nengai22/dl/kekka.pdf
  mutate(tfr = ifelse(year == 2022, 1.26, tfr))

adjtfr %>% 
  as.data.frame() %>% 
  rename(year = COUNTRY) %>% 
  gather(key = country, value = adjtfr, -year) %>% 
  mutate(adjtfr = as.numeric(as.character(adjtfr)),
         adjtfr = ifelse(adjtfr == 0, NA, adjtfr),
         year = as.numeric(as.character(year))) %>% 
  filter(country == "Japan") %>% 
  left_join(ptfr_d, by = c("year", "country")) %>% 
  gather(key = index, value = tfr, -c(year, country)) %>% 
  mutate(index = ifelse(index == "tfr", "ピリオドTFR", "テンポ効果を調整したピリオドTFR")) %>% 
  ggplot(aes(x = year, y = tfr, colour = index, group = index)) +
  geom_hline(yintercept = 2.08, linetype = "dashed") +
  geom_line(size = 1.2) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database and Japanese vital statistics. 作成者：茂木良平") +
  scale_colour_manual(values = c(Mycol[1], Mycol[3])) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.position = "top",
        legend.title = element_blank())
ggsave("out/jpn-ptfr-adjptfr.png", width = 7.5, height = 5, bg = "white")
