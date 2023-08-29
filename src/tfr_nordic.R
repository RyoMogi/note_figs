library(tidyverse)
library(openxlsx)
library(readxl)
library(gghighlight)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

# 記事：「北欧5か国の出生率が大幅に減少しています」https://note.com/rmogimogi/n/ncc819af9d93c

# データ読み込み
# Human Fertility Database: https://www.humanfertility.org/Home/Index
# 2022 data is from Finland Statistics Office, Nordic Statistics Office
tfr <- read.xlsx(".xlsx", sheet = "Total fertility rates", startRow = 2)[-1, ]

# データクリーン
tfr <- tfr %>% 
  as.data.frame() %>% 
  rename(year = COUNTRY) %>% 
  gather(key = country, value = tfr, -year) %>% 
  mutate(tfr = as.numeric(as.character(tfr)),
         tfr = ifelse(tfr == 0, NA, tfr),
         year = as.numeric(as.character(year)))

# 図1
tfr %>% 
  filter(country %in% c("Sweden", "Denmark", "Finland", "Norway", "Japan")) %>%
  mutate(country = case_when(country == "Sweden" ~ "スウェーデン",
                             country == "Denmark" ~ "デンマーク",
                             country == "Finland" ~ "フィンランド",
                             country == "Norway" ~ "ノルウェー",
                             T ~ "日本")) %>% 
  ggplot(aes(x = year, y = tfr)) +
  facet_wrap(~ country) +
  geom_hline(yintercept = 2.08, linetype = "dashed") +
  geom_line(aes(colour = country), size = 1.2) +
  gghighlight(use_direct_label = F) +
  labs(x = "年次", y = "合計特殊出生率") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("out/nordic-tfr.png", width = 8, height = 5, bg = "white")

# 2010年と2022年の合計特殊出生率の違いをチェック
tfr %>% 
  filter(country %in% c("Sweden", "Denmark", "Finland", "Norway"),
         year %in% c(2010, 2015, 2020, 2022)) %>% 
  spread(key = year, value = tfr) %>% 
  mutate(diff_1022 = round(100 - (`2022` / `2010`) * 100, 1))
