library(tidyverse)
library(openxlsx)
library(gghighlight)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

# 記事：「北欧の出生率の急激な減少は社会経済的地位の低いグループがけん引か？」https://note.com/rmogimogi/n/n46dbd0507d1e

# データ読み込み
# data is from Human Fertility Database：https://www.humanfertility.org/Home/Index. Accessed on 25/05/2023
childless <- read.xlsx(".xlsx", sheet = "Cohort childlessness", startRow = 2)[-1, ]

# 図1
childless %>% 
  as.data.frame() %>% 
  rename(cohort = COUNTRY) %>% 
  gather(key = country, value = childless, -cohort) %>% 
  mutate(childless = as.numeric(as.character(childless)),
         childless = ifelse(childless == 0, NA_real_, childless),
         cohort = as.numeric(as.character(cohort))) %>% 
  filter(country %in% c("Denmark", "Finland", "Norway", "Sweden")) %>% 
  mutate(country = case_when(country == "Sweden" ~ "スウェーデン",
                             country == "Denmark" ~ "デンマーク",
                             country == "Finland" ~ "フィンランド",
                             country == "Norway" ~ "ノルウェー",
                             T ~ "日本")) %>% 
  ggplot(aes(x = cohort, y = childless)) +
  facet_wrap(~ country) +
  geom_line(aes(colour = country), size = 1.2) +
  gghighlight(use_direct_label = F) +
  xlim(1950, 1980) +
  labs(x = "出生年", y = "44歳以上の女性における無子割合",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("out/nordic-childless.png", width = 8, height = 6, bg = "white")
  