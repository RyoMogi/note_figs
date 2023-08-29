library(tidyverse)
library(openxlsx)
#library(gghighlight)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

# 記事：「少子化研究者による日本の少子化の現状解説①全体像」https://note.com/rmogimogi/n/ne0df89902771

# データ: Human Fertility Database: https://www.humanfertility.org/Home/Index. Accessed on 25/05/2023
ptfr <- read.xlsx(".xlsx", sheet = "Total fertility rates", startRow = 2)[-1, ]

# 図1
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

# データ：国立社会保障・人口問題研究所. Accessed on 25/05/2023
f64 <- read.csv(file("data/gaiyoFigure6_4_nchild_4549_jpn.csv", encoding = "cp932"), skip = 1)

# 図2
f64 %>% 
  as.data.frame() %>% 
  mutate(year = str_sub(`調査回`, -5, -2),
         year = as.numeric(as.character(year)),
         `割合` = as.numeric(as.character(`割合`))) %>% 
  filter(`出生子ども数` == "０人") %>% View()
ggplot(aes(x = year, y = `割合`)) +
  geom_line(size = 1.2, colour = Mycol[3]) +
  ylim(0, 10) +
  labs(x = "年次", y = "妻45～49歳夫婦における無子割合",
       caption = "データソース：国立社会保障人口問題研究所『第16回出生動向基本調査結果概要』. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.position = "top",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("out/jpn-couplechildless.png", width = 7.5, height = 5, bg = "white")

# データ：Human Fertility Database: https://www.humanfertility.org/Home/Index. Accessed on 25/05/2023
childless <- read.xlsx(".xlsx", sheet = "Cohort childlessness", startRow = 2)[-1, ]

# 図3
childless %>% 
  as.data.frame() %>% 
  rename(cohort = COUNTRY) %>% 
  gather(key = country, value = childless, -cohort) %>% 
  mutate(childless = as.numeric(as.character(childless)),
         childless = ifelse(childless == 0, NA_real_, childless),
         cohort = as.numeric(as.character(cohort))) %>% 
  filter(country == "Japan") %>% 
  mutate(country = ifelse(country == "Japan", "日本", country)) %>% 
  ggplot(aes(x = cohort, y = childless)) +
  geom_line(size = 1.2, colour = Mycol[3]) +
  xlim(1950, 1980) +
  ylim(0, 30) +
  labs(x = "出生年", y = "44歳以上の女性における無子割合",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("out/jpn-childless.png", width = 7.5, height = 5, bg = "white")

# データ：国立社会保障・人口問題研究所. Accessed on 25/05/2023
f61 <- read.csv(file("data/gaiyoFigure6_1_nchild1519_jpn.csv", encoding = "cp932"), skip = 1)

# 図4
f61 %>% 
  as.data.frame() %>% 
  mutate(year = str_sub(`調査回`, -5, -2),
         year = as.numeric(as.character(year)),
         `平均出生子ども数` = as.numeric(as.character(`平均出生子ども数`))) %>% 
  ggplot(aes(x = year, y = `平均出生子ども数`)) +
  geom_line(size = 1.2, colour = Mycol[3]) +
  ylim(0, 2.5) +
  labs(x = "年次", y = "夫婦の完結出生子ども数（結婚持続期間 15～19 年）",
       caption = "データソース：国立社会保障人口問題研究所『第16回出生動向基本調査結果概要』. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("out/jpn-nchild-1519dur.png", width = 7.5, height = 5.5, bg = "white")

# データ：国立社会保障・人口問題研究所. Accessed on 25/05/2023
f65 <- read.csv(file("data/gaiyoFigure6_5_nchild_durmar_jpn.csv", encoding = "cp932"), skip = 1)

# 図5
f65 %>% 
  as.data.frame() %>% 
  mutate(year = str_sub(`調査回`, -5, -2),
         year = as.numeric(as.character(year)),
         `平均出生子ども数` = as.numeric(as.character(`平均出生子ども数`)),
         `結婚持続期間` = factor(`結婚持続期間`, levels = c("0～4年", "5～9年", "10～14年", "15～19年"))) %>% 
  ggplot(aes(x = year, y = `平均出生子ども数`, colour = `結婚持続期間`)) +
  geom_line(size = 1.2) +
  scale_colour_manual(values = Mycol) +
  ylim(0, 2.5) +
  labs(x = "年次", y = "夫婦の完結出生子ども数",
       caption = "データソース：国立社会保障人口問題研究所『第16回出生動向基本調査結果概要』. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("out/jpn-nchild-mardur.png", width = 7.5, height = 5, bg = "white")