library(tidyverse)
library(stringi)
`%out%` = Negate(`%in%`)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

# data is accessed on Aug 25th 2023
# estat: 人口動態統計2021年。中巻5。婚姻件数（当該年に結婚生活に入り届け出たもの），夫妻の初婚－再婚の組合せ・妻の結婚生活に入ったときの年齢（各歳）・夫の結婚生活に入ったときの年齢（各歳）別
# https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00450011&tstat=000001028897&cycle=7&tclass1=000001053058&tclass2=000001053061&tclass3=000001053069&tclass4val=0
marN2021 <- read.csv(file("data/age-specific-marriageN_2021.csv", encoding = "cp932"), skip = 11)
# estat: 人口動態統計2020年。中巻5。
marN2020 <- read.csv(file("data/age-specific-marriageN_2020.csv", encoding = "cp932"), skip = 4)
# estat: 人口動態統計2019年。中巻5。
marN2019 <- read.csv(file("data/age-specific-marriageN_2019.csv", encoding = "cp932"), skip = 4)
# estat: 人口動態統計2010年
marN2010 <- read.csv(file("data/age-specific-marriageN_2010.csv", encoding = "cp932"), skip = 5)
# estat: 人口動態統計2005年。中巻4。
marN2005 <- read.csv(file("data/age-specific-marriageN_2005.csv", encoding = "cp932"), skip = 4)
# estat: 人口動態統計2000年。中巻4。
marN2000 <- read.csv(file("data/age-specific-marriageN_2000.csv", encoding = "cp932"), skip = 4)
# estat: 人口動態統計1995年。中巻4。
marN1995 <- read.csv(file("data/age-specific-marriageN_1995.csv", encoding = "cp932"), skip = 5)

# husband
hus_marN2021 <- marN2021 %>% 
  as.data.frame() %>% 
  filter(`妻の年齢.1歳階級.` == "妻_総数",
         `夫の初婚.再婚` == "夫_初婚",
         `妻の初婚.再婚` == "妻_初婚") %>% 
  select(hus_age = `夫の年齢.1歳階級.`, y2021 = `X2021年`, y2020 = `X2020年`, 
         y2019 = `X2019年`, y2018 = `X2018年`, y2017 = `X2017年`, 
         y2016 = `X2016年`, y2015 = `X2015年`) %>% 
  mutate(hus_age = str_sub(hus_age, 3, 4)) %>% 
  filter(hus_age %out% c("総数", "不詳")) %>% 
  mutate(y2021 = as.numeric(gsub(",", "", y2021)),
         y2020 = as.numeric(gsub(",", "", y2020)),
         y2020 = ifelse(is.na(y2015), 0, y2020),
         y2019 = as.numeric(gsub(",", "", y2019)),
         y2019 = ifelse(is.na(y2015), 0, y2019),
         y2018 = as.numeric(gsub(",", "", y2018)),
         y2018 = ifelse(is.na(y2015), 0, y2018),
         y2017 = as.numeric(gsub(",", "", y2017)),
         y2017 = ifelse(is.na(y2015), 0, y2017),
         y2016 = as.numeric(gsub(",", "", y2016)),
         y2016 = ifelse(is.na(y2015), 0, y2016),
         y2015 = as.numeric(gsub(",", "", y2015)),
         y2015 = ifelse(is.na(y2015), 0, y2015),
         hus_age = as.numeric(as.character(hus_age))) %>% 
  gather(key = year, value = marN, -hus_age) %>% 
  mutate(year = str_sub(year, 2, 5),
         year = as.numeric(as.character(year)))

dmarN2020 <- marN2020[70, ] %>% 
  mutate(year = 2020) %>% 
  gather(key = hus_age, value = marN, -year) %>% 
  filter(hus_age %out% c("X", "夫総数", "不.詳"))

dmarN2019 <- marN2019[70, ] %>% 
  mutate(year = 2019) %>% 
  gather(key = hus_age, value = marN, -year) %>% 
  filter(hus_age %out% c("X", "夫総数", "不.詳"))

dmarN2010 <- marN2010[70, ] %>% 
  mutate(year = 2010) %>% 
  gather(key = hus_age, value = marN, -year) %>% 
  filter(hus_age %out% c("X", "夫総数", "不.詳"))

dmarN2005 <- marN2005[65, ] %>% 
  mutate(year = 2005) %>% 
  gather(key = hus_age, value = marN, -year) %>% 
  filter(hus_age %out% c("X", "夫総数", "不.詳"))

dmarN2000 <- marN2000[66, ] %>% 
  mutate(year = 2000) %>% 
  gather(key = hus_age, value = marN, -year) %>% 
  filter(hus_age %out% c("X", "夫総数", "不.詳"))

dmarN1995 <- marN1995[65, ] %>% 
  mutate(year = 1995) %>% 
  gather(key = hus_age, value = marN, -year) %>% 
  filter(hus_age %out% c("X", "夫総数", "不.詳"))

hus_marN <- dmarN2010 %>% 
  bind_rows(dmarN2020) %>% 
  bind_rows(dmarN2019) %>% 
  bind_rows(dmarN2005) %>% 
  bind_rows(dmarN2000) %>% 
  bind_rows(dmarN1995) %>% 
  mutate(hus_age = str_sub(hus_age, 1, 2),
         hus_age = stri_trans_general(hus_age, "Fullwidth-Halfwidth"),
         marN = as.numeric(as.character(marN)),
         marN = ifelse(is.na(marN), 0, marN)) %>% 
  filter(hus_age %out% c("総.", "不.")) %>% 
  mutate(hus_age = as.numeric(hus_age)) %>% 
  bind_rows(hus_marN2021)

hus_marN %>% 
  filter(hus_age <= 45,
         year <= 2015) %>% 
  group_by(year) %>% 
  mutate(marN_all = sum(marN),
         marN_cum = cumsum(marN),
         prop_marN = marN_cum / marN_all * 100) %>% 
  ungroup() %>% 
  mutate(year = factor(year)) %>% View()
  ggplot(aes(x = hus_age, y = prop_marN, group = year, colour = year)) +
  geom_line(size = 1.3) +
  scale_colour_manual(values = c(Mycol)) +
  labs(x = "夫の年齢", y = "婚姻割合",
       caption = "データソース：人口動態統計. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box = "vertical",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("out/jpn-marprop-2015.png", width = 7.5, height = 5, bg = "white")

hus_marN2021 %>% 
  filter(hus_age <= 45,
         year %in% c("2015", "2017", "2019", "2020", "2021")) %>% 
  group_by(year) %>% 
  mutate(marN_all = sum(marN),
         marN_cum = cumsum(marN),
         prop_marN = marN_cum / marN_all * 100) %>% 
  ungroup() %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(x = hus_age, y = prop_marN, group = year, colour = year)) +
  geom_line(size = 1.3) +
  scale_colour_manual(values = Mycol) +
  labs(x = "夫の年齢", y = "婚姻割合",
       caption = "データソース：人口動態統計. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box = "vertical",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("out/jpn-marprop-1521.png", width = 7.5, height = 5, bg = "white")
