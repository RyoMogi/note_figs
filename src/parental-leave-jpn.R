library(tidyverse)
library(ggrepel)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

# koyou-kintou-kikai
koyou_rate <- data.frame(
  year = c(1996, 1999, 2002, 2004, 2005, 2007:2021),
  f_rate = c(49.1, 56.4, 64, 70.6, 72.3, 89.7, 90.6, 85.6, 83.7, 87.8, 83.6, 83, 
             86.6, 81.5, 81.8, 83.2, 82.2, 83, 81.6, 85.1),
  m_rate = c(0.12, 0.42, 0.33, 0.56, 0.5, 1.56, 1.23, 1.72, 1.38, 2.63, 1.89, 2.03, 
             2.3, 2.65, 3.16, 5.14, 6.16, 7.48, 12.65, 13.97)
)

koyou_dur <- data.frame(
  dur = c("-4日", "5-13日", "14-30日", "1-2カ月", "3-5カ月", "6-7カ月",
          "8-9カ月", "10-11カ月", "12-17カ月", "18-23カ月", "24-35カ月", "36カ月-"),
  f2015 = c(0.8, 0.3, 0.6, 2.2, 7.8, 10.2, 12.7, 31.1, 27.6, 4, 2, 0.6),
  f2018 = c(0.5, 0.3, 0.1, 2.8, 7, 8.8, 10.9, 31.3, 29.8, 4.8, 3.3, 0.5),
  f2021 = c(0.5, 0, 0.1, 0.8, 3.5, 6.4, 8.7, 30, 34, 11.1, 4.5, 0.6),
  m2015 = c(56.9, 17.8, 8.4, 12.1, 1.6, 0.2, 0.7, 0.1, 2, 0, 0, 0),
  m2018 = c(36.3, 35.1, 9.6, 11.9, 3, 0.9, 0.4, 0.9, 1.7, 0, 0.1, 0),
  m2021 = c(25, 26.5, 13.2, 24.5, 5.1, 1.9, 1.1, 1.4, 0.9, 0, 0.2, 0)
)

koyou_rate %>% 
  gather(key = sex, value = rate, -year) %>% 
  mutate(sex = ifelse(sex == "f_rate", "女性", "男性")) %>% 
  ggplot(aes(x = year, y = rate, group = sex, colour = sex, label = rate)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text_repel(nudge_y = 5, show_guide = F) +
  ylim(0, 100) +
  scale_colour_manual(values = c(Mycol[3], Mycol[2])) +
  labs(x = "年次", y = "育児休業取得割合",
       caption = "データソース：厚生労働省「雇用均等基本調査」. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box = "vertical",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.spacing = unit(2, "lines"))
ggsave("out/jpn-parentleave_koyou.png", width = 7.5, height = 5, bg = "white")

koyou_dur <- koyou_dur %>% 
  gather(key = var, value = prop, -dur) %>%
  mutate(sex = str_sub(var, 1, 1),
         sex = ifelse(sex == "f", "女性", "男性"),
         year = str_sub(var, -4, -1),
         year = factor(year, levels = c("2021", "2018", "2015"))) %>%
  select(-var)

koyou_dur %>% 
  filter(sex == "女性") %>% 
  mutate(dur_cate = case_when(dur %in% c("-4日", "5-13日", "14-30日", "1-2カ月", "3-5カ月") ~ "-5カ月",
                              dur %in% c("18-23カ月", "24-35カ月", "36カ月-") ~ "18カ月-",
                              T ~ dur),
         dur_cate = factor(dur_cate, levels = c("-5カ月", "6-7カ月", "8-9カ月", "10-11カ月", 
                                                "12-17カ月", "18カ月-"))) %>% 
  group_by(sex, year, dur_cate) %>% 
  summarise(prop = sum(prop)) %>% 
  ggplot(aes(x = year, y = prop, fill = dur_cate, label = prop)) +
  geom_bar(position = position_fill(reverse = T), stat = "identity") +
  geom_text(size = 4, position = position_fill(reverse = T, vjust = 0.5)) +
  coord_flip() +
  labs(x = "年次", y = "女性の育休取得期間の割合",
       caption = "データソース：厚生労働省「雇用均等基本調査」. 作成者：茂木良平") +
  scale_fill_manual(values = c("grey", Mycol)) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box = "vertical",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.spacing = unit(2, "lines"))
ggsave("out/jpn-parentleave_koyou_durf.png", width = 7.5, height = 5, bg = "white")

koyou_dur %>% 
  filter(sex == "男性") %>% 
  mutate(dur_cate = ifelse(dur %in% c("6-7カ月", "8-9カ月", "10-11カ月", 
                                      "12-17カ月", "18-23カ月", "24-35カ月", "36カ月-"), "6カ月-", dur),
         dur_cate = factor(dur_cate, levels = c("-4日", "5-13日", "14-30日", 
                                                "1-2カ月", "3-5カ月", "6カ月-"))) %>% 
  group_by(sex, year, dur_cate) %>% 
  summarise(prop = sum(prop)) %>% 
  ggplot(aes(x = year, y = prop, fill = dur_cate, label = prop)) +
  geom_bar(position = position_fill(reverse = T), stat = "identity") +
  geom_text(size = 4, position = position_fill(reverse = T, vjust = 0.5)) +
  coord_flip() +
  labs(x = "年次", y = "男性の育休取得期間の割合",
       caption = "データソース：厚生労働省「雇用均等基本調査」. 作成者：茂木良平") +
  scale_fill_manual(values = c(Mycol, "grey")) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box = "vertical",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.spacing = unit(2, "lines"))
ggsave("out/jpn-parentleave_koyou_durm.png", width = 7.5, height = 5, bg = "white")

ggplot(aes(x = year, y = prop, fill = dur)) +
  facet_wrap(~ sex) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  labs(x = "年次", y = "育児休業取得割合",
       caption = "データソース：厚生労働省「雇用均等基本調査」. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box = "vertical",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.spacing = unit(2, "lines"))
ggsave("out/jpn-parentleave_koyou.png", width = 7.5, height = 5, bg = "white")

# JNFS
jnfs_rate <- data.frame(
  year = c("1985-1989", "1990-1994", "1995-1999", "2000-2004", "2005-2009",
           "2010-2014", "2015-2018"),
  f_rate_all = c(6, 9, 12.6, 17.5, 24.1, 34.8, 43),
  m_rate_all = c(NA, NA, 0.2, 0.3, 0.8, 0.9, 3.7),
  f_rate_ful = c(34.1, 50.3, 67, 79.5, 85.9, 88.2, 92.6),
  m_rate_ful = c(NA, NA, 0.9, 0.7, 1.7, 1.7, 6.3)
)

jnfs_rate %>% 
  gather(key = key, value = value, -year) %>% 
  mutate(sex = str_sub(key, 1, 1),
         sex = ifelse(sex == "m", "男性", "女性"),
         var = str_sub(key, -3, -1),
         var = ifelse(var == "all", "総合", "妻が正規雇用"),
         var = factor(var, levels = c("総合", "妻が正規雇用"))) %>% 
  select(-key) %>% 
  ggplot(aes(x = year, y = value, group = sex, colour = sex, label = value)) +
  facet_wrap(~ var) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(nudge_y = 5, show_guide = F) +
  ylim(0, 100) +
  scale_colour_manual(values = c(Mycol[3], Mycol[2])) +
  labs(x = "第一子出生年", y = "育児休業取得割合",
       caption = "データソース：社人研「出生動向基本調査16回」. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box = "vertical",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.spacing = unit(2, "lines"))
ggsave("out/jpn-parentleave_jnfs.png", width = 7.5, height = 5, bg = "white")

# Keidanren: https://www.keidanren.or.jp/policy/2023/040.pdf
kei_rate <- data.frame(
  year = 2018:2022,
  m_kei_rate = c(14, 18.8, 23.7, 29.3, 47.5),
  m_kei_comp = c(168, 185, 186, 194, 198),
  f_kei_rate = c(91.9, 92.6, 95.4, 95.3, 96.4),
  f_kei_comp = c(190, 191, 195, 194, 199)
)

kei_m_dur <- data.frame(
  m_dur = c("5日未満", "5日～2週間", "2週間～1カ月", "1カ月～3カ月", "3カ月～6カ月"),
  m_all = c(9.3, 12.8, 18, 49.4, 10.5),
  m_s5001 = c(1.9, 7.7, 15.4, 69.2, 5.8),
  m_s3001 = c(6.7, 13.3, 16.7, 46.7, 16.7),
  m_s1001 = c(6.5, 17.4, 19.6, 45.7, 10.9),
  m_s501 = c(5.9, 23.5, 17.6, 29.4, 23.5),
  m_s301 = c(21.4, 7.1, 28.6, 35.7, 7.1),
  m_s300 = c(46.2, 7.7, 15.4, 30.8, NA)
)

kei_rate %>% 
  gather(key = key, value = value, -year) %>% 
  mutate(sex = str_sub(key, 1, 1),
         sex = ifelse(sex == "m", "男性", "女性"),
         var = str_sub(key, -4, -1)) %>% 
  select(-key) %>% 
  spread(key = var, value = value) %>% 
  ggplot(aes(x = year, y = rate, group = sex, colour = sex, label = rate)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(nudge_y = -5, show_guide = F) +
  ylim(0, 100) +
  scale_colour_manual(values = c(Mycol[3], Mycol[2])) +
  labs(x = "年次", y = "育児休業取得割合",
       caption = "データソース：経団連「『男性の家事･育児』に関するアンケ―ト調査」. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box = "vertical",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.spacing = unit(2, "lines"))
ggsave("out/jpn-parentleave_keidanren.png", width = 7.5, height = 5, bg = "white")


