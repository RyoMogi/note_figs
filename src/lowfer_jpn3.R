library(tidyverse)
library(readxl)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

# 記事：「経済的な不安定さが要因か | 少子化研究者による日本の少子化の現状解説③未婚・晩婚化」https://note.com/rmogimogi/n/n61d3e5ab325f

# 図1
# データソース：国立社会保障人口問題研究所『人口統計資料集2023年版』. Accessed on 25/05/2023
# https://www.ipss.go.jp/syoushika/tohkei/Popular/P_Detail2023RE.asp?fname=T06-24.htm
nevmar_all <- read_excel(".xls", skip = 2)

nevmar_all[-c(1, 18), ] %>% 
  mutate(Sex = c(rep("男性", 16), rep("女性", 16))) %>% 
  filter(`年 齢` %in% c("25～29", "30～34")) %>% 
  gather(key = Year, value = nevmar, -c("年 齢", "Sex")) %>% 
  mutate(nevmar = as.numeric(as.character(nevmar)),
         Year = str_sub(Year, 1, 4),
         Year = as.numeric(as.character(Year))) %>%
  rename(Age = "年 齢") %>% 
  ggplot(aes(x = Year, y = nevmar, group = Age, colour = Age, label = nevmar)) +
  facet_wrap(~ Sex) +
  geom_line(size = 1.2) +
  geom_text(nudge_y = 3.5, size = 3, show_guide = F) +
  scale_colour_manual(values = c(Mycol[2], Mycol[3])) +
  ylim(0, 80) +
  labs(x = "年次", y = "未婚者割合",
       caption = "データソース：国立社会保障人口問題研究所『人口統計資料集2023年版』. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box = "vertical",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.spacing = unit(2, "lines"))
ggsave("out/jpn-nevmar25-34.png", width = 8, height = 5, bg = "white")

# 図2
# データソース：国立社会保障人口問題研究所『人口統計資料集2023年版』. Accessed on 25/05/2023
# https://www.ipss.go.jp/syoushika/tohkei/Popular/P_Detail2023RE.asp?fname=T06-12.htm
m1stmar <- read_excel(".xls", skip = 3)[, c(1, 5, 6, 8, 12, 13)]

names(m1stmar) <- c("year", "male", "female", "year", "male", "female")
m1stmar <- rbind(m1stmar[1:3], m1stmar[4:6])

m1stmar %>%
  as.data.frame() %>% 
  gather(key = sex, value = meanage, -year) %>% 
  mutate(sex = ifelse(sex == "male", "男性", "女性"),
         year = as.numeric(as.character(year)),
         meanage = as.numeric(as.character(meanage))) %>% View()
ggplot(aes(x = year, y = meanage, colour = sex, group = sex)) +
  geom_line(size = 1.2) +
  scale_colour_manual(values = c(Mycol[3], Mycol[2])) +
  #scale_x_continuous(n.breaks = 11) +
  labs(x = "年次", y = "平均初婚年齢",
       caption = "データソース：国立社会保障人口問題研究所『人口統計資料集2023年版』. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = c(0.1, 0.8),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("out/jpn-mean1stmar.png", width = 7.5, height = 5, bg = "white")