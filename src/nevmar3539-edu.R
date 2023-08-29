library(tidyverse)
library(readxl)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

# read data
# data is from Japanese Census. Accessed on 
nevmar_all <- read_excel("data/NIPSSR2023-T06-24.xls", skip = 2)
d2020 <- read.csv(file("data/jpn-census2020-11-1-nevmar3539-edu.csv", encoding = "cp932"), skip = 12)

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
