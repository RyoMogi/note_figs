library(tidyverse)
library(ggrepel)
library(openxlsx)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")



dnk_parleave <- read.xlsx("data/dnk-stats-BARSEL04.xlsx", sheet = "BARSEL04", startRow = 3)[c(1:6), ]
dnk_parleave <- dnk_parleave[, -c(1:4)] %>% 
  rename(edu_m = X5) %>% 
  gather(key = edu_f, value = days, -edu_m)

dnk_parleave %>% 
  filter(edu_m != "All fathers, regardless of education", 
         edu_f != "All.mothers,.regardless.of.education") %>% 
  mutate(edu_m = case_when(edu_m == "Father lower secondary" ~ "中卒", 
                           edu_m == "Father upper secondary" ~ "高卒",
                           edu_m == "Father short cycle tertiary" ~ "短大・専門卒", 
                           edu_m == "Father bachelor" ~ "大卒",
                           edu_m == "Father master" ~ "修士卒"),
         edu_f = case_when(edu_f == "Mother.lower.secondary" ~ "中卒", 
                           edu_f == "Mother.upper.secondary" ~ "高卒",
                           edu_f == "Mother.short.cycle.tertirary" ~ "短大・専門卒", 
                           edu_f == "Mother.bachelor" ~ "大卒",
                           edu_f == "Mother.master" ~ "修士卒"),
         edu_m = factor(edu_m, levels = c("中卒", "高卒", "短大・専門卒",
                                          "大卒", "修士卒")),
         edu_f = factor(edu_f, levels = c("中卒", "高卒", "短大・専門卒",
                                          "大卒", "修士卒")),
         days = as.numeric(as.character(days))) %>% 
  ggplot(aes(x = edu_m, y = edu_f, fill = days, label = days)) +
  geom_tile() +
  geom_text(show_guide = F) +
  scale_fill_gradient(low = "white", high = "red", name = "平均育児休業取得日数") +
  labs(x = "父親", y = "母親",
       caption = "データソース：デンマーク統計局. 作成者：茂木良平") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave("out/dnk-parentleave_days.png", width = 7.5, height = 5, bg = "white")
