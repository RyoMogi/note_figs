library(tidyverse)
library(readxl)
library(openxlsx)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

#----- read data
# data is from 人口統計資料集. Accessed on 09/08/2023
# 2005: https://www.ipss.go.jp/syoushika/tohkei/Popular/P_Detail2007.asp?fname=T03-09.html
# 2010: https://www.ipss.go.jp/syoushika/tohkei/Popular/P_Detail2012.asp?fname=T03-09.htm
# 2015: https://www.ipss.go.jp/syoushika/tohkei/Popular/P_Detail2017RE.asp?fname=T03-09.htm
# 2021: https://www.ipss.go.jp/syoushika/tohkei/Popular/P_Detail2023RE.asp?fname=T03-09.htm
d2005 <- read_excel("data/2005_T03-09.xls", skip = 1)[-1, 1:2]
d2010 <- read_excel("data/2010_T03-09.xls", skip = 1)[-1, 1:2]
d2015 <- read_excel("data/2015_T03-09.xls", skip = 1)[-1, 1:2]
d2021 <- read_excel("data/2021_T03-09.xls", skip = 1)[-1, 1:2]
# https://www.ipss.go.jp/pp-zenkoku/j/zenkoku2023/db_zenkoku2023/db_r5_suikeikekka_1.html
pop_est <- read.xlsx("data/pop_estimate2021_1-1.xlsx", startRow = 4)[, 2:3]

#----- clean data
# population momentum
colnames(d2005) <- c("year", "pop")
colnames(d2010) <- c("year", "pop")
colnames(d2015) <- c("year", "pop")
colnames(d2021) <- c("year", "pop")

d2005 <- d2005 %>% 
  mutate(pop2005 = pop / 1000) %>% 
  select(-pop)

d2010 <- d2010 %>% 
  rename(pop2010 = pop)

d2015 <- d2015 %>% 
  rename(pop2015 = pop)

d2021 <- d2021 %>% 
  rename(pop2021 = pop) %>% 
  mutate(year = as.numeric(year))

d_mom <- d2005 %>% 
  full_join(d2010, by = "year") %>% 
  full_join(d2015, by = "year") %>% 
  full_join(d2021, by = "year")

# population estiamte
colnames(pop_est) <- c("year", "pop_est")

pop_est <- pop_est %>% 
  right_join(d2021, by = "year") %>% 
  drop_na()

#----- figure
# population momentum
d_mom %>% 
  filter(year <= 2100) %>% 
  gather(key = yeard, value = pop, -year) %>% 
  mutate(yeard = case_when(yeard == "pop2005" ~ "2005年以降",
                           yeard == "pop2010" ~ "2010年以降",
                           yeard == "pop2015" ~ "2015年以降",
                           yeard == "pop2021" ~ "2021年以降")) %>%
  ggplot(aes(x = year, y = pop, group = yeard, colour = yeard)) +
  geom_line(size = 1.2) +
  scale_colour_manual(values = Mycol) +
  scale_x_continuous(limits = c(2000, 2100)) +
  labs(x = "年次", y = "推計人口数（千人）",
       caption = "データソース：人口統計資料集. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12))
ggsave("out/jpn_popmomentum.png", width = 7.5, height = 5, bg = "white")

# population estimate
pop_est %>% 
  gather(key = type, value = pop, -year) %>% 
  mutate(type = ifelse(type == "pop_est", "人口推計（出生中位・死亡中位）", "2021年以降人口置換水準に戻った場合の人口推計")) %>% 
  ggplot(aes(x = year, y = pop, group = type, colour = type)) +
  geom_line(size = 1.2) +
  scale_colour_manual(values = c(Mycol[4], Mycol[1])) +
  scale_x_continuous(limits = c(2020, 2070)) +
  labs(x = "年次", y = "推計人口数（千人）",
       caption = "データソース：人口統計資料集と将来人口推計. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12))
ggsave("out/jpn_popmomentum_popest.png", width = 7.5, height = 5, bg = "white")
