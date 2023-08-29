library(tidyverse)
library(openxlsx)
`%out%` = Negate(`%in%`)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")


#----- read data
# data is from Human Fertility Database. Accessed on 25/05/2023
ptfr <- read.xlsx("../data/TFR.xlsx", sheet = "Total fertility rates", startRow = 2)[-1, ]

dptfr <- ptfr %>% 
  as.data.frame() %>% 
  rename(year = COUNTRY) %>% 
  gather(key = country, value = ptfr, -year) %>% 
  mutate(ptfr = as.numeric(as.character(ptfr)),
         ptfr = ifelse(ptfr == 0, NA, ptfr),
         year = as.numeric(as.character(year)))


dptfr %>% 
  filter(country %out% c("Germany.East", "Germany.West", "UK.-.England.and.Wales",
                         "UK.-.Northern.Ireland", "UK.-.Scotland", "Chile"),
         year >= 1990,
         year <= 2010) %>% 
  mutate(region = case_when(country %in% c("Lithuania", "Estonia", "Russia", "Latvia",
                                           "Ukraine", "Belarus", "Romania", "Bulgaria") ~ "東欧",
                            country %in% c("Slovakia", "Hungary", "Slovenia", "Poland",
                                           "Germany", "Czechia") ~ "中欧",
                            country %in% c("Portugal", "Spain", "Italy") ~ "南欧",
                            country %in% c("Taiwan", "Japan", "Republic.of.Korea") ~ "東アジア",
                            country %in% c("Sweden", "USA", "United.Kingdom", "France", "Netherlands",
                                           "Austria", "Switzerland", "Canada",
                                           "Norway", "Denmark", "Finland") ~ "西欧と北米"),
         region = factor(region, levels = c("中欧", "南欧", "東欧", "東アジア", "西欧と北米"))) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = ptfr, group = country)) +
  facet_wrap(~ region) +
  geom_hline(yintercept = 1.3, colour = Mycol[3], linetype = "dashed", size = 1.1) +
  geom_line(colour = "grey40") +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 12))
ggsave("out/ptfr-19902010.png", width = 7.5, height = 5, bg = "white")

# Japan
dptfr %>% 
  filter(country == "Japan",
         year >= 2000,
         year <= 2015) %>% View()
  ggplot(aes(x = year, y = ptfr, group = country)) +
  geom_hline(yintercept = 1.3, colour = Mycol[3], linetype = "dashed", size = 1.1) +
  geom_line(colour = "grey40", size = 1.2) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text.x = element_text(size = 12))
ggsave("out/jpn-ptfr-20002015.png", width = 7.5, height = 5, bg = "white")
