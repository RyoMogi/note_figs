library(tidyverse)
library(openxlsx)
#library(gghighlight)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")


#----- read data
# data is from Human Fertility Database. Accessed on 25/05/2023
ptfr <- read.xlsx("../data/TFR.xlsx", sheet = "Total fertility rates", startRow = 2)[-1, ]
ctfr <- read.xlsx("../data/CCF.xlsx", sheet = "Completed cohort fertility", startRow = 2)[-1, ]
# Accessed on 27/07/2023
adjtfr <- read.xlsx("../data/adjTFR.xlsx", sheet = "Tempo-adjusted TFR", startRow = 2)[-1, ]
# Accessed on 02/08/2023
pmab <- read.xlsx("../data/MAB.xlsx", sheet = "Mean age at birth", startRow = 2)[-1, ]



#----- merge all data
dptfr <- ptfr %>% 
  as.data.frame() %>% 
  rename(year = COUNTRY) %>% 
  gather(key = country, value = ptfr, -year) %>% 
  mutate(ptfr = as.numeric(as.character(ptfr)),
         ptfr = ifelse(ptfr == 0, NA, ptfr),
         year = as.numeric(as.character(year)))

mean_mab <- pmab %>% 
  as.data.frame() %>% 
  rename(year = COUNTRY) %>% 
  gather(key = country, value = mab, -year) %>% 
  mutate(mab = as.numeric(as.character(mab)),
         mab = ifelse(mab == 0, NA, mab),
         year = as.numeric(as.character(year))) %>% 
  filter(year %in% 1960:1999) %>% 
  group_by(country) %>% 
  summarise(mean_mab = round(mean(mab, na.rm = T)))


dctfr <- ctfr %>% 
  as.data.frame() %>% 
  rename(cohort = COUNTRY) %>% 
  gather(key = country, value = ctfr, -cohort) %>% 
  mutate(ctfr = as.numeric(as.character(ctfr)),
         ctfr = ifelse(ctfr == 0, NA, ctfr),
         cohort = as.numeric(as.character(cohort))) %>% 
  left_join(mean_mab, by = c("country")) %>% 
  mutate(year = cohort + mean_mab)

dadjtfr <- adjtfr %>% 
  as.data.frame() %>% 
  rename(year = COUNTRY) %>% 
  gather(key = country, value = adjtfr, -year) %>% 
  mutate(adjtfr = as.numeric(as.character(adjtfr)),
         adjtfr = ifelse(adjtfr == 0, NA, adjtfr),
         year = as.numeric(as.character(year)))

d_3tfr <- dptfr %>% 
  left_join(dctfr, by = c("country", "year")) %>% 
  left_join(dadjtfr, by = c("country", "year")) %>% 
  gather(key = index, value = tfr, -c(year, country, cohort, mean_mab))


#----- figure
# Czechia
# only period TFR
d_3tfr %>% 
  filter(country == "Czechia",
         index == "ptfr") %>% 
  mutate(country = ifelse(country == "Czechia", "チェコ", "NA"),
         index = ifelse(index == "ptfr", "期間TFR", "NA")) %>% 
  ggplot(aes(x = year, y = tfr)) +
  geom_vline(xintercept = 1968, colour = "grey80", size = 5) +
  geom_vline(xintercept = 2001, colour = "grey80", size = 5) +
  geom_line(size = 1.2, colour = Mycol[3]) +
  annotate("segment", x = 1970, y = 1.8, xend = 1975, yend = 2.3,
           arrow = arrow(), size = 1.1) +
  annotate("segment", x = 2002, y = 1, xend = 2009, yend = 1.35,
           arrow = arrow(), size = 1.1) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12))
ggsave("out/cze_ptfr.png", width = 7.5, height = 5, bg = "white")

# both period and cohort TFR
d_3tfr %>% 
  filter(country == "Czechia",
         index != "adjtfr") %>% 
  mutate(country = ifelse(country == "Czechia", "チェコ", "NA"),
         index = case_when(index == "ctfr" ~ "コーホートTFR", 
                           index == "ptfr" ~ "ピリオドTFR"),
         index = factor(index, levels = c("コーホートTFR", "ピリオドTFR"))) %>% 
  ggplot(aes(x = year, y = tfr, group = index, colour = index)) +
  geom_vline(xintercept = 1968, colour = "grey80", size = 5) +
  geom_vline(xintercept = 2001, colour = "grey80", size = 5) +
  geom_line(size = 1.2) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  scale_colour_manual(values = c(Mycol[1], Mycol[3])) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12))
ggsave("out/cze_ptfr_ctfr.png", width = 7.5, height = 5, bg = "white")

# Japan
d_3tfr %>% 
  filter(country == "Japan",
         index != "adjtfr") %>% 
  mutate(country = ifelse(country == "Japan", "日本", "NA"),
         index = case_when(index == "ctfr" ~ "コーホートTFR", 
                           index == "ptfr" ~ "ピリオドTFR"),
         index = factor(index, levels = c("コーホートTFR", "ピリオドTFR"))) %>% 
  ggplot(aes(x = year, y = tfr, group = index, colour = index)) +
  geom_line(size = 1.2) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  scale_colour_manual(values = c(Mycol[1], Mycol[3], Mycol[4])) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12))
ggsave("out/jpn_ptfr_ctfr.png", width = 7.5, height = 5, bg = "white")

# Czechia and Sweden
d_3tfr %>% 
  filter(country %in% c("Czechia", "Sweden"),
         index != "adjtfr") %>% 
  mutate(country = ifelse(country == "Czechia", "チェコ", "スウェーデン"),
         index = ifelse(index == "ctfr", "コーホートTFR", "ピリオドTFR")) %>% 
  ggplot(aes(x = year, y = tfr, group = index, colour = index)) +
  facet_wrap(~ country) + 
  geom_line(size = 1.2) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  scale_colour_manual(values = c(Mycol[1], Mycol[3])) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12))
ggsave("out/cze_swe_ptfr_ctfr.png", width = 7.5, height = 5, bg = "white")

# cover image
d_3tfr %>% 
  filter(country == "Czechia",
         index == "ptfr") %>% 
  mutate(country = ifelse(country == "Czechia", "チェコ", "NA"),
         index = ifelse(index == "ptfr", "ピリオドTFR", "NA")) %>% 
  ggplot(aes(x = year, y = tfr)) +
  geom_vline(xintercept = 1968, colour = "grey80", size = 5) +
  geom_vline(xintercept = 2001, colour = "grey80", size = 5) +
  geom_line(size = 1.2, colour = Mycol[3]) +
  annotate("segment", x = 1970, y = 1.8, xend = 1975, yend = 2.3,
           arrow = arrow(), size = 1.1) +
  annotate("segment", x = 2002, y = 1, xend = 2009, yend = 1.35,
           arrow = arrow(), size = 1.1) +
  labs(y = "合計特殊出生率") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12))
ggsave("out/cze_ptfr.png", width = 5, height = 3, bg = "white")



d_3tfr %>% 
  filter(country == "Japan") %>% View()
  mutate(country = ifelse(country == "Japan", "日本", "NA"),
         index = case_when(index == "ctfr" ~ "コーホートTFR", 
                           index == "ptfr" ~ "ピリオドTFR",
                           index == "adjtfr" ~ "テンポ効果調整済みピリオドTFR"),
         index = factor(index, levels = c("コーホートTFR", "ピリオドTFR", "テンポ効果調整済みピリオドTFR"))) %>% 
  ggplot(aes(x = year, y = tfr, group = index, colour = index)) +
  geom_line(size = 1.2) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  scale_colour_manual(values = c(Mycol[1], Mycol[3], Mycol[4])) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12))
ggsave("out/jpn_ptfr_adjtfr_ctfr.png", width = 7.5, height = 5, bg = "white")
