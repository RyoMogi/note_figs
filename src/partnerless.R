library(tidyverse)
library(openxlsx)
`%out%` = Negate(`%in%`)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

# reasons to be single
reason <- data.frame(
  sex = c("男性", "男性", "男性", "男性", "女性", "女性", "女性", "女性"),
  age = c("18-24歳", "18-24歳", "25-34歳", "25-34歳", "18-24歳", "18-24歳", "25-34歳", "25-34歳"),
  choice = c("相手がいない", "うまくつき合えない", "相手がいない", "うまくつき合えない", 
             "相手がいない", "うまくつき合えない", "相手がいない", "うまくつき合えない"),
  prop = c(29.8, 12.3, 43.3, 20, 36, 7.3, 48.1, 18.2)
)

reason %>% 
  mutate(sex = factor(sex, levels = c("男性", "女性")),
         choice = factor(choice, levels = c("相手がいない", "うまくつき合えない"))) %>% 
  ggplot(aes(x = prop, y = choice, group = sex, fill = sex, label = prop)) +
  facet_wrap(~ age) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(size = 4.5, hjust = 1.5, position = position_dodge(.9), colour = "white") +
  scale_fill_manual(values = c(Mycol[2], Mycol[3])) +
  labs(x = "独身でいる理由として交際の問題を挙げた割合（％）",
       caption = "データソース：第16回出生動向基本調査結果の概要. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 15),
        strip.text.x = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
ggsave("out/jpn-reason-nevmar.png", width = 7.5, height = 5, bg = "white")

# % of not having a partner
partnerless <- data.frame(
  age = c("20-24", "25-29", "30-34", "20-24", "25-29", "30-34"),
  sex = c("男性", "男性", "男性", "女性", "女性", "女性"),
  prop = c(24, 26.1, 14.2, 30.9, 32.6, 20.6)
)

partnerless %>% 
  mutate(sex = factor(sex, levels = c("男性", "女性")),
         prop = 100 - prop,
         age = paste0(age, "歳"),
         age = factor(age, levels = c( "30-34歳", "25-29歳", "20-24歳"))) %>%
  ggplot(aes(x = prop, y = age, group = sex, fill = sex, label = prop)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(size = 4.5, hjust = 1.5, position = position_dodge(.9), colour = "white", show.legend = F) +
  scale_fill_manual(values = c(Mycol[2], Mycol[3])) +
  xlim(0, 100) +
  labs(x = "交際相手がいない人の割合（％）", 
       caption = "データソース：第16回出生動向基本調査結果の概要. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
ggsave("out/jpn-partnerless.png", width = 7.5, height = 5, bg = "white")

# % of never had a partner
nevpartner <- data.frame(
  age = c("20-24", "25-29", "30-34", "20-24", "25-29", "30-34"),
  sex = c("男性", "男性", "男性", "女性", "女性", "女性"),
  prop = c(40.1, 36.4, 38.9, 34.3, 28.6, 33.9)
)

nevpartner %>% 
  mutate(sex = factor(sex, levels = c("男性", "女性")),
         age = paste0(age, "歳"),
         age = factor(age, levels = c( "30-34歳", "25-29歳", "20-24歳"))) %>% 
  ggplot(aes(x = prop, y = age, group = sex, fill = sex, label = prop)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(size = 4.5, hjust = 1.5, position = position_dodge(.9), colour = "white", show.legend = F) +
  scale_fill_manual(values = c(Mycol[2], Mycol[3])) +
  labs(x = "交際経験が一度もない人の割合（％）",
       caption = "データソース：第16回出生動向基本調査結果の概要. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
ggsave("out/jpn-nevpartner.png", width = 7.5, height = 5, bg = "white")

# % of never had sex
nevsex <- data.frame(
  age = c("25-29", "30-34", "25-29", "30-34"),
  sex = c("男性", "男性", "女性", "女性"),
  prop = c(63.6, 62.8, 61.2, 55.6)
)

nevsex %>% 
  mutate(sex = factor(sex, levels = c("男性", "女性")),
         age = paste0(age, "歳"),
         age = factor(age, levels = c( "30-34歳", "25-29歳")),
         prop = 100 - prop) %>% 
  ggplot(aes(x = prop, y = age, group = sex, fill = sex, label = prop)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(size = 4.5, hjust = 1.5, position = position_dodge(.9), colour = "white", show.legend = F) +
  scale_fill_manual(values = c(Mycol[2], Mycol[3])) +
  labs(x = "性交経験が一度もない人の割合（％）",
       caption = "データソース：第16回出生動向基本調査結果の概要. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
ggsave("out/jpn-nevsex.png", width = 7.5, height = 5, bg = "white")

# values regarding dating
# https://www8.cao.go.jp/shoushi/shoushika/research/r02/kokusai/pdf_index.html
# only among those who are not married nor cohabiting
val <- read.xlsx("data/令和2年少子化調査_恋愛.xlsx")

val %>% 
  gather(key = reason, value = prop, -c(`性別`, `ケース数`, `国`)) %>% 
  mutate(`性別` = factor(`性別`, levels = c("男性", "女性")),
         `国` = factor(`国`, levels = c("日本", "ドイツ", "フランス", "スウェーデン")),
         reason = factor(reason, levels = rev(c("恋愛よりも勉強や仕事を優先したい",                    
                                                "恋愛よりも趣味を優先したい",                     
                                                "交際をする相手との結婚を考える",                        
                                                "それほど好きではない人とも恋愛や交際をしてもかまわない",
                                                "いつも恋愛をしていたい",
                                                "気になる相手には自分から積極的にアプローチをする",      
                                                "相手からアプローチがあれば考える",     
                                                "恋愛することで人生が豊かになる",                       
                                                "恋愛は面倒だと感じる",                       
                                                "恋愛することに自信がない",                              
                                                "恋愛はしたいが、お金がかかる")))) %>% 
  ggplot(aes(x = prop, y = reason, group = `性別`, fill = `性別`, label = prop)) +
  facet_wrap(~ `国`) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(size = 3, hjust = -0.5, position = position_dodge(.9), show.legend = F) +
  scale_fill_manual(values = c(Mycol[2], Mycol[3])) +
  labs(x = "恋愛に関する考え（％）",
       caption = "データソース：令和２年度少子化社会に関する国際意識調査. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
ggsave("out/values_dating.png", width = 9, height = 7.5, bg = "white")
