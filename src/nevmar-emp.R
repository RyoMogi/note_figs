library(tidyverse)
library(readxl)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

# read data
# data is from Japanese Census. Accessed on 29/05/2023
d2020 <- read.csv(file("data/jpn-census2020-3-1-nevmar-emp.csv", encoding = "cp932"), skip = 12)
d2010 <- read.csv(file("data/jpn-census2010-3-nevmar-emp.csv", encoding = "cp932"), skip = 10)

# 2020
d2020 <- d2020 %>% 
  as.data.frame() %>% 
  mutate(`X.雇用者.正規の職員.従業員` = as.numeric(as.character(gsub(",", "", `X.雇用者.正規の職員.従業員`))),
         `X.雇用者.労働者派遣事業所の派遣社員` = as.numeric(as.character(gsub(",", "", `X.雇用者.労働者派遣事業所の派遣社員`))),
         `X.雇用者.パート.アルバイト.その他` = as.numeric(as.character(gsub(",", "", `X.雇用者.パート.アルバイト.その他`))),
         `役員` = as.numeric(as.character(gsub(",", "", `役員`))),
         `雇人のある業主` = as.numeric(as.character(gsub(",", "", `雇人のある業主`))),
         `雇人のない業主` = as.numeric(as.character(gsub(",", "", `雇人のない業主`))),
         `家族従業者` = as.numeric(as.character(gsub(",", "", `家族従業者`))),
         `家庭内職者` = as.numeric(as.character(gsub(",", "", `家庭内職者`))),
         regular = `X.雇用者.正規の職員.従業員` + `役員`,
         nonregular = `X.雇用者.労働者派遣事業所の派遣社員` + `X.雇用者.パート.アルバイト.その他`,
         famwork = `家族従業者` + `家庭内職者`　+ `雇人のある業主` + `雇人のない業主`) %>% 
  select(sex = `男女`, marstat = `配偶関係`, age = `年齢`,
         regular, nonregular, famwork) %>% 
  gather(key = emptype, value = num, c(regular, nonregular, famwork)) %>% 
  group_by(sex, marstat, emptype) %>% 
  summarise(mean = mean(num)) %>% 
  spread(key = marstat, value = mean) %>% 
  mutate(prop = `未婚` / `総数` * 100,
         sex = ifelse(sex == "男", "男性", "女性"),
         emptype = case_when(emptype == "regular" ~ "正規雇用",
                             emptype == "nonregular" ~ "非正規雇用",
                             emptype == "famwork" ~ "自営業・家族従業者"),
         year = 2020) %>% 
  select(year, sex, emptype, prop, num_nevmar = `未婚`)

# 2010
d2010 <- d2010 %>% 
  as.data.frame() %>% 
  mutate(`X.雇用者.正規の職員.従業員` = as.numeric(as.character(gsub(",", "", `X.雇用者.正規の職員.従業員`))),
         `X.雇用者.労働者派遣事業所の派遣社員` = as.numeric(as.character(gsub(",", "", `X.雇用者.労働者派遣事業所の派遣社員`))),
         `X.雇用者.パート.アルバイト.その他` = as.numeric(as.character(gsub(",", "", `X.雇用者.パート.アルバイト.その他`))),
         `役員` = as.numeric(as.character(gsub(",", "", `役員`))),
         `雇人のある業主` = as.numeric(as.character(gsub(",", "", `雇人のある業主`))),
         `雇人のない業主` = as.numeric(as.character(gsub(",", "", `雇人のない業主`))),
         `家族従業者` = as.numeric(as.character(gsub(",", "", `家族従業者`))),
         `家庭内職者` = as.numeric(as.character(gsub(",", "", `家庭内職者`))),
         regular = `X.雇用者.正規の職員.従業員` + `役員`,
         nonregular = `X.雇用者.労働者派遣事業所の派遣社員` + `X.雇用者.パート.アルバイト.その他`,
         famwork = `家族従業者` + `家庭内職者`　+ `雇人のある業主` + `雇人のない業主`) %>% 
  select(sex = `男女別2010`, marstat = `配偶関係2010`, age = `年齢2`,
         regular, nonregular, famwork) %>% 
  gather(key = emptype, value = num, c(regular, nonregular, famwork)) %>% 
  group_by(sex, marstat, emptype) %>% 
  summarise(mean = mean(num)) %>% 
  spread(key = marstat, value = mean) %>% 
  mutate(prop = `未婚` / `総数（配偶関係）` * 100,
         sex = ifelse(sex == "男", "男性", "女性"),
         emptype = case_when(emptype == "regular" ~ "正規雇用",
                             emptype == "nonregular" ~ "非正規雇用",
                             emptype == "famwork" ~ "自営業・家族従業者"),
         year = 2010) %>% 
  select(year, sex, emptype, prop, num_nevmar = `未婚`)

options(scipen = 10000)
d2020 %>% 
  bind_rows(d2010) %>% 
  mutate(emptype = factor(emptype, levels = c("正規雇用", "非正規雇用", "自営業・家族従業者"))) %>% 
  ggplot(aes(x = year, y = prop, colour = emptype)) +
  facet_wrap(~ sex) +
  geom_line(size = 1.2) +
  geom_point(aes(size = num_nevmar)) +
  scale_colour_manual(values = Mycol[1:3]) +
  scale_x_continuous(n.breaks = 2) +
  labs(x = "年次", y = "50歳時点での未婚者割合",
       caption = "データソース：国勢調査. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box = "vertical",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.spacing = unit(2, "lines"))
ggsave("out/jpn-nevmar-emp.png", width = 7.5, height = 5, bg = "white")
