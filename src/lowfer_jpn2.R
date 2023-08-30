library(tidyverse)
library(readxl)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")


# 記事：「低学歴男性の3人に1人が50歳時に未婚 | 少子化研究者による日本の少子化の現状解説②非婚化」https://note.com/rmogimogi/n/ne1e16055ec41

# 図1
# データソース：国立社会保障人口問題研究所『人口統計資料集2023年版』. Accessed on 25/05/2023
# https://www.ipss.go.jp/syoushika/tohkei/Popular/P_Detail2023RE.asp?fname=T06-23.htm
nevmar <- read_excel(".xls", skip = 3)

names(nevmar) <- c("year", "male_nevmar", "male_mar", "male_wid", "male_div", 
                       "female_nevmar", "female_mar", "female_wid", "female_div")

nevmar %>% 
  select(year, male_nevmar, female_nevmar) %>% 
  gather(key = sex, value = prop_nevmar, -year) %>% 
  mutate(sex = ifelse(sex == "male_nevmar", "男性", "女性"),
         year = as.numeric(as.character(year))) %>% 
  ggplot(aes(x = year, y = prop_nevmar, colour = sex, group = sex)) +
  geom_line(size = 1.2) +
  scale_colour_manual(values = c(Mycol[3], Mycol[2])) +
  scale_x_continuous(n.breaks = 11) +
  labs(x = "年次", y = "50歳時点での未婚者割合",
       caption = "データソース：国立社会保障人口問題研究所『人口統計資料集2023年版』. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("out/jpn-nevmar.png", width = 7, height = 5, bg = "white")


# 図2
# データソース：国勢調査. Accessed on 29/05/2023
# 2020年は就業状態基本集計の11-1
# https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136467&tclass2val=0

# 2010年は産業等基本集計の10-1
# https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00200521&tstat=000001039448&cycle=0&tclass1=000001047544&tclass2=000001050184&tclass3val=0

# 2000年は第2次基本集計の報告書被掲載表の11
# https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00200521&tstat=000000030001&cycle=0&tclass1=000000030147&tclass2=000000030148&tclass3=000000030150&tclass4val=0

# 1990年は第2次基本集計の全国編の008
# https://www.e-stat.go.jp/stat-search/database?page=1&layout=datalist&toukei=00200521&tstat=000000000023&cycle=0&tclass1=000001009029&tclass2=000001009030&tclass3val=0

d2020 <- read.csv(file("data/jpn-census2020-11-1-nevmar-edu.csv", encoding = "cp932"), skip = 12)
d2010 <- read.csv(file("data/jpn-census2010-10-1-nevmar-edu.csv", encoding = "cp932"), skip = 10)
d2000 <- read_excel("data/jpn-census2000-11-nevmar-edu.xls", skip = 10)
d1990 <- read.csv(file("data/jpn-census1990-008-nevmar-edu.csv", encoding = "cp932"), skip = 10)

# 2020
d2020 <- d2020 %>% 
  as.data.frame() %>% 
  mutate(`X.卒業者.小学校` = as.numeric(as.character(gsub(",", "", `X.卒業者.小学校`))),
         `X.卒業者.中学校` = as.numeric(as.character(gsub(",", "", `X.卒業者.中学校`))),
         `未就学者` = as.numeric(as.character(gsub(",", "", `未就学者`))),
         `X.卒業者.高校.旧中` = as.numeric(as.character(gsub(",", "", `X.卒業者.高校.旧中`))),
         `X.卒業者.短大.高専` = as.numeric(as.character(gsub(",", "", `X.卒業者.短大.高専`))),
         `X.卒業者.大学` = as.numeric(as.character(gsub(",", "", `X.卒業者.大学`))),
         `X.卒業者.大学院` = as.numeric(as.character(gsub(",", "", `X.卒業者.大学院`))),
         lowedu = `X.卒業者.小学校` + `X.卒業者.中学校` + `未就学者`,
         midedu = `X.卒業者.高校.旧中`,
         midhighedu = `X.卒業者.短大.高専`,
         highedu = `X.卒業者.大学` + `X.卒業者.大学院`) %>% 
  select(sex = `男女`, marstat = `配偶関係`, age = `年齢`,
         lowedu, midedu, midhighedu, highedu) %>% 
  gather(key = edu, value = num, c(lowedu, midedu, midhighedu, highedu)) %>% 
  group_by(sex, marstat, edu) %>% 
  summarise(mean = mean(num)) %>% 
  spread(key = marstat, value = mean) %>% 
  mutate(prop = `未婚` / `総数` * 100,
         sex = ifelse(sex == "男", "男性", "女性"),
         edu = case_when(edu == "lowedu" ~ "中卒以下",
                         edu == "midedu" ~ "高卒",
                         edu == "midhighedu" ~ "短大、高専",
                         edu == "highedu" ~ "大卒以上"),
         year = 2020) %>% 
  select(year, sex, edu, prop, num_nevmar = `未婚`)

# 2010
d2010 <- d2010 %>% 
  as.data.frame() %>% 
  mutate(`卒業者.小学校.中学校` = as.numeric(as.character(gsub(",", "", `卒業者.小学校.中学校`))),
         `未就学者` = as.numeric(as.character(gsub(",", "", `未就学者`))),
         `卒業者.高校.旧中` = as.numeric(as.character(gsub(",", "", `卒業者.高校.旧中`))),
         `卒業者.短大.高専` = as.numeric(as.character(gsub(",", "", `卒業者.短大.高専`))),
         `卒業者.大学.大学院` = as.numeric(as.character(gsub(",", "", `卒業者.大学.大学院`))),
         lowedu = `卒業者.小学校.中学校` + `未就学者`,
         midedu = `卒業者.高校.旧中`,
         midhighedu = `卒業者.短大.高専`,
         highedu = `卒業者.大学.大学院`) %>% 
  select(sex = `男女別2010`, marstat = `配偶関係2010`, age = `年齢2`,
         lowedu, midedu, midhighedu, highedu) %>% 
  gather(key = edu, value = num, c(lowedu, midedu, midhighedu, highedu)) %>% 
  group_by(sex, marstat, edu) %>% 
  summarise(mean = mean(num)) %>% 
  spread(key = marstat, value = mean) %>% 
  mutate(prop = `未婚` / `総数（配偶関係）` * 100,
         sex = ifelse(sex == "男", "男性", "女性"),
         edu = case_when(edu == "lowedu" ~ "中卒以下",
                         edu == "midedu" ~ "高卒",
                         edu == "midhighedu" ~ "短大、高専",
                         edu == "highedu" ~ "大卒以上"),
         year = 2010) %>% 
  select(year, sex, edu, prop, num_nevmar = `未婚`)

# 2000 (due to a terrible data format, I did a very manual cleaning... check the original excel file)
rowsel <-  c(# total men: 45-49, 50-54
             143, 144,
             # total women: 45-49, 50-54
             159, 160,
             # never married men: 45-49, 50-54
             293, 294,
             # never married women: 45-49, 50-54
             309, 310)

d2000 <- d2000[rowsel - 11, ] %>% 
  mutate(sex = rep(c("男性", "男性", "女性", "女性"), 2),
         marstat = c(rep("total", 4), rep("nevmar", 4)),
         `小学校・中学校` = as.numeric(as.character(gsub(",", "", `小学校・中学校`))),
         `未就学者` = as.numeric(as.character(gsub(",", "", `...19`))),
         `高校・旧中` = as.numeric(as.character(gsub(",", "", `高校・旧中`))),
         `短大・高専` = as.numeric(as.character(gsub(",", "", `短大・高専`))),
         `大学・大学院` = as.numeric(as.character(gsub(",", "", `大学・大学院`))),
         lowedu = `小学校・中学校` + `未就学者`,
         midedu = `高校・旧中`,
         midhighedu =  `短大・高専`,
         highedu = `大学・大学院`) %>% 
  select(sex, marstat, age = `...9`,
         lowedu, midedu, midhighedu, highedu) %>% 
  gather(key = edu, value = num, c(lowedu, midedu, midhighedu, highedu)) %>% 
  group_by(sex, marstat, edu) %>% 
  summarise(mean = mean(num)) %>% 
  spread(key = marstat, value = mean) %>% 
  mutate(prop = nevmar / total * 100,
         edu = case_when(edu == "lowedu" ~ "中卒以下",
                         edu == "midedu" ~ "高卒",
                         edu == "midhighedu" ~ "短大、高専",
                         edu == "highedu" ~ "大卒以上"),
         year = 2000) %>% 
  select(year, sex, edu, prop, num_nevmar = nevmar)

# 1990
d1990 <- d1990 %>% 
  as.data.frame() %>% 
  mutate(`小学校.中学校.人.` = as.numeric(as.character(gsub(",", "", `小学校.中学校.人.`))),
         `未就学者.人.` = as.numeric(as.character(gsub(",", "", `未就学者.人.`))),
         `高校.旧中.人.` = as.numeric(as.character(gsub(",", "", `高校.旧中.人.`))),
         `短大.高専.人.` = as.numeric(as.character(gsub(",", "", `短大.高専.人.`))),
         `大学.大学院.人.` = as.numeric(as.character(gsub(",", "", `大学.大学院.人.`))),
         lowedu = `小学校.中学校.人.` + `未就学者.人.`,
         midedu = `高校.旧中.人.`,
         midhighedu = `短大.高専.人.`,
         highedu = `大学.大学院.人.`) %>% 
  select(sex = `男女Ａ030001`, marstat = `配偶関係Ｃ030009`, age = `X15歳以上年齢030366`,
         lowedu, midedu, midhighedu, highedu) %>% 
  gather(key = edu, value = num, c(lowedu, midedu, midhighedu, highedu)) %>% 
  group_by(sex, marstat, edu) %>% 
  summarise(mean = mean(num)) %>% 
  spread(key = marstat, value = mean) %>% 
  mutate(prop = `未婚` / `総数（不詳を含む）` * 100,
         sex = ifelse(sex == "男", "男性", "女性"),
         edu = case_when(edu == "lowedu" ~ "中卒以下",
                         edu == "midedu" ~ "高卒",
                         edu == "midhighedu" ~ "短大、高専",
                         edu == "highedu" ~ "大卒以上"),
         year = 1990) %>% 
  select(year, sex, edu, prop, num_nevmar = `未婚`)

options(scipen = 10000)
d2020 %>% 
  bind_rows(d2010) %>% 
  bind_rows(d2000) %>% 
  bind_rows(d1990) %>% 
  mutate(edu = factor(edu, levels = c("中卒以下", "高卒", "短大、高専", "大卒以上"))) %>% 
  ggplot(aes(x = year, y = prop, colour = edu)) +
  facet_wrap(~ sex) +
  geom_point(aes(size = num_nevmar)) +
  geom_line(size = 1.2) +
  scale_colour_manual(values = Mycol[1:4]) +
  scale_size(range = c(1, 7)) +
  ylim(0, 40) +
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
ggsave("out/jpn-nevmar-edu.png", width = 7.5, height = 5, bg = "white")
