library(tidyverse)
library(ISOweek)
library(hrbrthemes)

`%nin%` = Negate(`%in%`)

data_mvcr <- mvcr %>% 
  group_by(time, age) %>%
  summarise(values=n()) %>%
  mutate(age=as.factor(age)) %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-"))) %>%
  filter(week %nin% c(1,49,48,47,46))

levels(data_mvcr$age) <- c("do 5 let", "5 až 9 let", "10 až 14 let", "15 až 19 let", "20 až 24 let", "25 až 29 let", "30 až 34 let", "35 až 39 let", "40 až 44 let", "45 až 49 let", "50 až 54 let", "55 až 59 let", "60 až 64 let", "65 až 69 let", "70 až 74 let", "75 až 79 let", "80 až 84 let", "85 až 89 let", "90 a více let")

data <- cz_deaths %>%
  filter(geo=="CZ") %>%
  filter(sex=="T") %>%
  filter(age %nin% c("TOTAL", "UNK")) %>%
  mutate(age=as.factor(age)) %>%
  # select(time, values) %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-")))

levels(data$age) <- c("90 a více let", "do 5 let", "10 až 14 let", "15 až 19 let", "20 až 24 let", "25 až 29 let", "30 až 34 let", "35 až 39 let", "40 až 44 let", "45 až 49 let", "5 až 9 let", "50 až 54 let", "55 až 59 let", "60 až 64 let", "65 až 69 let", "70 až 74 let", "75 až 79 let", "80 až 84 let", "85 až 89 let")

data$age <- factor(data$age, levels = c("do 5 let", "5 až 9 let", "10 až 14 let", "15 až 19 let", "20 až 24 let", "25 až 29 let", "30 až 34 let", "35 až 39 let", "40 až 44 let", "45 až 49 let", "50 až 54 let", "55 až 59 let", "60 až 64 let", "65 až 69 let", "70 až 74 let", "75 až 79 let", "80 až 84 let", "85 až 89 let", "90 a více let"))

data_2020 <- data %>%
  filter(year==2020)

data <- data %>%
  filter(year!=2020)


maxi <- data %>%
  group_by(week, age) %>%
  summarize(max=max(values))

mini <- data %>%
  group_by(week, age) %>%
  summarize(min=min(values))

data <- data %>% left_join(maxi)

data <- data %>% left_join(mini)

data %>%
  filter(week!=53) %>%
  ggplot(aes(week, values, group = year)) +
  facet_wrap(~age, nrow = 7, ncol = 3) +
  geom_line(color="black",alpha=.5,size=.1) +
  geom_ribbon(data = . %>% filter(year == 2016) , aes(ymin = min, ymax = max), alpha=.2, color=NA) +
  geom_line(data = data_mvcr, aes(week, values), color= "firebrick1", linetype="twodash") +
  geom_line(data = data_2020, aes(week, values), color= "firebrick1") +
  geom_line(data = . %>% group_by(week, age) %>% summarize(values=mean(values)) %>% ungroup() %>% mutate(year=2010), aes(week, values), size=.5, color="black") +
  #  annotate(geom="text", x= 41, y = max(data_mvcr$values) + 50, label = paste0("+"), adj=0) +
  scale_x_continuous(breaks=c(5,14,23,31,40,49), labels=c("únor", "duben", "červen", "srpen", "říjen", "prosinec")) +
  labs(title = "Počty zemřelých 2005 - 2020, rozděleno podle věku",
       y="",x="") +
  theme_ipsum(plot_margin = margin(5, 5, 0, 5), 
              plot_title_margin=5 , 
              subtitle_margin=5,
              base_family = "Helvetica") +
  theme(plot.title.position="plot")

ggsave("vek.svg",width=62/2.4,height=34.9,dpi=320)

