library(tidyverse)
library(ISOweek)
library(hrbrthemes)

`%nin%` = Negate(`%in%`)

# graf celkový


data <- cz_deaths %>%
  filter(geo == "CZ") %>%
  filter(age=="TOTAL") %>%
  filter(sex=="T") %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-")))

data_2020 <- data %>%
  filter(year==2020)

data <- data %>%
  filter(year!=2020) %>%
  filter(year > 2014)

maxi <- data %>%
  group_by(week) %>%
  summarize(max=max(values))

mini <- data %>%
  group_by(week) %>%
  summarize(min=min(values))

data <- data %>% left_join(maxi)

data <- data %>% left_join(mini)



data %>%
#  filter(week != 53) %>%
  ggplot(aes(week, values, group = year)) +
  geom_line(color = "black",
            alpha = .5,
            size = .1) +
  geom_ribbon(
    data = . %>% filter(year == 2016) ,
    aes(ymin = min, ymax = max),
    alpha = .2,
    color = NA
  ) +
  geom_line(data = data_2020, aes(week, values), color = "firebrick1") +
  geom_line(
    data = . %>% group_by(week) %>% summarize(values = mean(values)) %>% ungroup() %>% mutate(year = 2010),
    aes(week, values),
    size = .5,
    color = "black"
  ) +
  labs(title = "Počty zemřelých po týdnech v České republice 2015–2020",
       y = "", x = "") +
  theme_ipsum(
    plot_margin = margin(5, 5, 0, 5),
    plot_title_margin = 5 ,
    subtitle_margin = 5,
    base_family = "Helvetica"
  ) +
  theme(plot.title.position = "plot")

ggsave(
  "nadumrti-cr-2020.svg",
  width = 62 / 7,
  height = 34.9 / 7,
  dpi = 320
)

data <- be_deaths %>%
  filter(geo == "BE") %>%
  filter(age=="TOTAL") %>%
  filter(sex=="T") %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-")))

data_2020 <- data %>%
  filter(year==2020)

data <- data %>%
  filter(year!=2020) %>%
  filter(year > 2014)

maxi <- data %>%
  group_by(week) %>%
  summarize(max=max(values))

mini <- data %>%
  group_by(week) %>%
  summarize(min=min(values))

data <- data %>% left_join(maxi)

data <- data %>% left_join(mini)



data %>%
  #  filter(week != 53) %>%
  ggplot(aes(week, values, group = year)) +
  geom_line(color = "black",
            alpha = .5,
            size = .1) +
  geom_ribbon(
    data = . %>% filter(year == 2016) ,
    aes(ymin = min, ymax = max),
    alpha = .2,
    color = NA
  ) +
  geom_line(data = data_2020, aes(week, values), color = "firebrick1") +
  geom_line(
    data = . %>% group_by(week) %>% summarize(values = mean(values)) %>% ungroup() %>% mutate(year = 2010),
    aes(week, values),
    size = .5,
    color = "black"
  ) +
  labs(title = "Počty zemřelých po týdnech v Belgii 2015–2020",
       y = "", x = "") +
  theme_ipsum(
    plot_margin = margin(5, 5, 0, 5),
    plot_title_margin = 5 ,
    subtitle_margin = 5,
    base_family = "Helvetica"
  ) +
  theme(plot.title.position = "plot")

ggsave(
  "nadumrti-be-2020.svg",
  width = 62 / 7,
  height = 34.9 / 7,
  dpi = 320
)

data <- pl_deaths %>%
  filter(geo == "PL") %>%
  filter(age=="TOTAL") %>%
  filter(sex=="T") %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-")))

data_2020 <- data %>%
  filter(year==2020)

data <- data %>%
  filter(year!=2020) %>%
  filter(year > 2014)

maxi <- data %>%
  group_by(week) %>%
  summarize(max=max(values))

mini <- data %>%
  group_by(week) %>%
  summarize(min=min(values))

data <- data %>% left_join(maxi)

data <- data %>% left_join(mini)



data %>%
  #  filter(week != 53) %>%
  ggplot(aes(week, values, group = year)) +
  geom_line(color = "black",
            alpha = .5,
            size = .1) +
  geom_ribbon(
    data = . %>% filter(year == 2016) ,
    aes(ymin = min, ymax = max),
    alpha = .2,
    color = NA
  ) +
  geom_line(data = data_2020, aes(week, values), color = "firebrick1") +
  geom_line(
    data = . %>% group_by(week) %>% summarize(values = mean(values)) %>% ungroup() %>% mutate(year = 2010),
    aes(week, values),
    size = .5,
    color = "black"
  ) +
  labs(title = "Počty zemřelých po týdnech v Polsku 2015–2020",
       y = "", x = "") +
  theme_ipsum(
    plot_margin = margin(5, 5, 0, 5),
    plot_title_margin = 5 ,
    subtitle_margin = 5,
    base_family = "Helvetica"
  ) +
  theme(plot.title.position = "plot")

ggsave(
  "nadumrti-be-2020.svg",
  width = 62 / 7,
  height = 34.9 / 7,
  dpi = 320
)
