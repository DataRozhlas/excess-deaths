library(tidyverse)
library(ISOweek)
library(hrbrthemes)

`%nin%` = Negate(`%in%`)

data_mvcr <- mvcr %>% 
  group_by(time, geo) %>%
  summarise(values=n()) %>%
  mutate(geo=as.factor(geo)) %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-"))) %>%
  filter(week %nin% c(1,49,48,47,46))

levels(data_mvcr$geo) <- c("Praha", "Jihočeský kraj", "Jihomoravský kraj", "Karlovarský kraj", "Vysočina", "Královéhradecký kraj", "Liberecký kraj", "Moravskoslezský kraj", "Olomoucký kraj", "Pardubický kraj", "Plzeňský kraj", "Středočeský kraj", "Ústecký kraj", "Zlínský kraj")

data <- cz_deaths %>%
  filter(nchar(geo)>4) %>%
  filter(sex=="T") %>%
  filter(age=="TOTAL") %>%
  mutate(geo=as.factor(geo)) %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-")))

levels(data$geo) <- c("Praha", "Středočeský kraj", "Jihočeský kraj", "Plzeňský kraj", "Karlovarský kraj", "Ústecký kraj", "Liberecký kraj", "Královéhradecký kraj", "Pardubický kraj", "Vysočina", "Jihomoravský kraj", "Olomoucký kraj", "Zlínský kraj", "Moravskoslezský kraj")

data_2020 <- data %>%
  filter(year==2020)

data <- data %>%
  filter(year!=2020)


maxi <- data %>%
  group_by(week, geo) %>%
  summarize(max=max(values))

mini <- data %>%
  group_by(week, geo) %>%
  summarize(min=min(values))

data <- data %>% left_join(maxi)

data <- data %>% left_join(mini)

data %>%
  filter(week!=53) %>%
  ggplot(aes(week, values, group = year)) +
  facet_wrap(~geo, nrow = 7, ncol = 2) +
  geom_line(color="black",alpha=.5,size=.1) +
  geom_ribbon(data = . %>% filter(year == 2016) , aes(ymin = min, ymax = max), alpha=.2, color=NA) +
  geom_line(data = data_mvcr, aes(week, values), color= "firebrick1", linetype="twodash") +
  geom_line(data = data_2020, aes(week, values), color= "firebrick1") +
  geom_line(data = . %>% group_by(week, geo) %>% summarize(values=mean(values)) %>% ungroup() %>% mutate(year=2010), aes(week, values), size=.5, color="black") +
  #  annotate(geom="text", x= 41, y = max(data_mvcr$values) + 50, label = paste0("+"), adj=0) +
  scale_x_continuous(breaks=c(5,14,23,31,40,49), labels=c("únor", "duben", "červen", "srpen", "říjen", "prosinec")) +
  labs(title = "Počty zemřelých 2005 - 2020, rozděleno podle kraje",
       y="",x="") +
  theme_ipsum(plot_margin = margin(5, 5, 0, 5), 
              plot_title_margin=5 , 
              subtitle_margin=5,
              base_family = "Helvetica") +
  theme(plot.title.position="plot")

ggsave("kraj.svg",width=62/3.5,height=34.9,dpi=320)
