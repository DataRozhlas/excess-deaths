library(tidyverse)
library(ISOweek)

data <- cz_deaths %>%
  filter(geo=="CZ") %>%
  filter(age=="TOTAL") %>%
  filter(sex=="T") %>%
  # select(time, values) %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-"))) %>%
  mutate(datum_fiktivni=ymd(paste("2020", month(date), day(date), sep="-")))

maxi <- data %>%
  group_by(week) %>%
  summarize(max=max(values))

mini <- data %>%
  group_by(week) %>%
  summarize(min=min(values))

data <- data %>% left_join(maxi)

data <- data %>% left_join(mini)

data_2020 <- data %>%
  filter(year==2020)

# # graf data
# data %>%
#   filter(year<2020) %>%
#   ggplot(aes(datum_fiktivni, values, group = year)) +
#   geom_line(color="black",alpha=.5,size=.1) +
#   geom_ribbon(data = . %>% group_by(datum_fiktivni) %>% filter(row_number()==1),  aes(x=datum_fiktivni, ymin = min, ymax = max), alpha=.2, color=NA, inherit.aes = F)
# geom_line(data = data_2020, aes(datum_fiktivni, values), color= "firebrick1") +
#   geom_line(data = . %>% group_by(week) %>% summarize(values=mean(values)) %>% ungroup() %>% mutate(year=2010), aes(week, values), size=.5, color="black")


# graf týdny
data %>%
  filter(year<2020) %>%
  filter(week!=53) %>%
  ggplot(aes(week, values, group = year)) +
  geom_line(color="black",alpha=.5,size=.1) +
  geom_ribbon(data = . %>% filter(year == 2016) , aes(ymin = min, ymax = max), alpha=.2, color=NA) +
  geom_line(data = data_2020, aes(week, values), color= "firebrick1") +
  geom_line(data = . %>% group_by(week) %>% summarize(values=mean(values)) %>% ungroup() %>% mutate(year=2010), aes(week, values), size=.5, color="black") +
  geom_line(data = . %>% filter(year==2018), color="navyblue", size=.2) +
  annotate(geom="text", x= 26.8, y = 2480, label = "vlny veder\n2018 a 2015", adj =0 ) +
  annotate(geom="text",x= 11, y = 2320, label = "2020", adj=0, color="firebrick1") +
  annotate(geom="text",x= 5, y = 2930, label = "chřipková\nepidemie\n2018", adj=0) +
  annotate(geom="text",x= 43, y = 1750, label = "průměr 2005–2019", adj=0, size=4) +
  geom_segment(data = data.frame(x=42.8, y=1750, 
                                 xend = 35.2, yend = 1874, 
                                 year=2010),
               aes(x=x,y=y,xend=xend,yend=yend), color = "black", 
               arrow = arrow(length = unit(0.1, "inches"), type="closed") ) +
  scale_x_continuous(breaks=c(5,14,23,31,40,49), labels=c("únor", "duben", "červen", "srpen", "říjen", "prosinec")) +
  labs(title = "Počty zemřelých po týdnech v České republice 2005–2020",
       subtitle = "Červeně rok 2020, šedě roky 2005 až 2019, tmavomodře 2018",
       y="",x="",
       caption = "Zdroj dat: Eurostat | Vizualizace: B. Coulmont") +
  theme_ipsum(plot_margin = margin(5, 5, 0, 5), 
              plot_title_margin=5 , 
              subtitle_margin=5,
              base_family = "Helvetica") +
  theme(plot.title.position="plot")

ggsave("~/Desktop/eurostat.png",width=1100/120,height=700/120,dpi=120)
  
#graf týdny MVČR

data_mvcr <- mvcr %>% group_by(time) %>%
  summarise(values=n()) %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-"))) %>%
  mutate(datum_fiktivni=ymd(paste("2020", month(date), day(date), sep="-"))) %>%
  filter(year==2020 & week<17)

data %>%
  filter(year<2020) %>%
  filter(week!=53) %>%
  ggplot(aes(week, values, group = year)) +
  geom_line(color="black",alpha=.5,size=.1) +
  geom_ribbon(data = . %>% filter(year == 2016) , aes(ymin = min, ymax = max), alpha=.2, color=NA) +
  geom_line(data = data_mvcr, aes(week, values), color= "firebrick1", linetype="twodash") +
  geom_line(data = . %>% group_by(week) %>% summarize(values=mean(values)) %>% ungroup() %>% mutate(year=2010), aes(week, values), size=.5, color="black") +
  geom_line(data = . %>% filter(year==2018), color="navyblue", size=.2) +
  annotate(geom="text", x= 26.8, y = 2480, label = "vlny veder\n2018 a 2015", adj =0 ) +
  annotate(geom="text",x= 11, y = 2320, label = "2020 předběžná data", adj=0, color="firebrick1") +
  annotate(geom="text",x= 5, y = 2930, label = "chřipková\nepidemie\n2018", adj=0) +
  annotate(geom="text",x= 43, y = 1750, label = "průměr 2005–2019", adj=0, size=4) +
  geom_segment(data = data.frame(x=42.8, y=1750, 
                                 xend = 35.2, yend = 1874, 
                                 year=2010),
               aes(x=x,y=y,xend=xend,yend=yend), color = "black", 
               arrow = arrow(length = unit(0.1, "inches"), type="closed") ) +
  scale_x_continuous(breaks=c(5,14,23,31,40,49), labels=c("únor", "duben", "červen", "srpen", "říjen", "prosinec")) +
  labs(title = "Počty zemřelých po týdnech v České republice 2005–2020",
       subtitle = "Červeně rok 2020, šedě roky 2005 až 2019, tmavomodře 2018",
       y="",x="",
       caption = "Zdroje dat: Eurostat, Ministerstvo vnitra České republiky - informativní výpis zemřelých z evidence obyvatel") +
  theme_ipsum(plot_margin = margin(5, 5, 0, 5), 
              plot_title_margin=5 , 
              subtitle_margin=5,
              base_family = "Helvetica") +
  theme(plot.title.position="plot")

  ggsave("~/Desktop/mvcr.png",width=1100/120,height=700/120,dpi=120)

