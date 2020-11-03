deaths %>%
  filter(geo=="AT") %>%
  arrange(time)


data <- deaths %>%
  filter(geo=="AT") %>%
  filter(age=="TOTAL") %>%
  filter(sex=="T") %>%
  # select(time, values) %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-"))) %>%
  mutate(datum_fiktivni=ymd(paste("2020", month(date), day(date), sep="-")))

data_2020 <- data %>%
  filter(year==2020)

data_2020 %>%
  select(values,week) %>%
  arrange(desc(values)) %>%
  head()


data <- data %>% filter(year!=2020)

maxi <- data %>%
  group_by(week) %>%
  summarize(max=max(values))

mini <- data %>%
  group_by(week) %>%
  summarize(min=min(values))

data <- data %>% left_join(maxi)

data <- data %>% left_join(mini)

# graf týdny
data %>%
  filter(year<2020) %>%
  filter(week!=53) %>%
  ggplot(aes(week, values, group = year)) +
  geom_line(color="black",alpha=.5,size=.1) +
  geom_ribbon(data = . %>% filter(year == 2016) , aes(ymin = min, ymax = max), alpha=.2, color=NA) +
  geom_line(data = data_2020, aes(week, values), color= "firebrick1") +
  geom_line(data = . %>% group_by(week) %>% summarize(values=mean(values)) %>% ungroup() %>% mutate(year=2010), aes(week, values), size=.5, color="black") +
  annotate(geom="text",x= 15.5, y = 1800, label = "2020", adj=0, color="firebrick1") +
  ##annotate(geom="text",x= 43, y = 1750, label = "průměr 2005–2019", adj=0, size=4) +
  #geom_segment(data = data.frame(x=42.8, y=1750, 
  #                               xend = 35.2, yend = 1874, 
  #                               year=2010),
  #             aes(x=x,y=y,xend=xend,yend=yend), color = "black", 
  #             arrow = arrow(length = unit(0.1, "inches"), type="closed") ) +
  scale_x_continuous(breaks=c(5,14,23,31,40,49), labels=c("únor", "duben", "červen", "srpen", "říjen", "prosinec")) +
  labs(title = "Počty zemřelých po týdnech v Rakousku 2000–2020",
       subtitle = "Červeně rok 2020, šedě roky 2000 až 2019, černě průměr",
       y="",x="",
       caption = "Zdroj dat: Eurostat | Vizualizace: iROZHLAS.cz, B. Coulmont") +
  theme_ipsum(plot_margin = margin(5, 5, 0, 5), 
              plot_title_margin=5 , 
              subtitle_margin=5,
              base_family = "Helvetica") +
  theme(plot.title.position="plot")


#ggsave("eurostat-at.png",width=11*1.2,height=3.67*1.2,dpi=320)
ggsave("eurostat-at.svg",width=62/7,height=34.9/7,dpi=320)
