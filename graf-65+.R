library(tidyverse)
library(ISOweek)
library(hrbrthemes)

# starší 65

plus65 <- levels(as.factor(cz_deaths$age))[c(3, 17:21)]


data <- cz_deaths %>%
  filter(geo=="CZ") %>%
  filter(sex=="T") %>%
  filter(age %in% plus65) %>%
  group_by(time) %>%
  summarise(values=sum(values)) %>%
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

data_mvcr <- mvcr %>% 
  filter(Dosažený.věk > 64) %>%
  group_by(time) %>%
  summarise(values=n()) %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-"))) %>%
  mutate(datum_fiktivni=ymd(paste("2020", month(date), day(date), sep="-"))) %>%
  filter(year==2020) %>%
  filter(week>1) %>%
  filter(week<42)


data %>%
  filter(year<2020) %>%
  filter(week!=53) %>%
  ggplot(aes(week, values, group = year)) +
  geom_line(color="black",alpha=.5,size=.1) +
  geom_ribbon(data = . %>% filter(year == 2016) , aes(ymin = min, ymax = max), alpha=.2, color=NA) +
  geom_line(data = data_mvcr, aes(week, values), color= "firebrick1", linetype="twodash") +
  geom_line(data = data_2020, aes(week, values), color= "firebrick1") +
  geom_line(data = . %>% group_by(week) %>% summarize(values=mean(values)) %>% ungroup() %>% mutate(year=2010), aes(week, values), size=.5, color="black") +
  #  annotate(geom="text", x= 41, y = max(data_mvcr$values) + 50, label = paste0("+"), adj=0) +
  scale_x_continuous(breaks=c(5,14,23,31,40,49), labels=c("únor", "duben", "červen", "srpen", "říjen", "prosinec")) +
  labs(title = "Počty zemřelých starších než 65 let",
       y="",x="") +
  theme_ipsum(plot_margin = margin(5, 5, 0, 5), 
              plot_title_margin=5 , 
              subtitle_margin=5,
              base_family = "Helvetica") +
  theme(plot.title.position="plot")

ggsave("starsi65.svg",width=62/7,height=34.9/7,dpi=320)
ggsave("starsi65.png",width=62/7,height=34.9/7,dpi=320)



# o kolik víc než průměr v říjnu
data %>%
  filter(week==41) %>%
  filter(year!=2020) %>%
  group_by(year) %>%
  summarise(celkem=sum(values)) %>%
  summarise(mean(celkem))
