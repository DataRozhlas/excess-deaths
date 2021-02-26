library(readr)
library(dplyr)

karlinsky <- read_csv("https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv")

pocet_roku <- karlinsky %>%
  filter(year<2021) %>%
  group_by(country_name) %>%
  distinct(year) %>%
  summarise(roku=n()) %>%
  arrange(desc(roku)) %>%
  print(n=100)

umrti_2020 <- karlinsky %>%
  filter(year==2020) %>%
  group_by(country_name) %>%
  summarise(umrti=sum(deaths)) %>%
  arrange(desc(umrti)) %>%
  print(n=100)

umrti_do_2019 <- karlinsky %>%
  filter(year<2020) %>%
  group_by(country_name) %>%
  summarise(umrti_celkem=sum(deaths)) %>%
  arrange(desc(umrti_celkem)) %>%
  print(n=100)
