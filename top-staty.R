library(readr)
library(dplyr)
library(wpp2019)
library(countrycode)

data(pop)
rm(popF,popFT,popM,popMT)

pop <- pop %>%
  select(1,2,17)

karlinsky <- read_csv("https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv")

pocet_roku <- karlinsky %>%
  filter(year<2020) %>%
  group_by(country_name) %>%
  distinct(year) %>%
  summarise(roku=n()) %>%
  arrange(desc(roku)) %>%
  print(n=100)

umrti_2020 <- karlinsky %>%
  filter(year==2020) %>%
  group_by(country_name) %>%
  summarise(umrti_2020=sum(deaths)) %>%
  arrange(desc(umrti_2020)) %>%
  print(n=100)

umrti_do_2019 <- karlinsky %>%
  filter(year<2020) %>%
  group_by(country_name) %>%
  summarise(umrti_celkem=sum(deaths)) %>%
  arrange(desc(umrti_celkem)) %>%
  print(n=100)

result <- umrti_2020 %>%
  left_join(umrti_do_2019) %>%
  left_join(pocet_roku) %>%
  mutate(dlouhodoby_prumer=umrti_celkem/roku) %>%
  mutate(nadumrti_2020_pct=(umrti_2020/dlouhodoby_prumer-1)*100) %>%
  mutate(country_code=countrycode(country_name, "country.name", "un")) %>%
  arrange(desc(nadumrti_2020_pct)) %>%
  left_join((pop)) %>%
  write_csv("top-staty.csv")
