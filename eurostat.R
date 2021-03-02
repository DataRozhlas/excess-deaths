library(devtools)
install_github("ropengov/eurostat")
library(eurostat)
library(tidyverse)
library(openxlsx)

check_access_to_data()

deaths <- get_eurostat("demo_r_mweek3", cache = F)

cz_deaths <- deaths %>%
  filter(grepl("CZ", geo))

de_deaths <- deaths %>%
  filter(grepl("DE", geo))

pl_deaths <- deaths %>%
  filter(grepl("PL", geo))

sk_deaths <- deaths %>%
  filter(grepl("SK", geo))

be_deaths <- deaths %>%
  filter(grepl("BE", geo))

at_deaths <- deaths %>%
  filter(grepl("AT", geo))

se_deaths <- deaths %>%
  filter(grepl("SE", geo))

rm(deaths)

# cz_deaths_total <- cz_deaths %>%
#   filter(age=="TOTAL") %>%
#   filter(nchar(as.character(geo))>4) %>%
#   filter(sex!="T") %>%
#   select(-1, -3) %>%
#   arrange(desc(time), sex, geo)
#   
# 
# table(cz_deaths_total$geo)




write.xlsx(cz_deaths, "eurostat-cz.xlsx", asTable = T)

write_csv(cz_deaths, "deaths_cz.csv")

# zemřeli 2020 so far