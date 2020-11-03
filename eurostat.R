library(devtools)
install_github("ropengov/eurostat")
library(eurostat)
library(dplyr)
library(readr)

check_access_to_data()

deaths <- get_eurostat("demo_r_mweek3", cache = F)

cz_deaths <- deaths %>%
  filter(grepl("CZ", geo))

deaths %>%
  arrange(desc(time)) %>%


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




write.xlsx(cz_deaths, "deaths_cz.xlsx")

write_csv(cz_deaths, "deaths_cz.csv")
