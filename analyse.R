library(eurostat)

check_access_to_data()

deaths <- get_eurostat("demo_r_mweek3", cache = F)

library(dplyr)



cz_deaths <- deaths %>%
  filter(grepl("CZ", geo))

library(openxlsx)


write.xlsx(cz_deaths, "deaths_cz.xlsx")

levels(cz_deaths$age)
