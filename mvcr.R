library(openxlsx)
library(lubridate)
library(dplyr)

# mvcr <- read.xlsx("Informativní přehled úmrtí.xlsx",detectDates = T)
mvcr <- read.xlsx("Úmrtí-osob-výpis.xlsx", detectDates = T)

# převeď datum úmrtí na číslo týdne
mvcr$time <- paste0(year(mvcr$Datum.úmrtí), "W", formatC(isoweek(mvcr$Datum.úmrtí), format="d", width=2, flag="0"))

# pohlaví anglicky
mvcr[mvcr$Pohlaví=="Z",]$Pohlaví <- "F"
names(mvcr)[2] <- "sex"

# názvy krajů na kódy NUTS 
mvcr$geo <- as.factor(mvcr$Kraj.trvalého.pobytu)
levels(mvcr$geo) <- c("CZ010", "CZ031", "CZ064", "CZ041", "CZ063", "CZ052", "CZ051", "CZ080", "CZ071", "CZ053", "CZ032", "CZ020", "CZ042", "CZ072")

# doplnit věkové skupiny
labs <- c("Y_LT5", paste0("Y", seq(5, 85, by = 5), "-", seq(5 + 5 - 1, 90 - 1, by = 5)), "Y_GE90")
mvcr$age <- cut(mvcr$Dosažený.věk, breaks=c(seq(0, 90, by = 5), Inf), labels=labs, right=FALSE)

# agregace, jen období, které chybí v datech eurostatu
mvcr_total <- mvcr %>%
  filter(time %in% c("2020W19", "2020W18", "2020W17", "2020W16", "2020W15", "2020W14")) %>%
  group_by(age, sex, geo, time) %>%
  summarise(values=n()) %>%
  arrange(desc(time), sex, geo) 

mvcr_total_export <- data.frame(unit=factor("NR"), sex=as.factor(mvcr_total$sex), age=mvcr_total$age, geo=mvcr_total$geo, time=as.factor(mvcr_total$time), values=mvcr_total$values)

# odfiltruj součty

cz_deaths_compact <- cz_deaths %>%
  filter(!age=="TOTAL") %>%
  filter(!age=="UNK") %>%
  filter(!sex=="T") %>%
  filter(!nchar(as.character(geo))<5) %>%
  filter(values>0)


write.xlsx(rbind(cz_deaths_compact, mvcr_total_export) %>% arrange(desc(time)), "deaths_cz_mvcr.xlsx")
