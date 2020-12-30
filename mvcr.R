library(openxlsx)
library(lubridate)
library(dplyr)

# mvcr <- read.xlsx("Informativní přehled úmrtí.xlsx",detectDates = T)
# mvcr <- read.xlsx("Úmrtí-osob-výpis.xlsx", detectDates =T)
# mvcr <- read.xlsx("umrti-vypis-kveten.xlsx", detectDates =T, startRow = 2)

mvcr <- read.xlsx("umrti-vypis-prosinec.xlsx", detectDates =T, startRow = 2)

# převeď datum úmrtí na číslo týdne
mvcr$time <- paste0(isoyear(mvcr$Datum.úmrtí), "W", formatC(isoweek(mvcr$Datum.úmrtí), format="d", width=2, flag="0"))

# najdi černý den

read.xlsx("umrti-vypis-listopad.xlsx", detectDates =T, startRow = 2) %>%
 group_by(Datum.úmrtí) %>%
  summarise(pocet=n()) %>%
  arrange(desc(pocet))

# kolik lidí zemřelo v říjnu

read.xlsx("umrti-vypis-prosinec.xlsx", detectDates =T, startRow = 2) %>%
  group_by(month(Datum.úmrtí)) %>%
  summarise(pocet=n()) %>%
  arrange(desc(pocet))


# zemřeli 2020 so far
cz_deaths %>%
  filter(geo=="CZ") %>%
  filter(sex=="T") %>%
  filter(age=="TOTAL") %>%
  mutate(year=as.numeric(str_sub(time, 1, 4)))%>%
  filter(year>2019) %>%
  summarise(total=sum(values))

mvcr <- read.xlsx("umrti-vypis-prosinec.xlsx", detectDates =T, startRow = 2)
mvcr$time <- as.numeric(isoweek(mvcr$Datum.úmrtí))

mvcr %>%
  filter(time > 45) %>%
  summarise(n())

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
#  filter(time %in% c("2020W39", "2020W40", "2020W41", "2020W42", "2020W43")) %>%
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

write.xlsx(mvcr_total_export, "mvcr.xlsx", asTable=T)

