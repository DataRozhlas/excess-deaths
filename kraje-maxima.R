library(openxlsx)

data_mvcr <- mvcr %>% 
  group_by(time, geo) %>%
  summarise(values=n()) %>%
  mutate(geo=as.factor(geo)) %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-"))) %>%
  filter(week %nin% c(1,49,48,47,46))

maxima_v_krajich <- data_mvcr %>%
  group_by(geo) %>%
  summarise(values=max(values)) %>%
  left_join(data_mvcr) %>%
  select(geo,values,week)

levels(maxima_v_krajich$geo) <- c("Praha", "Jihočeský kraj", "Jihomoravský kraj", "Karlovarský kraj", "Vysočina", "Královéhradecký kraj", "Liberecký kraj", "Moravskoslezský kraj", "Olomoucký kraj", "Pardubický kraj", "Plzeňský kraj", "Středočeský kraj", "Ústecký kraj", "Zlínský kraj")

data <- cz_deaths %>%
  filter(nchar(geo)>4) %>%
  filter(sex=="T") %>%
  filter(age=="TOTAL") %>%
  mutate(geo=as.factor(geo)) %>%
  mutate(week=as.numeric(str_sub(time, 6, 7))) %>%
  mutate(year=as.numeric(str_sub(time, 1, 4))) %>%
  mutate(date=ISOweek2date(paste(year, paste0("W", str_sub(time, 6, 7)), "7", sep="-"))) %>%
  filter(year!=2020)

levels(data$geo) <- c("Praha", "Středočeský kraj", "Jihočeský kraj", "Plzeňský kraj", "Karlovarský kraj", "Ústecký kraj", "Liberecký kraj", "Královéhradecký kraj", "Pardubický kraj", "Vysočina", "Jihomoravský kraj", "Olomoucký kraj", "Zlínský kraj", "Moravskoslezský kraj")

kraje_tydny_prumery <-  data %>%
  group_by(geo, week) %>%
  summarise(prumer=mean(values))

kraje_tydny_prumery %>%
  right_join(maxima_v_krajich) %>%
  mutate(pct_rozdil=round(values/prumer*100-100, 1)) %>%
  write.xlsx("kraje_maxima.xlsx")
  
