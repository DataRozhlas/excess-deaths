data.frame(mvcr %>% group_by(time) %>% summarize(mvcr=n()) %>% mutate(time=as.factor(time)) %>% select(tyden=time),
           data %>% filter(year>2018) %>% arrange(time) %>% select(eurostat=values) %>% add_row(eurostat=rep(NA, 6)), mvcr %>% group_by(time) %>% summarize(mvcr=n()) %>% select(mvcr)) %>% mutate(rozdil=mvcr-eurostat) %>% write.xlsx("mvcr-eurostat-sorvnani.xlsx")



data %>% filter(year>2018) %>% arrange(time) %>% select(eurostat=values)

mvcr %>% group_by(time) %>% summarize(mvcr=n()) %>% mutate(time=as.factor(time))

