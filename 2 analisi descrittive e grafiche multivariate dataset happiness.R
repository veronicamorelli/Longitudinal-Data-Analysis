
options(scipen = 999, digits = 3)

library(readxl)
data <- read_excel("Data1.xlsx")
str(data)

data <- as.data.frame(data)

data2 <- data 

table(data$happiness)

lista <- list()
cont=0
for (i in 4) {
  cont=cont+1
  lista[[cont]]<-which(data[,i]==99)
  data2[lista[[cont]],i]<-NA
}
all.equal(data, data2)

table(data2$happiness)

data3 <- data2

library(dplyr)
data3 <- data3 %>% mutate(sex = case_when(sex == 1 ~ "M",
                                          sex == 2 ~ "F"))
table(data3$sex)

library(dplyr)
data3 <- data3 %>% mutate(fulfill = case_when(fulfill == 1 ~ 4,
                                              fulfill == 2 ~ 3,
                                              fulfill == 3 ~ 2,
                                              fulfill == 4 ~ 1,
                                              fulfill == 5 ~ 0))
table(data3$fulfill)

data3 <- data3 %>% mutate(sat_life = case_when(sat_life == 1 ~ 5,
                                               sat_life == 2 ~ 4,
                                               sat_life == 4 ~ 2,
                                               sat_life == 5 ~ 1,
                                               TRUE  ~ as.numeric(sat_life)))
table(data3$sat_life)

data3 <- data3 %>% mutate(likelyunemp = case_when(likelyunemp == 1 ~ 4,
                                                  likelyunemp == 2 ~ 3,
                                                  likelyunemp == 3 ~ 2,
                                                  likelyunemp == 4 ~ 1,
                                                  TRUE  ~ as.numeric(likelyunemp)))
table(data3$likelyunemp)

data3 <- data3 %>% mutate(kibo = case_when(kibo == 1 ~ 4,
                                           kibo == 2 ~ 3,
                                           kibo == 3 ~ 2,
                                           kibo == 4 ~ 1,
                                           TRUE  ~ as.numeric(kibo)))
table(data3$kibo)

library(dplyr)
data3 <- data3 %>% mutate(happiness_rec = case_when( happiness == 0 ~ 0,
                                                     happiness == 1 ~ 0,
                                                     happiness == 2 ~ 0,
                                                     happiness == 3 ~ 1,
                                                     happiness == 4 ~ 1,
                                                     happiness == 5 ~ 2,
                                                     happiness == 6 ~ 3,
                                                     happiness == 7 ~ 3,
                                                     happiness == 8 ~ 4,
                                                     happiness == 9 ~ 4,
                                                     happiness == 10 ~ 4))

table(data3$happiness_rec)

library(dplyr)
data_description_num <- data3 %>%
                          select(year, panelid) %>%
                          group_by(year) %>%
                          dplyr::summarise(n=n()) 
data_description_num

library(dplyr)
data_descriptionfreq_rec <- data3 %>%
                               group_by(year, happiness_rec) %>% 
                               dplyr::summarise(n = n()) %>%
                               mutate(freq = n / sum(n))
data_descriptionfreq_rec

library(ggplot2)
ggplot(data_descriptionfreq_rec,
       aes(x=year,
           y=freq,
           group=as.factor(happiness_rec))) +
  geom_point( aes( color=as.factor(happiness_rec) )) +
  geom_line( aes( color=as.factor(happiness_rec) )) + 
  labs(title = "Frequenza relativa di felicità negli anni",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        )

data3.1 <- data3[,c(2,3,19)]
str(data3.1)

table(data3.1$happiness_rec)

library(reshape)
data3.1Wide <- reshape(data3.1, 
                          idvar="panelid", 
                          timevar="year", 
                          v.names="happiness_rec", 
                          direction="wide")
str(data3.1Wide)

data3.2Wide <- data3.1Wide[,-1]

data3.2Wide <- data3.2Wide %>% 
                mutate(happiness_rec.2014 = NA, happiness_rec.2015 = NA)
str(data3.2Wide)

data3.2Wide <- cbind(data3.2Wide[1:11],data3.2Wide[15:16],data3.2Wide[12:14]) 
str(data3.2Wide)

library(longCatEDA)
lasagnaplot1 <-longCat(data3.2Wide,
                       times = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                       tLabels = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                   2012, 2013, 2014, 2015,2016, 2017, 2018)
                       )

lasagnaplot1_sort <- sorter(lasagnaplot1,
                            ascending = FALSE)

data3.2Wide_sort <- lasagnaplot1_sort$y.sorted 
data3.2Wide_sort <- as.data.frame(data3.2Wide_sort)

library(RColorBrewer)
display.brewer.pal(n = 5, name = 'Spectral')
pal1 <- brewer.pal(n = 5, name = 'Spectral')

longCatPlot(lasagnaplot1, 
            xlab="Anno", 
            cols=pal1,
            ylab="Id Partecipante",
            reverse=TRUE,
            legendBuffer=0.2,
            main="Felicità negli anni",
            sort=TRUE)

data3.2Wide_sort %>%
  filter(is.na(happiness_rec.2003)&is.na(happiness_rec.2004)&is.na(happiness_rec.2005)&
           is.na(happiness_rec.2006)&is.na(happiness_rec.2007)&is.na(happiness_rec.2008)&
           is.na(happiness_rec.2009)&is.na(happiness_rec.2010)&is.na(happiness_rec.2011)&
           is.na(happiness_rec.2012)&is.na(happiness_rec.2013)&is.na(happiness_rec.2014)&
           is.na(happiness_rec.2015)&is.na(happiness_rec.2016)&is.na(happiness_rec.2017)&
           is.na(happiness_rec.2018)) %>%
  summarise(n_miss=n())

library(dplyr)
library(tidyverse)
data3.2Wide_sort %>%
  drop_na(happiness_rec.2003) %>%
  drop_na(happiness_rec.2004) %>%
  drop_na(happiness_rec.2005) %>%
  drop_na(happiness_rec.2006) %>%
  drop_na(happiness_rec.2007) %>%
  drop_na(happiness_rec.2008) %>%
  drop_na(happiness_rec.2009) %>%
  drop_na(happiness_rec.2010) %>%
  drop_na(happiness_rec.2011) %>%
  drop_na(happiness_rec.2012) %>%
  drop_na(happiness_rec.2013) %>%
  # drop_na(happiness_rec.2014) 
  # drop_na(happiness_rec.2015) 
  drop_na(happiness_rec.2016) %>%
  drop_na(happiness_rec.2017) %>%
  drop_na(happiness_rec.2018) %>%
  summarise(n_nomiss = n())

data3.2Wide_sort_blu <- data3.2Wide_sort[1:10,-c(12,13)]
data3.2Wide_sort_blu

library(dplyr)
data3.1Wide %>%
  filter(happiness_rec.2003==4&happiness_rec.2004==4&happiness_rec.2005==4&happiness_rec.2006==4&
           happiness_rec.2007==4&happiness_rec.2008==4&happiness_rec.2009==4&happiness_rec.2010==4&
           happiness_rec.2011==4&happiness_rec.2012==4&happiness_rec.2013==4&happiness_rec.2016==4&

data3 %>%
  select(panelid, year, happiness, fulfill, sat_life, sex, marstatus, age, educ, empstatus, income) %>%
  filter(panelid==306|panelid==515)

data3.1Wide %>%
  filter(happiness_rec.2003==5&happiness_rec.2004==5&happiness_rec.2005==5&happiness_rec.2006==5&
           happiness_rec.2007==5&happiness_rec.2008==5&happiness_rec.2009==5&happiness_rec.2010==5&
           is.na(happiness_rec.2011)&happiness_rec.2012==5&happiness_rec.2013==5&happiness_rec.2016==5&
           happiness_rec.2017==5&happiness_rec.2018==5)

data3.1Wide %>%
  filter(happiness_rec.2003==5&happiness_rec.2004==5&happiness_rec.2005==5&happiness_rec.2006==5&
           happiness_rec.2007==5&happiness_rec.2008==5&happiness_rec.2009==5&happiness_rec.2010==5&
           happiness_rec.2011==5&happiness_rec.2012==5&happiness_rec.2013==5&happiness_rec.2016==5&
           happiness_rec.2017==5&is.na(happiness_rec.2018))

data3 %>%
  select(panelid, year, happiness, fulfill, sat_life, sex, marstatus, age, educ, empstatus, income) %>%
  filter(panelid==924)

data3.1Wide %>%
  filter(is.na(happiness_rec.2003)&happiness_rec.2004==5&happiness_rec.2005==5&happiness_rec.2006==5&
           happiness_rec.2007==5&happiness_rec.2008==5&happiness_rec.2009==5&happiness_rec.2010==5&
           happiness_rec.2011==5&happiness_rec.2012==5&happiness_rec.2013==5&happiness_rec.2016==5&
           happiness_rec.2017==5&happiness_rec.2018==5)

data3 %>%
  select(panelid, year, happiness, fulfill, sat_life, sex, marstatus, age, educ, empstatus, income) %>%
  filter(panelid==2115|panelid==2814|panelid==4038|panelid==4129|panelid==4410|panelid==4578)

data3.2Wide_sort_rosso <- data3.2Wide_sort %>%
                            filter(happiness_rec.2003==1 & happiness_rec.2004==1)
data3.2Wide_sort_rosso

data3.1Wide %>%
  filter(happiness_rec.2003==1&happiness_rec.2004==1&happiness_rec.2005==4)

data3 %>%
  select(panelid, year, happiness, fulfill, sat_life, sex, marstatus, age, educ, empstatus, income) %>%
  filter(panelid==678)

data3.1Wide %>%
  filter(happiness_rec.2003==1&happiness_rec.2004==1&happiness_rec.2005==2)

data3 %>%
  select(panelid, year, happiness, fulfill, sat_life, sex, marstatus, age, educ, empstatus, income) %>%
  filter(panelid==679|panelid==349|panelid==711)

library(dplyr)
data_descriptionfreq <- data3 %>%
                          group_by(year, happiness) %>% 
                          dplyr::summarise(n = n()) %>%
                          mutate(freq = n / sum(n))
data_descriptionfreq

ggplot(data_descriptionfreq,
       aes(x=year,
           y=freq,
           group=as.factor(happiness))) +
  geom_point( aes( color=as.factor(happiness) )) +
  geom_line( aes( color=as.factor(happiness) )) + 
  labs(title = "Frequenza relativa di felicità negli anni",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")))


table(data3$fulfill) # dato mancante codificato con 9
table(data3$sat_life) # dato mancante codificato con 9
table(data3$sex) # nessun dato mancante diversamente codificato da NA
table(data3$marstatus) # dato mancante codificato con 9
table(data3$ages) # nessun dato mancante diversamente codificato da NA
table(data3$educ) # dato mancante codificato con 99
table(data3$empstatus) # dato mancante codificato con 9
table(data3$income) # dato mancante codificato con 99
table(data3$likelyunemp) # dato mancante codificato con 8 e 9
table(data3$tiiki) # nessun dato mancante diversamente codificato con NA
table(data3$kibo) # nessun dato mancante diversamente codificato con NA
table(data3$numfam) # dato mancante codificato con 99

data4 <- data3
ind <- list()
cont = 0
for(i in c(5,6,8,12,15)){
  cont=cont+1
  ind[[cont]]<-which(data3[,i]==9 | data3[,i]==8)
  data4[ind[[cont]],i]<-NA
}
all.equal(data4,data3)

data5 <- data4
ind <- list()
cont = 0
for(i in c(11,13,18)){
  cont=cont+1
  ind[[cont]]<-which(data4[,i]==99)
  data5[ind[[cont]],i]<-NA
}
all.equal(data5,data4)

library(funModeling)
library(dplyr)
status1 = df_status(data5, print_results = F) 
status1 %>% arrange(-p_na, type)

library(naniar)
library(ggplot2)
vis_miss(data5[,-c(1,2,3,14,19)], sort_miss = TRUE) + 
  labs(title= "Dati mancanti per variabile") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))

library(ExPanDaR)
library(ggplot2)
prepare_missing_values_graph(data5[,-c(1,2,14,19)], ts_id = "year")  +
  labs(title = "Dati mancanti negli anni") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))

library(naniar)
gg_miss_upset(data5[,c(4,6,10,13,15)], nsets =6) 

library(tsibble)
library(brolgar)
data5tsib_camp <- as_tsibble(x = data5,
                         key = panelid,
                         index = year,
                         regular = FALSE)

length(unique(data5tsib_camp$panelid))

library(brolgar)
set.seed(1234)
data5_camp <- data5tsib_camp %>%
                  sample_n_keys(size = 100)

data5_camp <- as.data.frame(data5_camp)
str(data5_camp)

data5 <- data5 %>%
           group_by(year) %>%
           mutate(max_peso = max(swght_b), 
           peso_fin = swght_b/max_peso ) 

data5 <- as.data.frame(data5)
str(data5)

library(tsibble)
library(brolgar)
data5tsib_pes <- as_tsibble(x = data5,
                        key = panelid,
                        index = year,
                        regular = FALSE)

library(brolgar)
set.seed(123)
data5tsib_camppes <- data5tsib_pes %>%
                      sample_n_keys(size = 10)

length(unique(data5tsib_camppes$panelid))
unique(data5tsib_camppes$panelid)

data5tsib_camppes <- as.data.frame(data5tsib_camppes)

library(dplyr)
data5_pescam <- data5tsib_camppes %>%
            group_by(panelid, year) %>%
            select(panelid, year, happiness_rec, peso_fin) 

library(ggplot2)
ggplot(data5_pescam,
       aes(x = year,
           y = peso_fin,
           group = as.factor(panelid))) +
  geom_point( aes( color=as.factor(panelid) )) +
  geom_line( aes( color=as.factor(panelid) )) +
  labs(title = "Pesi per 10 soggetti negli anni",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic"))

data5_2003 <- data5 %>%
                filter(year==2003) %>%
                select(happiness_rec, peso_fin) 
                # pesi per id che non hanno happiness mancante nel 2003
library(weights)
data5_2003_freq <- round(wpct(data5_2003$happiness_rec, weight = data5_2003$peso_fin),2)
data5_2003_freq

data5_2004 <- data5 %>%
                filter(year==2004) %>%
                select(happiness_rec, peso_fin) 
                # pesi per id che non hanno happiness mancante nel 2004
data5_2004_freq <- round(wpct(data5_2004$happiness_rec, weight = data5_2004$peso_fin),2)
data5_2004_freq

data5_2005 <- data5 %>%
                filter(year==2005) %>%
                select(happiness_rec, peso_fin) 
                # pesi per id che non hanno happiness mancante nel 2005
data5_2005_freq <- round(wpct(data5_2005$happiness_rec, weight = data5_2005$peso_fin),2)
data5_2005_freq

data5_2006 <- data5 %>%
                filter(year==2006) %>%
                select(happiness_rec, peso_fin)    
data5_2006_freq <- round(wpct(data5_2006$happiness_rec, weight = data5_2006$peso_fin),2)
data5_2006_freq

data5_2007 <- data5 %>%
                filter(year==2007) %>%
                select(happiness_rec, peso_fin)    
data5_2007_freq <- round(wpct(data5_2007$happiness_rec, weight = data5_2007$peso_fin),2)
data5_2007_freq

data5_2008 <- data5 %>%
                filter(year==2008) %>%
                select(happiness_rec, peso_fin)    
data5_2008_freq <- round(wpct(data5_2008$happiness_rec, weight = data5_2008$peso_fin),2)
data5_2008_freq

data5_2009 <- data5 %>%
                filter(year==2009) %>%
                select(happiness_rec, peso_fin)    
data5_2009_freq <- round(wpct(data5_2009$happiness_rec, weight = data5_2009$peso_fin),2)
data5_2009_freq

data5_2010 <- data5 %>%
                filter(year==2010) %>%
                select(happiness_rec, peso_fin)    
data5_2010_freq <- round(wpct(data5_2010$happiness_rec, weight = data5_2010$peso_fin),2)
data5_2010_freq

data5_2011 <- data5 %>%
                filter(year==2011) %>%
                select(happiness_rec, peso_fin)    
data5_2011_freq <- round(wpct(data5_2011$happiness_rec, weight = data5_2011$peso_fin),2)
data5_2011_freq

data5_2012 <- data5 %>%
                filter(year==2012) %>%
                select(happiness_rec, peso_fin)    
data5_2012_freq <- round(wpct(data5_2012$happiness_rec, weight = data5_2012$peso_fin),2)
data5_2012_freq

data5_2013 <- data5 %>%
                filter(year==2013) %>%
                select(happiness_rec, peso_fin)    
data5_2013_freq <- round(wpct(data5_2013$happiness_rec, weight = data5_2013$peso_fin),2)
data5_2013_freq

data5_2016 <- data5 %>%
                filter(year==2016) %>%
                select(happiness_rec, peso_fin)    
data5_2016_freq <- round(wpct(data5_2016$happiness_rec, weight = data5_2016$peso_fin),2)
data5_2016_freq

data5_2017 <- data5 %>%
                filter(year==2017) %>%
                select(happiness_rec, peso_fin)    
data5_2017_freq <- round(wpct(data5_2017$happiness_rec, weight = data5_2017$peso_fin),2)
data5_2017_freq

data5_2018 <- data5 %>%
                filter(year==2018) %>%
                select(happiness_rec, peso_fin)    
data5_2018_freq <- round(wpct(data5_2018$happiness_rec, weight = data5_2018$peso_fin),2)
data5_2018_freq

data_descriptionfreq_recpes <- rbind(data5_2003_freq, data5_2004_freq, data5_2005_freq, data5_2006_freq,
                                     data5_2007_freq, data5_2008_freq, data5_2009_freq, data5_2010_freq,
                                     data5_2011_freq, data5_2012_freq, data5_2013_freq, data5_2016_freq,
                                     data5_2017_freq, data5_2018_freq)
data_descriptionfreq_recpes <- t(data_descriptionfreq_recpes) 

data_descriptionfreq_recpes <- cbind(0:4, data_descriptionfreq_recpes)
data_descriptionfreq_recpes <- as.data.frame(data_descriptionfreq_recpes)
str(data_descriptionfreq_recpes)

colnames(data_descriptionfreq_recpes)[1] <- c("happiness_rec") 
colnames(data_descriptionfreq_recpes)[2:12] <- c(2003:2013)
colnames(data_descriptionfreq_recpes)[13:15] <- c(2016:2018)
str(data_descriptionfreq_recpes)

data_descriptionfreq_recpesW <- reshape(data_descriptionfreq_recpes, 
                                        direction = "long",
                                        varying = list(names(data_descriptionfreq_recpes)[2:15]),
                                        idvar = "happiness_rec",
                                        v.names = c("freqpes"),
                                        timevar = "year",
                                        times = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
                                                  2013, 2016, 2017, 2018))
str(data_descriptionfreq_recpesW)

data_descriptionfreq_rec <- as.data.frame(data_descriptionfreq_rec)

library(dplyr)
library(tidyverse)
data_descriptionfreq_rec2 <- data_descriptionfreq_rec
data_descriptionfreq_rec2 <- data_descriptionfreq_rec2 %>%
                              select(year, happiness_rec, freq) %>%
                              drop_na(happiness_rec) %>%
                              mutate(freq=round(freq,2))

data_descriptionfreq_recTot <- cbind(data_descriptionfreq_recpesW, data_descriptionfreq_rec2)
str(data_descriptionfreq_recTot)

data_descriptionfreq_recTot <- data_descriptionfreq_recTot[,-c(4,5)]
str(data_descriptionfreq_recTot)

library(reshape2)
data_descriptionfreq_recTot2 <- melt(data_descriptionfreq_recTot, 
                                     id = c("happiness_rec", "year"))
str(data_descriptionfreq_recTot2)

colnames(data_descriptionfreq_recTot2)[3] <- c("tipo_frequenza")
colnames(data_descriptionfreq_recTot2)[4] <- c("frequenza")

library(ggplot2)
ggplot(data_descriptionfreq_recTot2,
       aes(x = year,
           y = frequenza,
           group = as.factor(happiness_rec))) +
  geom_point( aes( color=as.factor(happiness_rec) )) +
  geom_line( aes( color=as.factor(happiness_rec) )) + 
  facet_wrap(vars(tipo_frequenza)) +
  labs(title = "Frequenza relativa di felicità negli anni pesate e non",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic"))

library(dplyr)
data_descriptionfreq_sex <- data5 %>%
                              select(panelid, year, happiness, sex) %>%
                              group_by(year, sex) %>%
                              dplyr::summarise(n = n()) %>%
                              mutate(freq = n / sum(n))
data_descriptionfreq_sex

par(mfrow=c(1,1))

library(ggplot2)
ggplot(data_descriptionfreq_sex,
       aes(x=year,
           y=freq,
           group=as.factor(sex))) +
  geom_point( aes( color=as.factor(sex) )) +
  geom_line( aes( color=as.factor(sex) )) + 
  labs(title = "Frequenza relativa di genere negli anni",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        )

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = sex)) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per genere negli anni", x = "Anni", y = "Genere") + 
  facet_wrap(vars(happiness_rec)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
library(dplyr)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness, 
             fill = sex)) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per genere negli anni", x = "Anni", y = "Genere") + 
  facet_wrap(vars(happiness)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

data5_sexM <- data5%>% 
  select(panelid, year, happiness_rec, sex) %>%
  filter(sex=="M")

data5_sexM <- as.data.frame(data5_sexM)
str(data5_sexM)

data5_sexM <- data5_sexM[,-4] 

data5_sexF <- data5%>%
                select(panelid, year, happiness_rec, sex) %>%
                filter(sex=="F")

data5_sexF <- as.data.frame(data5_sexF)
str(data5_sexF)

data5_sexF <- data5_sexF[,-4] 

library(reshape)
data5Wide_sexM <- reshape(data5_sexM, 
                           idvar="panelid", 
                           timevar="year", 
                           v.names="happiness_rec", 
                           direction="wide")
data5Wide_sexM <- data5Wide_sexM[,-1]

data5Wide_sexM <- data5Wide_sexM %>% 
                      mutate(happiness_rec.2014 = NA, happiness_rec.2015 = NA)
data5Wide_sexM <- cbind(data5Wide_sexM[1:11],data5Wide_sexM[15:16],data5Wide_sexM[12:14]) 
str(data5Wide_sexM)

data5Wide_sexF <- reshape(data5_sexF, 
                          idvar="panelid", 
                          timevar="year", 
                          v.names="happiness_rec", 
                          direction="wide")
data5Wide_sexF <- data5Wide_sexF[,-1]

data5Wide_sexF <- data5Wide_sexF %>% 
                      mutate(happiness_rec.2014 = NA, happiness_rec.2015 = NA)
data5Wide_sexF <- cbind(data5Wide_sexF[1:11],data5Wide_sexF[15:16],data5Wide_sexF[12:14]) 
str(data5Wide_sexF)

library(longCatEDA)
lasagnaplot5_sexM <-longCat(data5Wide_sexM,
                       times = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                       tLabels = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                   2012, 2013, 2014, 2015,2016, 2017, 2018))

library(longCatEDA)
lasagnaplot5_sexF <-longCat(data5Wide_sexF,
                            times = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                      2012, 2013, 2014, 2015, 2016, 2017, 2018),
                            tLabels = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                        2012, 2013, 2014, 2015,2016, 2017, 2018))

par(mfrow=c(1,2))

library(longCatEDA)
longCatPlot(lasagnaplot5_sexM, 
            xlab="Anno", 
            cols=pal1,
            ylab="Id Partecipante",
            reverse=TRUE,
            lcex = 0.5,
            main="Felicità nel tempo (uomo)",
            sort=TRUE)

longCatPlot(lasagnaplot5_sexF, 
            xlab="Anno", 
            cols=pal1,
            ylab="Id Partecipante",
            reverse=TRUE,
            lcex = 0.5,
            main="Felicità nel tempo (donna)")

tab_sexhap <- table(data5$happiness_rec, data5$sex)
            
chisq.test(tab_sexhap)

library(dplyr)
data_descriptionfreq_sat <- data5 %>%
                              select(panelid, year, happiness, sat_life) %>%
                              filter(year==2012|year==2013|year==2016|year==2017|year==2018) %>%
                              group_by(year, sat_life) %>%
                              dplyr::summarise(n = n()) %>%
                              mutate(freq = n / sum(n))

par(mfrow=c(1,1))

library(ggplot2)
ggplot(data_descriptionfreq_sat,
       aes(x=year,
           y=freq,
           group=as.factor(sat_life))) +
  geom_point( aes( color=as.factor(sat_life) )) +
  geom_line( aes( color=as.factor(sat_life) )) + 
  labs(title = "Frequenza relativa di soddisfazione nella vita dal 2012 al 2018",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        )
}
library(ggplot2)
data5 %>% 
  filter(year==2012|year==2013|year==2016|year==2017|year==2018) %>%
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = as.factor(sat_life))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per Soddisfazione nella vita negli anni", 
       x = "Anno", 
       y = "Soddisfazione nella vita") + 
  facet_wrap(vars(happiness_rec)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  filter(year==2012|year==2013|year==2016|year==2017|year==2018) %>%
  ggplot(aes(x = year, 
             y = happiness, 
             fill = as.factor(sat_life))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per Soddisfazione nella vita negli anni", 
       x = "Anno", 
       y = "Soddisfazione nella vita") + 
  facet_wrap(vars(happiness)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(dplyr)
data5_sat2 <- data5 %>%
                filter(year==2012|year==2013|year==2016|year==2017|year==2018)

tab_sathap <- table(data5_sat2$happiness_rec, data5_sat2$sat_life)

chisq.test(tab_sathap)

data_descriptionfreq_ful <- data5 %>%
                              select(panelid, year, happiness, fulfill) %>%
                              group_by(year, fulfill) %>%
                              dplyr::summarise(n = n()) %>%
                              mutate(freq = n / sum(n))
data_descriptionfreq_ful

par(mfrow=c(1,1))

library(ggplot2)
ggplot(data_descriptionfreq_ful,
       aes(x=year,
           y=freq,
           group=as.factor(fulfill))) +
  geom_point( aes( color=as.factor(fulfill) )) +
  geom_line( aes( color=as.factor(fulfill) )) + 
  labs(title = "Frequenza relativa di senso di appagamento nella vita negli anni",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        )

data5_ful <- data5[,c(2,3,5)]
data5_ful <- as.data.frame(data5_ful)
str(data5_ful)

data5_fulWide <- reshape(data5_ful, 
                         idvar="panelid", 
                         timevar="year", 
                         v.names="fulfill", 
                         direction="wide")
library(dplyr)
data5_fulWide <- data5_fulWide %>% 
                mutate(fulfill.2014 = NA, fulfill.2015 = NA)
str(data5_fulWide)

data5_fulWide <- data5_fulWide[,-1]
# str(data5_fulWide)

data5_fulWide <- cbind(data5_fulWide[1:11],data5_fulWide[15:16],data5_fulWide[12:14]) 
str(data5_fulWide)

library(longCatEDA)
lasagnaplot5_ful <-longCat(data5_fulWide,
                       times = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                       tLabels = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                   2012, 2013, 2014, 2015,2016, 2017, 2018)
                       )


longCatPlot(lasagnaplot5_ful, 
            xlab="Anno", 
            cols=pal1,
            ylab="Id Partecipante",
            reverse=TRUE,
            legendBuffer=0.2,
            main="Senso di appagamento della vita negli anni",
            sort=TRUE)

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = as.factor(fulfill))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per Senso di appagamento negli anni", x = "Anno", y = "Senso di appagamento") + 
  facet_wrap(vars(happiness_rec)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness, 
             fill = as.factor(fulfill))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per Senso di appagamento negli anni", x = "Anno", y = "Senso di appagamento") + 
  facet_wrap(vars(happiness)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

tab_hapful <- table(data5$happiness_rec, data5$fulfill)
tab_hapful

chisq.test(tab_hapful)

data_descriptionfreq_mar <- data5 %>%
                              select(panelid, year, happiness, marstatus) %>%
                              group_by(year, marstatus) %>%
                              dplyr::summarise(n = n()) %>%
                              mutate(freq = n / sum(n))
data_descriptionfreq_mar

data5_mar <- data5[,c(2,3,8)]
data5_mar <- as.data.frame(data5_mar)
str(data5_mar)

data5_marWide <- reshape(data5_mar, 
                         idvar="panelid", 
                         timevar="year", 
                         v.names="marstatus", 
                         direction="wide")
data5_marWide <- data5_marWide %>% 
                    mutate(marstatus.2014 = NA, marstatus.2015 = NA)
data5_marWide <- data5_marWide[,-1]
str(data5_marWide)

data5_marWide <- cbind(data5_marWide[1:11],data5_marWide[15:16],data5_marWide[12:14]) 
str(data5_marWide)

library(longCatEDA)
lasagnaplot5_mar <-longCat(data5_marWide,
                       times = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                       tLabels = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                   2012, 2013, 2014, 2015,2016, 2017, 2018)
                       )


longCatPlot(lasagnaplot5_mar, 
            xlab="Anno", 
            cols=pal1,
            ylab="Id Partecipante",
            reverse=TRUE,
            legendBuffer=0.2,
            main="Stato matrimoniale negli anni",
            sort=TRUE)

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = as.factor(marstatus))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per stato matrimoniale negli anni", x = "Anno", y = "Stato matrimoniale") + 
  facet_wrap(vars(happiness_rec)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness, 
             fill = as.factor(marstatus))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per stato matrimoniale negli anni", x = "Anno", y = "Stato matrimoniale") + 
  facet_wrap(vars(happiness)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = marstatus, 
             fill = as.factor(happiness_rec))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per stato matrimoniale negli anni", x = "Anno", y = "Stato matrimoniale") + 
  facet_wrap(vars(marstatus)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

tab_hapmar <- table(data5$happiness_rec, data5$marstatus)
tab_hapmar

chisq.test(tab_hapmar)

data5_age <- data5[,c(2, 3, 10, 19)]

library(dplyr)
data5_ageNA <- data5 %>%
                select(year, age) %>%
                filter(is.na(age))%>%
                group_by(year) %>%
                summarise(n=n())
data5_ageNA

data5 %>%
  select(year,age) %>%
  filter(year==2003) %>%
  summarise(n=n())

data5 %>%
  select(year,age) %>%
  filter(is.na(age) & year==2003) %>%
  summarise(n=n())

data5 %>%
  select(year,age) %>%
  filter(year==2004) %>%
  summarise(n=n())

data5 %>%
  select(year,age) %>%
  filter(is.na(age) & year==2004) %>%
  summarise(n=n())

library(tidyverse)
data5_ageNOmissing <- data5_age %>%
                        drop_na(age)

table(data5_ageNOmissing$year)

library(psych)
data_description_age <- describeBy(data5_ageNOmissing, group="year", mat=TRUE)
data_description_age <- data_description_age[c(25:36),]
data_description_age

library(ExPanDaR)
graphtrend_age <- prepare_trend_graph(data5_ageNOmissing[c("year", "age")], "year")
graphtrend_age$plot

library(RColorBrewer)
display.brewer.pal(n = 7, name = 'Oranges')
brewer.pal(n = 7, name = 'Oranges')

library(ggplot2)
library(dplyr)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = age)) +
  geom_bar(stat = "identity",
           aes(group=age)) +
  labs(title = "Felicità per età negli anni", x = "Year", y = "Age") + 
  facet_wrap(vars(happiness_rec)) +
  scale_fill_gradient(na.value = 'white', 
                      low = "#FDD0A2", high = "#8C2D04") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

data5 %>% 
  ggplot(aes(x = year, 
             y = happiness, 
             fill = age)) +
  geom_bar(stat = "identity", 
           aes(group=age)) +
  labs(title = "Felicità per età neggli anni", x = "Year", y = "Age") + 
  facet_wrap(vars(happiness)) +
  scale_fill_gradient(na.value = 'white', 
                      low = "#FDD0A2", high = "#8C2D04") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

data_descriptionfreq_ages <- data5 %>%
                                select(panelid, year, happiness, ages) %>%
                                group_by(year, ages) %>%
                                dplyr::summarise(n = n()) %>%
                                mutate(freq = n / sum(n))
data_descriptionfreq_ages

library(ggplot2)
ggplot(data_descriptionfreq_ages,
       aes(x=year,
           y=freq,
           group=as.factor(ages))) +
  geom_point( aes( color=as.factor(ages) )) +
  geom_line( aes( color=as.factor(ages) )) + 
  labs(title = "Frequenza relativa di classe d'età negli anni",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        )

library(ggplot2)
library(dplyr)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = as.factor(ages))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per classe d'età negli anni", x = "Anno", y = "Classe d'età") + 
  facet_wrap(vars(happiness_rec)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

data5 %>% 
  ggplot(aes(x = year, 
             y = happiness, 
             fill = as.factor(ages))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per classe d'età negli anni", x = "Anno", y = "Classe d'età") + 
  facet_wrap(vars(happiness)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

tab_hapages <- table(data5$happiness_rec, data5$ages)
tab_hapages

chisq.test(tab_hapages)

data_descriptionfreq_educ <- data5 %>%
                                select(panelid, year, happiness, educ) %>%
                                group_by(year, educ) %>%
                                dplyr::summarise(n = n()) %>%
                                mutate(freq = n / sum(n))
data_descriptionfreq_educ

library(ggplot2)
library(dplyr)
ggplot(data_descriptionfreq_educ,
       aes(x=year,
           y=freq,
           group=as.factor(educ))) +
  geom_point( aes( color=as.factor(educ) )) +
  geom_line( aes( color=as.factor(educ) )) + 
  labs(title = "Frequenza relativa del grado di educazione negli anni",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        )

data5_educ <- data5[,c(2,3,11)]
data5_educ <- as.data.frame(data5_educ)
str(data5_educ)

data5_educWide <- reshape(data5_educ, 
                           idvar="panelid", 
                           timevar="year", 
                           v.names="educ", 
                           direction="wide")
data5_educWide <- data5_educWide %>% 
                    mutate(educ.2014 = NA, educ.2015 = NA)
data5_educWide <- data5_educWide[,-1]
str(data5_educWide)

library(RColorBrewer)
pal3 <- brewer.pal(n = 4, name = 'Spectral')

data5_educWide <- cbind(data5_educWide[1:11],data5_educWide[15:16],data5_educWide[12:14]) 
str(data5_educWide)

library(longCatEDA)
lasagnaplot5_educ <-longCat(data5_educWide,
                       times = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                       tLabels = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                   2012, 2013, 2014, 2015,2016, 2017, 2018)
                       )


longCatPlot(lasagnaplot5_educ, 
            xlab="Anno", 
            cols=pal1,
            ylab="Id Partecipante",
            reverse=TRUE,
            legendBuffer=0.2,
            main="Livello massimo di educazione della vita negli anni",
            sort=TRUE)

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = as.factor(educ))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per livello di educazione negli anni", 
       x = "Anno", 
       y = "Livello di educazione") + 
  facet_wrap(vars(happiness_rec)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness, 
             fill = as.factor(educ))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per livello di educazione negli anni", 
       x = "Anno", 
       y = "Livello di educazione")+ 
  facet_wrap(vars(happiness)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

tab_hapeduc <- table(data5$happiness_rec, data5$educ)
tab_hapeduc

chisq.test(tab_hapeduc)

data_descriptionfreq_emp <- data5 %>%
                              select(panelid, year, happiness, empstatus) %>%
                              group_by(year, empstatus) %>%
                              dplyr::summarise(n = n()) %>%
                              mutate(freq = n / sum(n))  
data_descriptionfreq_emp

library(ggplot2)
library(dplyr)
ggplot(data_descriptionfreq_emp,
       aes(x=year,
           y=freq,
           group=as.factor(empstatus))) +
  geom_point( aes( color=as.factor(empstatus) )) +
  geom_line( aes( color=as.factor(empstatus) )) + 
  labs(title = "Frequenza relativa dello stato lavorativo negli anni",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        )

data5_emp <- data5[,c(2,3,12)]
data5_emp <- as.data.frame(data5_emp)
str(data5_emp)

data5_empWide <- reshape(data5_emp, 
                         idvar="panelid", 
                         timevar="year", 
                         v.names="empstatus", 
                         direction="wide")
data5_empWide <- data5_empWide %>% 
                    mutate(empstatus.2014 = NA, empstatus.2015 = NA)
data5_empWide <- data5_empWide[,-1]
str(data5_empWide)

data5_empWide <- cbind(data5_empWide[1:11],data5_empWide[15:16],data5_empWide[12:14]) 
str(data5_empWide)

library(longCatEDA)
lasagnaplot5_emp <-longCat(data5_empWide,
                       times = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                       tLabels = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                   2012, 2013, 2014, 2015,2016, 2017, 2018)
                       )

longCatPlot(lasagnaplot5_emp, 
            xlab="Anno", 
            cols=pal1,
            ylab="Id Partecipante",
            reverse=TRUE,
            legendBuffer=0.2,
            main="Stato lavorativo negli anni",
            sort=TRUE)

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = as.factor(empstatus))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità negli anni per stato occupazionale", x = "Anno", y = "Stato occupazionale") + 
  facet_wrap(vars(happiness_rec)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness, 
             fill = as.factor(empstatus))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità negli anni per stato occupazionale", x = "Anno", y = "Stato occupazionale")  +            facet_wrap(vars(happiness)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = empstatus, 
             fill = as.factor(happiness_rec))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità negli anni per stato occupazionale", x = "Anno", y = "Stato occupazionale")  +
  facet_wrap(vars(empstatus)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  filter(empstatus==4) %>%
  ggplot(aes(x = year, 
             y = empstatus, 
             fill = as.factor(happiness_rec))) +
  geom_bar(stat = "identity") +
  labs(title = "Grado di felicità dei soggetti disoccupati negli anni", x = "Anno") + 
  facet_wrap(vars(empstatus)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

tab_hapemp <- table(data5$happiness_rec, data5$empstatus)
tab_hapemp

chisq.test(tab_hapemp)

library(dplyr)
data_descriptionfreq_inc <- data5 %>%
                              select(panelid, year, happiness, income) %>%
                              group_by(year, income) %>%
                              dplyr::summarise(n = n()) %>%
                              mutate(freq = n / sum(n))
data_descriptionfreq_inc

library(ggplot2)
library(dplyr)
ggplot(data_descriptionfreq_inc,
       aes(x=year,
           y=freq,
           group=as.factor(income))) +
  geom_point( aes( color=as.factor(income) )) +
  geom_line( aes( color=as.factor(income) )) + 
  labs(title = "Frequenza relativa di reddito negli anni",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        ) 

library(dplyr)
data5 %>%
  select(panelid, income, year) %>%
  filter((year==2017|year==2018)&income==4) %>%
  
  summarise(n=n())

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = as.factor(income))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per reddito negli anni", x = "Anno", y = "Reddito") + 
  facet_wrap(vars(happiness_rec)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness, 
             fill = as.factor(income))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per Reddito negli anni", x = "Anno", y = "Reddito") + 
  facet_wrap(vars(happiness)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = income, 
             fill = as.factor(happiness_rec))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per Reddito negli anni", x = "Anno", y = "Reddito") +  
  facet_wrap(vars(income)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

tab_hapinc <- table(data5$happiness_rec, data5$income)
tab_hapinc

chisq.test(tab_hapinc)

data_descriptionfreq_lunemp <- data5 %>%
                                select(panelid, year, happiness, likelyunemp) %>%
                                group_by(year, likelyunemp) %>%
                                dplyr::summarise(n = n()) %>%
                                mutate(freq = n / sum(n))  
data_descriptionfreq_lunemp

library(ggplot2)
ggplot(data_descriptionfreq_lunemp,
       aes(x=year,
           y=freq,
           group=as.factor(likelyunemp))) +
  geom_point( aes( color=as.factor(likelyunemp) )) +
  geom_line( aes( color=as.factor(likelyunemp) )) + 
  labs(title = "Frequenza relativa possibilità di perdita del lavoro nel tempo",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        )

data5_lunemp <- data5[,c(2,3,15)]
data5_lunemp <- as.data.frame(data5_lunemp)
str(data5_lunemp)

data5_lunempWide <- reshape(data5_lunemp, 
                             idvar="panelid", 
                             timevar="year", 
                             v.names="likelyunemp", 
                             direction="wide")
data5_lunempWide <- as.data.frame(data5_lunempWide)
data5_lunempWide <- data5_lunempWide[,-1]

data5_lunempWide <- data5_lunempWide %>% 
                       mutate(likelyunemp.2014 = NA, likelyunemp.2015 = NA)
str(data5_lunempWide)

data5_lunempWide <- cbind(data5_lunempWide[1:11],data5_lunempWide[15:16],data5_lunempWide[12:14]) 
str(data5_lunempWide)

library(longCatEDA)
lasagnaplot5_lunemp <-longCat(data5_lunempWide,
                       times = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                       tLabels = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                   2012, 2013, 2014, 2015,2016, 2017, 2018)
                       )

library(longCatEDA)
longCatPlot(lasagnaplot5_lunemp, 
            xlab="Anno", 
            cols=pal3,
            ylab="Id Partecipante",
            reverse=TRUE,
            legendBuffer=0.2,
            main="Possibilità dei soggetti di essere disoccupati nei prossimi due anni",
            sort=TRUE)

library(ggplot2)
library(dplyr)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = as.factor(likelyunemp))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per possibilità di perdere il lavoro nei prossimi anni", 
       x = "Anno", y = "Possibilità di disoccupazione") + facet_wrap(vars(happiness_rec)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness, 
             fill = as.factor(likelyunemp))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità per possibilità di perdere il lavoro nei prossimi anni", 
       x = "Anno", y = "Possibilità di disoccupazione") + facet_wrap(vars(happiness)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  filter(likelyunemp==4) %>%
  ggplot(aes(x = year, 
             y = likelyunemp, 
             fill = as.factor(happiness_rec))) +
  geom_bar(stat = "identity") +
  labs(title = "Grado di felicità dei soggetti per likelyunemp=4", x = "Anno") + 
  facet_wrap(vars(likelyunemp)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

tab_haplunemp <- table(data5$happiness_rec, data5$likelyunemp)
tab_haplunemp

chisq.test(tab_haplunemp)

library(dplyr)
data_descriptionfreq_kib <- data5 %>%
                              select(panelid, year, happiness, kibo) %>%
                              group_by(year, kibo) %>%
                              dplyr::summarise(n = n()) %>%
                              mutate(freq = n / sum(n))
data_descriptionfreq_kib

library(ggplot2)
ggplot(data_descriptionfreq_kib,
       aes(x=year,
           y=freq,
           group=as.factor(kibo))) +
  geom_point( aes( color=as.factor(kibo) )) +
  geom_line( aes( color=as.factor(kibo) )) + 
  labs(title = "Frequenza relativa della dimensione del luogo di residenza",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        )

data5_kib <- data5[,c(2,3,17)]
data5_kib <- as.data.frame(data5_kib)
str(data5_kib)

data5_kibWide <- reshape(data5_kib, 
                             idvar="panelid", 
                             timevar="year", 
                             v.names="kibo", 
                             direction="wide")
data5_kibWide <- as.data.frame(data5_kibWide)
data5_kibWide <- data5_kibWide[,-1]

data5_kibWide <- data5_kibWide %>% 
                       mutate(kibo.2014 = NA, kibo.2015 = NA)
str(data5_kibWide)

data5_kibWide <- cbind(data5_kibWide[1:11],data5_kibWide[15:16],data5_kibWide[12:14]) 
str(data5_kibWide)

library(longCatEDA)
lasagnaplot5_kib <-longCat(data5_kibWide,
                       times = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                       tLabels = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
                                   2012, 2013, 2014, 2015,2016, 2017, 2018)
                       )

library(longCatEDA)
longCatPlot(lasagnaplot5_kib, 
            xlab="Anno", 
            cols=pal1,
            ylab="Id Partecipante",
            reverse=TRUE,
            legendBuffer=0.2,
            main="Dimensioni del luogo di residenza negli anni",
            sort=TRUE)

library(ggplot2)
library(dplyr)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = as.factor(kibo))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità negli anni per dimensioni del luogo di residenza", 
       x = "Anno", y = "dimensioni del luogo di residenza negli anni") + 
  facet_wrap(vars(happiness_rec)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness, 
             fill = as.factor(kibo))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità negli anni per dimensioni del luogo di residenza", 
       x = "Anno", y = "dimensioni del luogo di residenza negli anni") + 
  facet_wrap(vars(happiness)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

tab_hapkib <- table(data5$happiness_rec, data5$kibo)
tab_hapkib

chisq.test(tab_hapkib)

library(dplyr)
data_descriptionfreq_tii <- data5 %>%
                              select(panelid, year, happiness, tiiki) %>%
                              group_by(year, tiiki) %>%
                              dplyr::summarise(n = n()) %>%
                              mutate(freq = n / sum(n))
data_descriptionfreq_tii

library(ggplot2)
ggplot(data_descriptionfreq_tii,
       aes(x=year,
           y=freq,
           group=as.factor(tiiki))) +
  geom_point( aes( color=as.factor(tiiki) )) +
  geom_line( aes( color=as.factor(tiiki) )) + 
  labs(title = "Frequenza relativa di regione di residenza in Giappone",
       caption = "NB: dati mancanti per 2014 e 2015",
       x = "Anno", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        )

library(ggplot2)
library(dplyr)
data5 %>% 
  ggplot(aes(x = year, 
             y = tiiki, 
             fill = as.factor(happiness_rec))) +
  geom_bar(stat = "identity") +
  labs(title = "Felicità negli anni per regione di residenza", 
       x = "Anno", y = "per regione di residenza") + 
  facet_wrap(vars(tiiki)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

tab_haptii <- table(data5$happiness_rec, data5$tiiki)
tab_haptii

chisq.test(tab_haptii)

table(data5$numfam)

library(dplyr)
data5_nfam <- data5 %>% 
                  select(panelid, year, numfam)

library(psych)
data_description_nfam <- describeBy(data5_nfam, group="year", mat=TRUE)
data_description_nfam <- data_description_nfam[c(29:42),]
data_description_nfam

library(ExPanDaR)
graphtrend_nfam <- prepare_trend_graph(data5_nfam[c("numfam", "year")], "year")
graphtrend_nfam$plot

library(ggplot2)
library(dplyr)
data5 %>% 
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = numfam)) +
  geom_bar(stat = "identity",
           aes(group=numfam)) +
  labs(title = "Felicità per numero di componenti in famiglia nel tempo", 
       x = "Anno", 
       y = "Numero di componenti in famiglia") + 
  facet_wrap(vars(happiness_rec)) +
  scale_fill_gradient(na.value = 'white', 
                      low = "#FDD0A2", high = "#8C2D04") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))

library(ggplot2)
data5 %>% 
  filter(happiness_rec==1) %>%
  ggplot(aes(x = year, 
             y = happiness_rec, 
             fill = numfam)) +
  geom_bar(stat = "identity",
           aes(group=numfam)) +
  labs(title = "Numero di componenti in famiglia nel tempo per soggetti infelici", 
       x = "Anno", 
       y = "Numero di componenti in famiglia") + 
  facet_wrap(vars(happiness_rec)) +
  scale_fill_gradient(na.value = 'white', 
                      low = "#FDD0A2", high = "#8C2D04") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    legend.key = element_rect(colour = "white", fill = NA))




