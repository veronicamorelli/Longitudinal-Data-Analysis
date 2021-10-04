install.packages("LMest")
install.packages("tidyverse")
install.packages("funModeling")
install.packages("reshape")
install.packages("carData")
install.packages("tibble")
install.packages("devtools")
install.packages("dyplr")
install.packages("tsibble")
install.packages("brolgar")
install.packages("gghighlight")
install.packages("ExPanDaR")
install.packages("kableExtra")
install.packages("lcsm")
install.packages("longCatEDA")


library(brolgar)
library(devtools)                                                                                                                                
install_github("swihart/lasagnar")                                                               
library(fields)
library(lasagnar)   
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(colorspace)  

require(LMest)
?LMest

data(package = "LMest")

# Data sets in package 'LMest':
# PSIDlong                           Dataset about income dynamics
# RLMSdat                            Dataset about job satisfaction
# data_criminal_sim                  Criminal dataset

data(data_criminal_sim)
data(RLMSdat)
data(PSIDlong)


#### data_criminal_sim (vedi descrizione file word) ####

criminalLong <- data_criminal_sim
dim(criminalLong) #60000x13
str(criminalLong)
head(criminalLong)

criminalLongdf <- as.data.frame(criminalLong)
str(criminalLongdf)

library(funModeling)
library(dplyr)
status1 =df_status(criminalLongdf, print_results = F) 
status1
head(status1%>% arrange(-p_na,type))
# nessun dato mancante
# y1 dummy: violence against people

criminalLong2 <- criminalLongdf[,c(1, 3, 4)]
str(criminalLong2)

# long-->wide
library(reshape)
criminalWidedf <- reshape(criminalLong2, 
                          idvar="id", 
                          timevar="time", 
                          v.names="y1", 
                          direction="wide")
str(criminalWidedf)

# Analisi descrittive

library(funModeling)
status3 =df_status(criminalWidedf, print_results = F) 
status3

criminallong3 <- criminalLong2 %>%
                  group_by(time, y1) %>%
                  summarise(n = n())  %>%
                  mutate(freq = n / sum(n))

rlmslong3 <- rlmslong2 %>%
                  group_by(Occasion, Job_Satisfaction) %>%
                  summarise(n = n()) %>%
                  mutate(freq = n / sum(n))
rlmslong3
  

# lasagna plot
head(criminalWidedf)
str(criminalWidedf)
criminalWidedf$id <- as.factor(criminalWidedf$id)
table(criminalWidedf$y1.1)

library(longCatEDA)
criminalWidedf_noid <- criminalWidedf[,-1]
head(criminalWidedf)
lc4<-longCat(criminalWidedf_noid)
longCatPlot(lc4, 
            xlab="Years by 5",
            ylab="Id",
            main="Crime against people through time", 
            cols=colors,
            legendBuffer = 0.2,
            sort=TRUE)

colors <- c("#CCEBC5" , "#E7298A")



#### RLMSdat (vedi descrizione file word) #### 

     
rlmsWide <- RLMSdat
rlmsWidedf <- as.data.frame(rlmsWide)
dim(rlmsWidedf) # 1718 x 7
str(rlmsWidedf)
head(rlmsWidedf)

library(funModeling)
status2 =df_status(rlmsWidedf, print_results = F) 
status2

id <- 1:1718
rlmsWidedf2 <- cbind(id, rlmsWidedf)
str(rlmsWidedf2)

# Nel dataset in formato long ci dovranno essere 1718x7 osservazioni = 12026. 
# Ci dovranno essere 3 colonne: "id", "Occasion", "Job_Satisfaction". Dove "Occasion" ha 7 diversi livelli e 
# "Job_Satisfaction" ha 5 livelli

# wide --> long
rlmslong <- reshape(rlmsWidedf2, 
                    direction = "long",
                    varying = list(names(rlmsWidedf2)[2:8]),
                    v.names = c("Job_Satisfaction"),
                    idvar = "id",
                    timevar = "Occasion",
                    times = c("IKSJQ", "IKSJR", "IKSJS", "IKSJT","IKSJU", "IKSJV", "IKSJW"))
summary(rlmslong)          
str(rlmslong)
table(rlmslong$Occasion) # nessun dato mancante

table(rlmslong$Job_Satisfaction)


## Analisi descrittive

# Statistiche descrittive 
library(ExPanDaR)
library(kableExtra)
t <- prepare_descriptive_table(rlmslong)
t$kable_ret  %>%
  kable_styling("condensed", full_width = F, position = "center")


# violin plot
library(vioplot)
ret <- prepare_by_group_violin_graph(rlmslong, by_var = "Occasion", var = "Job_Satisfaction",
                                     order_by_mean = TRUE)
ret
ret$mapping

#box-plot
library(ggplot2)
ggplot(rlmslong, aes(x=Occasion, y=id, fill=Job_Satisfaction)) + geom_boxplot()

ggplot(rlmslong, aes(Occasion, Job_Satisfaction)) +
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.colour = "blue") +
  theme_classic()

library(psych)
rlmslong_description <- describeBy(rlmslong, group="Occasion", mat=TRUE)
rlmslong_description <- rlmslong_description[c(15:21),]

# punteggio globale di job_statisfaction 
library(ggplot2)
require(dplyr)

rlmslong %>%
  ggplot(aes(x = Occasion, y = Job_Satisfaction, fill=Job_Satisfaction)) + 
  geom_bar(stat = "identity") + 
  # geom_bar() makes the height of the bar proportional to the number of cases in each group
  facet_grid(Job_Satisfaction ~ .) + 
  labs(title = "Job Satisfaction by Occasion", x = "Occasion", y = "Job Satisfaction")

# Si può notare come per tutte le diverse occasioni il numero di punteggi attribuiti alla soddisfazione lavorativa
# rimangono costanti. C'è una leggera diminuzione nel tempo di chi ha una bassa soddisfazione lavorativa
# (punteggi 4 e 5). Si nota come il punteggio 2 (mostly satisfied) sia maggiormente preferito. Come spesso
# accade i punteggi estremi sono meno scelti. Un'alta quantità di soggetti ha comunque scelto il punteggio nella
# categoria media. 

# Spaghetti plot
# Non va bene (vedi lasagna plot in fondo)
p <- ggplot(data = rlmslong, aes(x = Occasion, y = Job_Satisfaction, group = id))
p + geom_point() + geom_line()


# tibble
str(rlmslong)
rlmslong$Occasion <- as.factor(rlmslong$Occasion)
str(rlmslong)
rlmslong$Occasion <- as.integer(rlmslong$Occasion)
# IKSJQ = 1
# IKSJR = 2
# IKSJS = 3
# IKSJT = 4
# IKSJU = 5
# IKSJV = 6
# IKSJW = 7
str(rlmslong)
library(tsibble)
rlmslongtib <- as_tsibble(x = rlmslong,
                      key = id,
                      index = Occasion,
                      regular = FALSE)
rlmslongtib

library(brolgar)
library(ggplot2)


# Diverse figure in cui vengono selezionati casualmente delle osservazioni
set.seed(1234)
ggplot(rlmslongtib,
       aes(x = Occasion,
           y = Job_Satisfaction,
           group = id)) +
  geom_line() +
  facet_sample(2)


# Trend Graph di Job Satisfaction nel tempo
library(ExPanDaR)
graph <- prepare_trend_graph(rlmslong[c("Occasion", "Job_Satisfaction")], "Occasion")
graph$plot


# Lasagna plot
str(rlmslong)
ggplot(rlmslong,aes(x=Occasion,y=as.factor(id),fill=Job_Satisfaction)) + geom_tile(colour='black') 

require(ggplot2)


# Funziona
rlmslong22<-rlmslong[c(1:10, 1719:1728, 3437:3446),]
str(rlmslong22)
rlmslong22$Occasion<- as.factor(rlmslong22$Occasion)
str(rlmslong22)
ggplot(rlmslong22,aes(x=Occasion,y=id,fill=Job_Satisfaction)) + geom_tile(colour='black') 

# Perchè non funziona??
str(rlmslong)
rlmslong$Occasion <- as.factor(rlmslong$Occasion)
str(rlmslong)
ggplot(rlmslong,aes(x=Occasion,y=id,fill=Job_Satisfaction)) + geom_tile(colour='black')
# non funziona perchè ci sono troppe osservazioni

rlmslong23<-rlmslong[c(1:100),]
str(rlmslong23)
rlmslong23$Occasion<- as.factor(rlmslong23$Occasion)
str(rlmslong23)
ggplot(rlmslong23,aes(x=Occasion,y=id,fill=Job_Satisfaction)) + geom_tile(colour='black')

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5000555/ 
# Sorta di lasagna plot ma con dati in formato Wide

example("longCat")
example("longCatPlot")

library(RColorBrewer)
display.brewer.pal(n = 5, name = 'PiYG')
pal1 <- brewer.pal(n = 5, name = 'PiYG')
pal1

display.brewer.pal(n = 7, name = 'Greens')
pal2 <- brewer.pal(n = 7, name = 'Greens')
pal2

display.brewer.pal(n = 5, name = 'Spectral')
pal3 <- brewer.pal(n = 5, name = 'Spectral')
pal3

display.brewer.pal(n = 5, name = 'Pastel1')
pal4 <- brewer.pal(n = 5, name = 'Pastel1')
pal4

display.brewer.pal(n = 7, name = 'Oranges')
pal5 <- brewer.pal(n = 7, name = 'Oranges')
pal5

library(longCatEDA)
lc<-longCat(rlmsWide)
longCatPlot(lc, 
            xlab="Occasione", cols=pal3,
            ylab="Id Partecipante",
            reverse=TRUE,
            legendBuffer=0.2,
            main="Job Satisfaction through time",
            sort=TRUE)

summary(rlmsWide)


# statistiche descrittive


#### PSIDlong (vedi descrizione file word) ####


psidLong <- PSIDlong
psidLongdf <- as.data.frame(psidLong)
dim(psidLongdf) # 10122 x 13
str(psidLongdf)
head(psidLongdf)

psidLongdf2 <- psidLongdf[,c(1,2,13)]
head(psidLongdf2)
table(psidLongdf2$time) # 1446 osservazioni per 7 volte
str(psidLongdf2)

colnames(psidLongdf2)[3] <- "income"
head(psidLongdf2)

## Analisi descrittive

# Statistiche descrittive 
library(ExPanDaR)
library(kableExtra)
t2 <- prepare_descriptive_table(psidLongdf2)
t2$kable_ret  %>%
  kable_styling("condensed", full_width = F, position = "center")

library(funModeling)
status3 =df_status(psidLongdf2, print_results = F) 
status3 
# 2.5 % di soggetti che hanno 0 reddito (potrebbe essere un dato mancante perchè non è stato dichiarato?)

# violin plot
library(vioplot)
ret2 <- prepare_by_group_violin_graph(psidLongdf2, by_var = "time", var = "income",
                                     order_by_mean = TRUE)
ret2

str(psidLongdf2)
psidLongdf3 <- psidLongdf2
psidLongdf3$time <- as.factor(psidLongdf3$time)

library(ggplot2)
p2 <- ggplot(psidLongdf3, aes(x=time, y=income, color=time)) + 
  geom_violin(trim=FALSE)  
p2 + stat_summary(fun=median, geom="point", size=2, color="red") + coord_flip()


# trend income by time
library(ExPanDaR)
graphtrend_income <- prepare_trend_graph(psidLongdf3[c("time", "income")], "time")
graphtrend_income$plot
graphtrend_income$df

prepare_scatter_plot(psidLongdf3, x="time", y="income", size="income", color="income")

ggplot(data = psidLongdf3, aes(time, income)) +
  geom_line(color = "pink",size = 1) +
  geom_point(color="black") 

ggplot(data = psidLongdf3, mapping = aes(x = time, y = income)) +
  geom_boxplot(alpha = 0)   + geom_jitter(alpha = 0.1, color = "tomato")




# spaghetti plot
library(ggplot2)
p <- ggplot(data = psidLongdf3, aes(x = time, y = income, group = id))
p + geom_line()

# analisi con pacchetto brolgar
library(tsibble)
library(brolgar)

str(psidLongdf2)
# rlmslong$Occasion <- as.integer(rlmslong$Occasion)

psidlongtib <- as_tsibble(x = psidLongdf2,
                          key = id,
                          index = time,
                          regular = FALSE)
psidlongtib

psidlongdf5 <- as.data.frame(psidlongtib)

str(psidlongtib)
str(psidlongdf5)

# campionamento di osservazioni
set.seed(1234)
psidlongtib %>%
  sample_n_keys(size = 10) %>%
  ggplot(aes(x = time,
             y = income,
             group = id)) + 
  geom_line() +
  facet_sample()

# stessa cosa ma in più finestre
set.seed(1234)
ggplot(psidlongtib,
       aes(x = time,
           y = income,
           group = id)) +
  geom_line() +
  facet_sample()

# calcolo della variabile monotonicità che indica l'andamendo dell'income per le unità negli anni

psidlongtib2 <- psidlongtib

psidlongtib2 %>%
  sample_n_keys(size = 500)

psidlong_mono2 <- psidlongtib2 %>%
                    sample_n_keys(size = 500) %>%
                    features(income, feat_monotonic) %>%
                    left_join(psidlongtib, by = "id")

library(gghighlight)
ggplot(psidlong_mono2,
       aes(x = time,
           y = income,
           group = id)) +
  geom_line() + 
  gghighlight(unvary)

psidlong_mono$id <- as.factor(psidlong_mono$id)
library(gghighlight)
ggplot(psidlong_mono,
       aes(x = time,
           y = income,
           group = id)) +
  geom_line() + 
  gghighlight(unvary)

psidlong_mono2[(psidlong_mono2$id==530),]
psidlong_mono2[(psidlong_mono2$decrease==TRUE),]


# non va molto bene
#####


# Lasagna plot
# questo comando non funziona
library(ggplot2)
str(psidLongdf3)
ggplot(psidLongdf3[1:100,], aes(x = time, y = as.factor(id),fill =  income)) +   geom_tile(colour='black') 



ggplot(psidLongdf3[1:100,], 
       aes(x = time, 
           y = as.factor(id), 
           fill =  income))
+ 
  geom_tile(colour = 'gray98') +
  scale_fill_gradient(na.value = 'white', 
                      low = "pink", high = "red") +
  scale_x_discrete(expand = c(0, 0), labels = NULL, 
                   breaks = NULL) +    
  scale_y_discrete(expand = c(0, 0), labels = NULL, 
                   breaks = NULL)  +
  labs(x = " ", y = "Country") + 
  theme(
    plot.background=element_rect(fill="white", color=NA),
    panel.background=element_rect(fill="white", color=NA),
    axis.text.x = element_text(size = 8.5),
    legend.key = element_rect(colour = "white", fill = NA),
  )

# conversione a wide
library(reshape)
psidWidedf3 <- reshape(psidLongdf3, 
                          idvar="id", 
                          timevar="time", 
                          v.names="income", 
                          direction="wide")
# head(psidWidedf3)
# str(psidWidedf3)
# colnames(psidWidedf3)[c(2,3,4,5,6,7,8)] <- c("income_1987", "income_1988", "income_1989", "income_1990", 
#                                              "income_1991","income_1992", "income_1993")
head(psidWidedf3)
str(psidWidedf3)
psidWidedf3$id <- as.factor(psidWidedf3$id)

lc2<-longCat(psidWidedf3)
longCatPlot(lc2, xlab="Years")


library(longCatEDA)
psidWidedf3_noid <- psidWidedf3[,-1]
lc3<-longCat(psidWidedf3_noid)

longContPlot(psidWidedf3_noid, xlab="Years")

?longContPlot

# statistiche descrittive per dataset wide
summary(psidWidedf3_noid)

library(LMest)
stat <- lmestData(psidLongdf3, 
                  id="id",
                  time="time")
stat

        
   
