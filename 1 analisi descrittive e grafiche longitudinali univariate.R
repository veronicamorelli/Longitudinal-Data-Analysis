library(LMest)
data(package = "LMest")

Data sets in package ‘LMest’:

- PSIDlong                           Dataset about income dynamics

- RLMSdat                            Dataset about job satisfaction

- data_criminal_sim                  Criminal dataset

data(data_criminal_sim)
data(RLMSdat)
data(PSIDlong)

## Dataset: data_criminal_sim


criminalLong <- data_criminal_sim
criminalLongdf <- as.data.frame(criminalLong)


library(funModeling)
library(dplyr)
status1 =df_status(criminalLongdf, print_results = F) 
status1

criminalLong2 <- criminalLongdf[,c(1, 3, 4)]
str(criminalLong2)

library(reshape)
criminalWidedf <- reshape(criminalLong2, 
                          idvar="id", 
                          timevar="time", 
                          v.names="y1", 
                          direction="wide")
str(criminalWidedf)
head(criminalWidedf)

criminalLongdf_description <- criminalLong2 %>% 
                                group_by(time, y1) %>%
                                summarise(n = n())  %>%
                                mutate(freq = n / sum(n))
criminalLongdf_description

library(ggplot2)
ggplot(criminalLongdf_description,
       aes(x=time,
           y=freq,
           group=as.factor(y1))) +
  geom_point( aes( color=as.factor(y1) )) +
  geom_line( aes( color=as.factor(y1) )) + 
  labs(title = "Frequenza relativa di crimine contro le persone negli anni",
       x = "Anni", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        )

library(longCatEDA)
lasagnaplot1 <-longCat(criminalWidedf_noid)
colors <- c("#ABDDA4", "#2B83BA")
longCatPlot(lasagnaplot1, 
            xlab="Fasce di 5 anni",
            ylab="Id Partecipante",
            main="Violenza contro le persone nel tempo", 
            cols=colors,
            legendBuffer = 0.2,
            sort=TRUE)

rlmsWide <- RLMSdat
rlmsWidedf <- as.data.frame(rlmsWide)
str(rlmsWidedf)

library(funModeling)
library(dplyr)
status2 =df_status(rlmsWidedf, print_results = F) 
status2

id <- 1:1718
rlmsWidedf2 <- cbind(id, rlmsWidedf)
str(rlmsWidedf2)

rlmsWidedf2$IKSJQ <- recode(rlmsWidedf2$IKSJQ, 
                            "1"=5, "2"=4, "3"=3, "4"=2, "5"=1)
rlmsWidedf2$IKSJR <- recode(rlmsWidedf2$IKSJR, 
                            "1"=5, "2"=4, "3"=3, "4"=2, "5"=1)
rlmsWidedf2$IKSJS <- recode(rlmsWidedf2$IKSJS, 
                            "1"=5, "2"=4, "3"=3, "4"=2, "5"=1)
rlmsWidedf2$IKSJT <- recode(rlmsWidedf2$IKSJT, 
                            "1"=5, "2"=4, "3"=3, "4"=2, "5"=1)
rlmsWidedf2$IKSJU <- recode(rlmsWidedf2$IKSJU, 
                            "1"=5, "2"=4, "3"=3, "4"=2, "5"=1)
rlmsWidedf2$IKSJV <- recode(rlmsWidedf2$IKSJV, 
                            "1"=5, "2"=4, "3"=3, "4"=2, "5"=1)
rlmsWidedf2$IKSJW <- recode(rlmsWidedf2$IKSJW, 
                            "1"=5, "2"=4, "3"=3, "4"=2, "5"=1)

rlmslong <- reshape(rlmsWidedf2, 
                    direction = "long",
                    varying = list(names(rlmsWidedf2)[2:8]),
                    v.names = c("Job_Satisfaction"),
                    idvar = "id",
                    timevar = "Occasion",
                    times = c("IKSJQ", "IKSJR", "IKSJS", "IKSJT","IKSJU", "IKSJV", "IKSJW"))
str(rlmslong)
head(rlmslong)

rlmslong_descriptionfreq <- rlmslong %>%
                              group_by(Occasion, Job_Satisfaction) %>% 
                              dplyr::summarise(n = n()) %>%
                              mutate(freq = n / sum(n))
                              
rlmslong_descriptionfreq

library(ggplot2)
ggplot(rlmslong_descriptionfreq,
       aes(x=Occasion,
           y=freq,
           group=as.factor(Job_Satisfaction))) +
  geom_point( aes( color=as.factor(Job_Satisfaction) )) +
  geom_line( aes( color=as.factor(Job_Satisfaction) )) + 
  labs(title = "Frequenza relativa di soddisfazione lavorativa negli anni",
       x = "Occasione", 
       y = "Frequenza relativa") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 1, size = 12, face = "italic")
        )

rlmsWidedf3 <- rlmsWidedf2[,-1]
str(rlmsWidedf3)

library(longCatEDA)
library(RColorBrewer)

# display.brewer.pal(n = 5, name = 'Spectral')
pal3 <- brewer.pal(n = 5, name = 'Spectral')

lasagnaplot2 <-longCat(rlmsWidedf3)
longCatPlot(lasagnaplot2, 
            xlab="Occasione", cols=pal3,
            ylab="Id Partecipante",
            reverse=TRUE,
            legendBuffer=0.2,
            main="Soddisfazione lavorativa nel tempo",
            sort=TRUE)

psidLong <- PSIDlong
psidLongdf <- as.data.frame(psidLong)
str(psidLongdf)

psidLongdf2 <- psidLongdf[,c(1,2,13)]
str(psidLongdf2)

colnames(psidLongdf2)[3] <- "income"
head(psidLongdf2)

library(funModeling)
status3 =df_status(psidLongdf2, print_results = F) 
status3

library(dplyr)
psidLongdf2 %>%
  select(id, time, income) %>%
  group_by(id) %>%
  filter(income==0) 

library(dplyr)
psidlong_nullincome <- psidLongdf2 %>%
                        select(id, time, income) %>%
                        group_by(id) %>%
                        filter(income==0) %>%
                        summarise(count=n()) 

psidlong_nullincome %>%
  select(id, count) %>%
  filter(count==7)

library(psych)
psidlong_description <- describeBy(psidLongdf2, group="time", mat=TRUE)
psidlong_description <- psidlong_description[c(15:21),]
psidlong_description

library(ExPanDaR)
graphtrend_income <- prepare_trend_graph(psidLongdf2[c("time", "income")], "time")
graphtrend_income$plot

library(tsibble)
library(brolgar)
psidlongtsib <- as_tsibble(x = psidLongdf2,
                          key = id,
                          index = time,
                          regular = FALSE)

set.seed(123)
psidlongtsib_camp <- psidlongtsib %>%
                      sample_n_keys(size = 100)

psidlongdf_camp <- as.data.frame(psidlongtsib_camp)
str(psidlongdf_camp)

library(psych)
psidlong_description_camp <- describeBy(psidlongdf_camp, group="time", mat=TRUE)
psidlong_description_camp <- psidlong_description_camp[c(15:21),]
psidlong_description_camp

library(RColorBrewer)
# display.brewer.pal(n = 7, name = 'Greens')
pal4 <- brewer.pal(n = 7, name = 'Greens')
pal4

library(ggplot2)
ggplot(psidlongdf_camp, 
       aes(x = time, 
           y = as.factor(id), 
           fill =  income)) +
  geom_tile(colour = 'gray98') +
  scale_fill_gradient(na.value = 'white', 
                       low = "#C7E9C0", high = "#005A32") +
  scale_y_discrete(expand = c(0, 0), labels = NULL, 
                   breaks = NULL)  +
  labs(title = "Reddito nel tempo",
       x = "Tempo", 
       y = "Id Partecipante") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.background=element_rect(fill="white", color=NA),
    panel.background=element_rect(fill="white", color=NA),
    axis.text.x = element_text(size = 8.5),
    legend.key = element_rect(colour = "black", fill = NA),
  )

# library(dplyr)
psidlongdf_camp %>%
  select(id, time, income) %>%
  filter(income>300)

psidlongdf_camp %>%
  select(id, time, income) %>%
  filter(id==373)
