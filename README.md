## Analisi Descrittiva e Grafica dei Dati Longitudinali: Caratteristiche e Specificità
### Relatore: Prof.ssa Fulvia Pennoni
#### Tesi di laurea di Veronica Morelli nel corso di Statistica e Gestione delle Informazioni

Nel file *1 analisi descrittive e grafiche longitudinali univariate.R* vengono presentati i codici di R utilizzati per svolgere le analisi descrittive e grafiche presentate nel capitolo 6 della tesi. Si analizzano 3 dataset tra cui:
1. Dataset: ***data_criminal_sim***
Il dataset data_criminal_sim è disposto in formato long contiene 60.000 osservazioni simulate secondo una struttura che richiama i dati reali provenienti dal Regno 
Unito, in particolare dall’offenders index, un set di dati di ricerca dell’Home Office. Nei dati simulati si dispone delle relative alle condanne complete di una coorte di delinquenti seguiti dall’età della responsabilità penale, 10 anni, fino a 40 anni e include anche la proporzione di non 
delinquenti. Si considerano T=6 fasce d’età di ampiezza pari a 5 anni. Per ogni fascia d’età si dispone di una variabile binaria pari a 1 se il soggetto è stato condannato per un reato delle seguenti 10 tipologie e a 0 altrimenti. In questo caso si seleziona la variabile *y1* relativa al crimine di violenza contro le persone.
2. Dataset: ***RLMSdat***
Il dataset è presente nella libreria *LMest* in formato wide e contiene dei dati reali derivanti da una ricerca condotta da Higher School of Economics and ZAO "Demoscope" insieme al Carolina Population Center. L’indagine denominata Russia Longitudinal Monitoring Survey (RMLS) comprende anche delle domande sulla soddisfazione lavorativa 
misurata da una variabile ordinale in sette diverse occasioni con 5 categorie.
3. Dataset: ***PSIDlong***
Il dataset PSIDlong è presente nella libreria *LMest* in formato long e contiene dati provenienti dal Panel Study of Income Dynamics (PSID): l’indagine longitudinali più lunga al 32 mondo.  Lo studio è iniziato nel 1968 con un campione rappresentativo a livello nazionale di oltre 18.000 individui che vivono in 5.000 famiglie negli Stati Uniti. In particolare si selezionano le informazioni relative al reddito. 
Per questi 3 dataset si effettuano delle analisi descrittive, si produce un lasagna plot e si analizzano le caratteristiche dei soggetti nel tempo.

Nel file *2 analisi descrittive e grafiche multivariate dataset happiness.R* vengono presentati i codici di R utilizzati per svolgere le analisi descrittive e grafiche presentate nel capitolo 6 della tesi. 

Il dataset che viene analizzato proviene da uno studio condotto dal programma COE dell’Università di Osaka nel 21esimo secolo dal nome ‘Behavioral Macro-Dynamics Based 
on Surveys and Experiments’ (‘Macrodinamica comportamentale basata su indagini ed esperimenti’).
L’obiettivo dell’analisi è esplorare possibili relazioni tra grado di felicità e tutte le restanti covariate: si vuole valutare l’influenza che fattori sociali, demografici, economici, ambientali e lavorativi ecc. possono avere sulla felicità. 
Le variabili sono: 
- *panelid* (identificativo del soggetto), *year* (anno in cui si svolge l'indagine), *happiness* (grado di felicità a 10 livelli), *fulfill* (senso di appagamente della vita a 5 livelli), *sat_life* (soddisfazione nella vita a 5 livelli), *sex* (genere), *marstatus* (stato matrimoniale), *ages* (classe d'età), *age* (età), *educ* (livello di educazione), *empstatus* (stato lavorativo), *income* (reddito), *swght_b* (pesi campionari), *kibo* (dimensione demografica della città di residenza), *tiiki* (regione di residenza), *likelyunemp* (possibilità di esser disoccupati) e *numfam* (numero di componenti in famiglia).
Nel file si presentano i codici per ricodificare le variabili, analizzare i dati mancanti, produrre i lasagna plot, effetturare le statistiche descrittive, valutare con grafici la relazione tra grado di felicità e tutte le variabili esplicative. Inoltre è presente il codice che mostra il calcolo delle frequenze relative della variabile *happiness* che vengono confrontate con quelle non pesate. 
