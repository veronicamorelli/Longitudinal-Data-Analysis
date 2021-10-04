install.packages("LMest")
require(LMest)
?LMest
data(package = "LMest")
data(data_criminal_sim)
data(RLMSdat)
data(PSIDlong)

# data_criminal_sim 
# long --> wide

criminal <- data_criminal_sim[,c(1, 3, 4)]


# RLMSdat
# wide --> long


# PSIDlong
# tenere in long

PSID <- PSIDlong[,c(1, 2 ,13)]
