## “Descriptive and Graphic Analysis of Longitudinal Data: Characteristics and Specificity”.
### Supervisor: Prof.ssa Fulvia Pennoni
#### Bachelor Thesis in Statistics and Information Management

File *1 longitudinal univariate.R* descriptive and graphical analyses presents the R codes used to carry out the descriptive and graphical analyses presented in Chapter 6 of the thesis. Three datasets are analyzed including:
1. Dataset: ***data_criminal_sim***
The dataset data_criminal_sim is arranged in long format contains 60,000 observations simulated according to a structure that recalls real data from the UK 
United Kingdom, specifically from the offenders index, a Home Office research dataset. In the simulated data, one has the relative full convictions of a cohort of offenders followed by the age of criminal responsibility, 10 years, up to 40 years, and also includes the proportion of non 
delinquents. T=6 age groups of 5-year breadth are considered. For each age group we have a binary variable equal to 1 if the person has been convicted of a crime of the following 10 types and 0 otherwise. In this case, the variable *y1* related to the crime of violence against persons is selected.
2. Dataset: ***RLMSdat***
The dataset is in the *LMest* library in wide format and contains real data from a survey conducted by Higher School of Economics and ZAO "Demoscope" together with the Carolina Population Center. The survey called the Russia Longitudinal Monitoring Survey (RMLS) also includes questions on job satisfaction 
measured by an ordinal variable on seven different occasions with 5 categories.
3. Dataset: ***PSIDlong***
The PSIDlong dataset is in the *LMest* library in long format and contains data from the Panel Study of Income Dynamics (PSID): the longest longitudinal survey in 32 the world.  The study began in 1968 with a nationally representative sample of over 18,000 individuals living in 5,000 households in the United States. In particular, income information is selected. 
Descriptive analyses are performed for these 3 datasets, a lasagna plot is produced, and subject characteristics are analyzed over time.

The file *2 descriptive and graphical analyses multivariate dataset happiness.R* presents the R codes used to perform the descriptive and graphical analyses presented in Chapter 6 of the thesis.  

The dataset being analyzed is from a study conducted by Osaka University's COE program in the 21st century named 'Behavioral Macro-Dynamics Based 
on Surveys and Experiments' ('Behavioral Macrodynamics Based on Surveys and Experiments').
The aim of the analysis is to explore possible relationships between degree of happiness and all the remaining covariates: it aims to assess the influence that social, demographic, economic, environmental and work factors, etc., may have on happiness. 
The variables are: 
*panelid* (subject identifier), *year* (year in which the survey takes place), *happiness* (degree of happiness at 10 levels), *fulfillment* (sense of life fulfillment at 5 levels), *sat_life* (life satisfaction at 5 levels), *sex* (gender), *marriage status), *ages* (age group), *age* (age), *educ* (level of education), *empstatus* (employment status), *income* (income), *swght_b* (sample weights), *kibo* (demographic size of city of residence), *tiiki* (region of residence), *likelyunemp* (possibility of being unemployed), and *numfam* (number of household members).
Codes are presented in the file to recode variables, analyze missing data, produce lasagna plots, carry out descriptive statistics, and evaluate with graphs the relationship between degree of happiness and all explanatory variables. Also presented is the code showing the calculation of the relative frequencies of the *happiness* variable that are compared with the unweighted ones. 
