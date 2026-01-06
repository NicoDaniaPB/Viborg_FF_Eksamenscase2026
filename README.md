# Viborg_FF_Eksamenscase2026

# Hvad gør koden?
Formålet med denne koden, er at indsamle data for relevante variabler for VFFs
hjemmekampe, og analyserer dataene for at finde en brugbar machine-learning 
model. 

# Hvilke filer er der?
1. Webscraping-superstats
2. DMI-data.R
3. VFF hjemmekampe med helligedage og feriedage.R
4. VFF Transermarkt.R
5. Data-fra-Bjarne.r
6. SQLite.R
7. Modellering - Eksamensprojekt.R

VFF-projekt.bib
VFF-synopsis.qmd

Zipfil med VFF-data 

# Installeringskrav
Tidyverse, rvest, stringr, lubridate, janitor, RSQLite, httr, jsonlite, 
worlfootballR, forcats, ggplot2, glmnet, leaps, caret, purrr, car, tibble

# Hvor køres koden?
Åbn koden og kør den linje for linje i RStudio. Der er tekst undervejs, som 
beskriver hvad der foregår.

# Hvad gør koden?
Koden indsamler data fra diverse kilder, og analyserer disse data i form af 
nøgletal som R^2, MSE og RMSE, samt grafiske illustrationer.
