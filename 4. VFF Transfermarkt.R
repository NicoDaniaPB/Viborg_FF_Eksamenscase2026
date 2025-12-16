#> Kør kun installation hvis du ikke allerede har installeret devtools
#>indlæs pakken fra github ved hjælp af "devtools"

install.packages("devtools")
devtools::install_github("JaseZiv/worldfootballR")

# Indlæsning af nødvendige pakker
pacman::p_load(worldfootballR, tidyverse, janitor)

# Mappen til at gemme data i.
if (!dir.exists("VFFdata")) dir.create("VFFdata")

## indhent holdets Transfermarkt URL som vi indhenter data fra.
tm_league_team_urls(country_name = "Denmark", start_year = 2002)

# Indhent transfer info på sæson niveau.
## det tager lidt tid da der er begrænset adgang.

Viborg_2002 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2002", transfer_window = "all" )
Viborg_2003 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2003", transfer_window = "all" )
Viborg_2004 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2004", transfer_window = "all" )
Viborg_2005 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2005", transfer_window = "all" )
Viborg_2006 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2006", transfer_window = "all" )
Viborg_2007 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2007", transfer_window = "all" )
Viborg_2008 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2008", transfer_window = "all" )
Viborg_2009 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2009", transfer_window = "all" )
Viborg_2010 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2010", transfer_window = "all" )
Viborg_2011 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2011", transfer_window = "all" )
Viborg_2012 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2012", transfer_window = "all" )
Viborg_2013 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2013", transfer_window = "all" )
Viborg_2014 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2014", transfer_window = "all" )
Viborg_2015 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2015", transfer_window = "all" )
Viborg_2016 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2016", transfer_window = "all" )
Viborg_2017 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2017", transfer_window = "all" )
Viborg_2018 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2018", transfer_window = "all" )
Viborg_2019 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2019", transfer_window = "all" )
Viborg_2020 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2020", transfer_window = "all" )
Viborg_2021 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2021", transfer_window = "all" )
Viborg_2022 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2022", transfer_window = "all" )
Viborg_2023 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2023", transfer_window = "all" )
Viborg_2024 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2024", transfer_window = "all" )
Viborg_2025 <- tm_team_transfers(team_url = "https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/2025", transfer_window = "all" )

# Her kan vi se alle transfervinduer på årsbasis enkeltvist - indsæt blot år du ønsker at se, eks. 2002.

view(Viborg_2002)

# Nu samler vi alle de enkeltvise transfer data sæt i en samlet liste og en samlet dataframe.

# Liste med alle data frames ovenfor.
list_of_transfers <- list(Viborg_2002, Viborg_2003, Viborg_2004, Viborg_2005,
                          Viborg_2006, Viborg_2007, Viborg_2008, Viborg_2009, 
                          Viborg_2010, Viborg_2011, Viborg_2012, Viborg_2013, 
                          Viborg_2014, Viborg_2015, Viborg_2016, Viborg_2017, 
                          Viborg_2018, Viborg_2019, Viborg_2020, Viborg_2021, 
                          Viborg_2022, Viborg_2023, Viborg_2024, Viborg_2025)

# Alle års transferdata samles i en data frame:

total_transfers <- rbind.data.frame(Viborg_2002, Viborg_2003, Viborg_2004, Viborg_2005,
                                    Viborg_2006, Viborg_2007, Viborg_2008, Viborg_2009, 
                                    Viborg_2010, Viborg_2011, Viborg_2012, Viborg_2013, 
                                    Viborg_2014, Viborg_2015, Viborg_2016, Viborg_2017, 
                                    Viborg_2018, Viborg_2019, Viborg_2020, Viborg_2021, 
                                    Viborg_2022, Viborg_2023, Viborg_2024, Viborg_2025)

# Vi tjekker lige at alle data frames for sæsonerne er samlet

View(list_of_transfers)
View(total_transfers)

# nu fjerner (filtrerer) vi de sæsoner ud hvor Viborg FF ikke har været i superligaen
# sæsoner der skal fjernes er: 08/09, 09/10, 11/12, 12/13, 14/15, 17/18, 18/19, 19/20, 20/21

# Sæsoner hvor Viborg IKKE var i Superligaen
fjern_season_id <- c(2008, 2009, 2011, 2012, 2014, 2017, 2018, 2019, 2020)

# Filtrer årene vi definerede ovenfor væk fra den samlede tabel
total_transfers_clean <- total_transfers |> 
  filter(!(season %in% fjern_season_id))

# Tjek at årene er blevet fjernet
View(total_transfers_clean)

#her kontrollerer vi blot at sæsonerne faktisk matcher (ens output & indhold i vores dataframes)
table(total_transfers$season)

# Herefter fjerner vi de kolloner vi har trukket ud, som vi ikke skal bruge ved hjælp af subset metoden
# det er følgende vi fjerner: 
# Country, player_url, transfer_notes, league_2, country_2, in_squad, appearances, goals, minutes_played & Transfer_notes"

total_transfers_clean_filtered <- subset(total_transfers_clean, select = -c(team_name, league, country, player_url, player_position,
                                                                            player_age, player_nationality, club_2, league_2,
                                                                            country_2, in_squad, appearances, goals, minutes_played, transfer_notes))

### nu vi har filtreret alle variabler fra vi ikke skal bruge, 
### omdøber vi "season" til "saeson" således det passer med andre 
### webscraping data sæt vi har lavet.

total_transfers_clean_filtered <- total_transfers_clean_filtered |> 
  clean_names() |> rename(saeson = season)

# her tjekker vi blot at ændringen er trådt i kraft og kolonnen har fået sit nye navn

view(total_transfers_clean_filtered)

### vi opretter ny kolonne med formattet YYYY / YYYY +1 som ønsket, så vi har 
### årene på sæsonbasis, istedet for årstal for start af sæsonen

total_transfers_clean_filtered <- total_transfers_clean_filtered |> 
  mutate(season_format = paste0(saeson, "/", as.numeric(saeson) + 1))

# nu fjerner vi de to kolonner vi ikke skal bruge da vi har det ønskede sæsonformat

total_transfers_clean_filtered <- subset(total_transfers_clean_filtered, select = -c(saeson))

# Vi tjekker lige at vi har fået fjernet "saeson" altså årstallet, så vi blot har sæson YYYY / YYYY istedet

view(total_transfers_clean_filtered)

## vi har nu de 6 kolonner som vi ønsker at beholde, i det korrekte format.
## nu begynder vi at bearbejde data for at få transfers på sæsonniveau
## nu filtrerer vi alle lånespillere fra vores transferoverblik

transfers <- total_transfers_clean_filtered %>% filter(is_loan == FALSE)

## Vi tjekker lige at vi har fået filtreret alle lånespillere fra

View(transfers)

## Nu skal vi have fjernet alle N/A værdier fra kolonnen "transfer_fee"

transfers <- transfers |>  
  filter(transfer_fee != "n/a")

## Vi tjekker lige at vi har fået fjernet alle N/A værdier

view(transfers)

## vi skal nu definere at "arrivals" er udgifter
## og at "departures" er indtægter

## nu laver vi et samlet overblik over transferbalancer fra 2000/2001 sæsonen til og med 2025/2026 sæsonen

total_all <- transfers |> 
  mutate(
    transfer_fee = as.numeric(transfer_fee)   # sikrer numeriske værdier
  ) |> 
  filter(season_format >= "2000/2001",
         season_format <= "2025/2026") |> 
  group_by(season_format) |> 
  summarise(
    total_income = sum(if_else(transfer_type == "Departures",
                               transfer_fee, 0),
                       na.rm = TRUE),
    
    total_expense = sum(if_else(transfer_type == "Arrivals",
                                transfer_fee, 0),
                        na.rm = TRUE),
    
    net_balance = total_income - total_expense
  ) |> 
  arrange(season_format)

### Nu tilføjer vi en kolonne med "antal transfers i sæsonen"
### DET BURDE IKKE PÅVIRKE RESULTATET AF NEDENSTÅENDE, om man fjerne N/A værdier eller ej.

total_all_2 <- transfers |> 
  mutate(
    transfer_fee = as.numeric(transfer_fee)   # sikrer numeriske værdier
  ) |> 
  filter(season_format >= "2000/2001",
         season_format <= "2025/2026") |> 
  group_by(season_format) |> 
  summarise(
    total_income = sum(if_else(transfer_type == "Departures",
                               transfer_fee, 0), na.rm = TRUE),
    
    total_expense = sum(if_else(transfer_type == "Arrivals",
                                transfer_fee, 0), na.rm = TRUE),
    
    net_balance = total_income - total_expense,
    
    antal_transfers = n()    # NY KOLONNE: tæller alle transfers pr. sæson
  ) |> 
  arrange(season_format)

### Vi har nu indsamlet og behandlet alt ønsket data, så vi har det output vi ønsker at kunne binde sammen
### med vores resterende data indsamling

### vi gemmer nu vores færdigbehandlede tabel:
saveRDS(total_all_2, "VFFdata/TransfermarktData.rds")


