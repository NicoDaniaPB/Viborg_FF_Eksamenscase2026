#> Transferdata har været meget svære at få fingrene i,
#> da de fleste sider med data på området er utilgængelige for os.
#> Vi har derfor fundet et open-source-projekt, skabt af en eller flere privatpersoner,
#> som gør det lettere at hente fodbolddata i R.
#> Pakken hedder worldfootballR. Link til GitHub:
#> https://github.com/JaseZiv/worldfootballR
#>
#> Pakken blev den 18. september 2025 arkiveret og bliver ikke længere vedligeholdt.
#> Det kan give udfordringer i forhold til reproducerbarhed fremadrettet.
#> Hvis det viser sig, at transferdata har en signifikant betydning for antallet
#> af tilskuere til VFF's hjemmekampe, er antagelsen dog, at VFF selv har adgang
#> til deres interne transferdata. Disse data har vi dog ikke adgang til på nuværende tidspunkt.
#>
#> Kør installationen herunder, hvis du ikke allerede har installeret devtools.
#> devtools::install_github() bruges til at installere R-pakker direkte fra GitHub.
install.packages("devtools")

#> worldfootballR indeholder blandt andet transferdata fra Transfermarkt
#> (en af verdens største offentligt tilgængelige databaser for spillertransfers),
#> som ikke kan webscrapes direkte. Derfor benyttes denne metode i stedet.
#> Argumentet force = TRUE betyder, at pakken geninstalleres,
#> selv hvis den allerede er installeret i samme version.
devtools::install_github("JaseZiv/worldfootballR", force = TRUE)
#> Hvis R spørger om opdatering af afhængige pakker, vælges 3 for "None",
#> så eksisterende pakker ikke opdateres, da det ikke er nødvendigt.


# Indlæsning af nødvendige pakker.
pacman::p_load(worldfootballR, tidyverse, janitor)

# Mappe til data. Hvis den ikke eksistere, så bliver den oprettet.
if (!dir.exists("VFFdata")) dir.create("VFFdata")

#> Indlæser datasæt med Viborgs hjemmekampe i Superligaen, som blandt andet indeholder 
#> informationer om hvilke sæsoner de har været i Superligaen, som skal bruges til at 
#> hente transferdata fra de korrekte år.
vff_hjemmekampe <- readRDS("VFFdata/superstatsvff.rds")
sæsoner <- vff_hjemmekampe |>
  distinct(sæson) |>          # distinct(sæson) sikrer, at hver sæson kun optræder en gang.
  pull(sæson)                 # pull(sæson) konverterer kolonnen til en vektor.

#> Når der hentes data fra Transfermarkt, identificeres sæsoner ved den pågældende sæsons startår.
#>  substr(1, 4) udtrækker de første fire tegn i sæson strengen, f.eks. 2002 fra 2002/2003 osv.
#>  as.integer() konverterer værdien fra tekst til en numerisk værdi for årstallet.
år <- sæsoner |>
  substr(1, 4) |>
  as.integer()

#> fjerner år før 2002, da det er fra dette år og frem vi ser på VFF's kampe.
år <- år[år >= 2002]

#> Opretter en tom liste.
#> Listen udvides løbende i loopet, hvor hvert element
#> bliver en dataframe med transferdata for hver sæson.
list_of_transfers <- list()

#> Gennemløber alle startår og henter transferdata fra Transfermarkt.
#> seq_along(år) opretter en sekvens fra 1 til længden af vektoren år.
#> For loopet kører derfor en gang for hvert år i vektoren år.
for (i in seq_along(år)) {
  # Henter et startår ad gangen fra vektoren år.
  y <- år[i]
  #> Opbygger URL til Viborg FF's Transfermarkt-side for den givne sæson.
  #> paste0 sammensætter året og URL'en uden mellemrum til en samlet streng.
  url <- paste0("https://www.transfermarkt.com/viborg-ff/startseite/verein/1063/saison_id/", y)
  #> Henter alle transfers (sommer + vinter) for den pågældende sæson.
  #> og gemmer resultatet i listen, som f.eks. Viborg_2002.
  #> tm_team_transfers() henter alle spillertransfers for VFF.
  list_of_transfers[[paste0("Viborg_", y)]] <- tm_team_transfers(
    team_url = url,                 # team_url angiver URL’en til holdets Transfermarkt side.
    transfer_window = "all")        # transfer_window = "all" betyder, at både sommer og vintertransfers hentes.
  # Udskriver status i konsollen, så kan man se når data er hentet..
  cat("Henter data fra året:", y, "\n")
}

#> Kombinerer alle lister til en samlet dataframe.
#> bind_rows() binder alle tabellerne sammen under hinanden og laver et stort datasæt i en tibble.
total_transfers <- bind_rows(list_of_transfers)

#> Fjern kolonner der ikke skal bruges.
#> Med select og - foran de kolonner der ikke skal bruges, så de fravælges.
#> dplyr:: er benytet da nogen får fejl ved select uden  dplyr:: foran,
#> da andre pakker i R har en select funktion. 
total_transfers <- total_transfers |>
  dplyr::select(-team_name, -league, -country, -player_url, -player_position,
                -player_age, -player_nationality, -club_2, -league_2,
                -country_2, -in_squad, -appearances, -goals,
                -minutes_played, -transfer_notes)

# Gør kolonnenavne konsistente og nemme at arbejde med i R med janitor pakken.
# clean_names() omdanner f.eks. Transfer Fee til transfer_fee.
total_transfers_clean <- total_transfers |>
  clean_names()                                # Renser kolonnenavnene med clean_names() fra janitor.

#> Sæson står lige nu som et enkelt årstal. Laves om til korrekt sæsonformat f.eks. fra 2025 til 2025/2026.
#> Opretter en ny sæson kolonne med den nuværende ved hjælp af mutate, som bruger den oprindelige kolonne til at lave en ny.
total_transfers_clean <- total_transfers_clean |>
  #> Opretter sæsonformatet YYYY/YYYY+1 ud fra sæsonens startår.
  #> Sammensættes med paste0 med / imellem start og slut år. + 1 giver korrekt slut år i sæson strengen.
  #> as.numeric, som numerisk værdi. 
  mutate(season_format = paste0( season, "/", as.numeric( season) + 1))  

#> Fjerner den oprindelige sæson kolonne med kun et enkelt årstal.
#> Igen bruges select( med - foran den kolonne, som defor fravælges).
total_transfers_clean <- total_transfers_clean |> 
  dplyr::select(- season)

#> Fjerner alle transfers der er registreret som lån.
#> Ved at filtrere på kolonnen is_loan
#> og kun beholde rækker hvor is_loan == FALSE.
transfers <- total_transfers_clean |> 
  filter(is_loan == FALSE)

#> Fjerner observationer uden registreret transfersum
#> ved at filtrere alle rækker fra hvor transfer_fee er NA.
#> !is.na filtrerer rækker fra med manglende transfersum
#> og beholder kun observationer med en transfer_fee.
transfers <- transfers |> 
  filter(!is.na(transfer_fee))

# Beregner samlet transferøkonomi pr. sæson samt antallet af transfers.
total_all <- transfers |>
  # Grupperer data på sæson, så beregningerne foretages separat for hver sæson.
  group_by(season_format) |>
  # Opsummerer transferdata for hver sæson.
  # Tager mange rækker pr. sæson og samler dem til en række pr. sæson.
  summarise(
    #> Beregner samlede transferindtægter pr. sæson.
    #> Udvælger kun transfers hvor transfer_type er "Departures" som er spiller afgange. 
    #> og summerer transfer_fee for dem. 
    total_income = sum(transfer_fee[transfer_type == "Departures"]),
    #> Beregner samlede transferudgifter for sæsonen (spillerkøb).
    #> Samme koncept som før. Bare med spiller tilgange (Arrivals)
    total_expense = sum(transfer_fee[transfer_type == "Arrivals"]),
    # Nettobalance beregnes som indtægter minus udgifter.
    net_balance = total_income - total_expense,
    #> Antal transfers i sæsonen (både køb og salg).
    #> Tæller antallet af observationer for hver sæson.
    #> n() svarer til det samlede antal transfers i sæsonen.
    antal_transfers = n()) |> 
  #> Fjerner gruppering efter summarise.
  #> Så datasættet igen behandles som et almindeligt ugrupperet datasæt.
  ungroup() |> 
  #> Sorterer sæsonerne i faldende rækkefølge (nyeste først).
  #> arrange(desc) sørger for at arrangere i faldende rækkefølge.
  arrange(desc(season_format))

# Gemmer transfer data for VFF, som en RDS-fil. 
saveRDS(total_all, "VFFdata/TransfermarktData.rds")