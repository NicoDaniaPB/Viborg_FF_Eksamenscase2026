#Vi indlæser pakkerne
pacman::p_load("tidyverse", "rvest", "stringr", "lubridate")

#Vi sikre os at mappen VFFdata er til rådighed. 
if (!dir.exists("VFFdata")) dir.create("VFFdata")

# Vi starter nu på webscraping af superstats -----------------------------------------------

#> Først skal sæsonerne hvor VFF har været i Superligaen webscrapes.
#> Så der ikke bliver hentet unødvenidgt meget data ned.
#> Desuden er det reproducerbart og man slipper for at definere en vektor med år manuelt. 

# Her læser vi HTML-siden direkte fra URL'en med read_html og gemmer den i containeren html_superstats.
html_superstats <- read_html("https://superstats.dk/hold/alltime?id=11&utm", encoding = "UTF-8")  

# Vi finder alle <table>-elementer på siden med html_elements. 
tables_sæsoner <- html_superstats |> html_elements("table")                    

#> map() anvender her funktionen html_table på hver tabel i tables_sæsoner.
#> Det giver en liste af dataframes, en for hver html-tabel på siden.
map(tables_sæsoner, html_table)

#> Det er tabel 3 der er den rigtige tabel, som indeholder de sæsoner hvor VFF er i Superligaen.
#> [[3]] trækker det tredje element ud af listen.
#> Funktionen html_table() fra pakken rvest konverterer html-tabellen til en dataframe.
tables_VFF <- (tables_sæsoner[[3]] |>  html_table()) 

#> Fjerner den første række i tabellen den med NA-værdier.
#> [-1, ] Tag alle rækker undtagen række 1, da 1 har - foran betyder det fjern. 
tables_VFF <- tables_VFF[-1, ] 

#> Returnerer TRUE hvis tabellen indeholder mindst en NA.
#> any er der nogen værdier? som er NA i tables_VFF?
any(is.na(tables_VFF))
# Output her: FALSE. #Det betyder at alle celler indeholder data.

#> Udtrækker slutåret for hver sæson fra teksten 1999/2000, 2016/2017 osv.
#> URL'en på Superstats bruger slutåret (f.eks. 2017 for sæsonen 2016/2017),
#> så de sidste fire cifre i sæsonstrengen skal bruges i et loop senere for at finde de rigtige sæsoner
#> Laver dem til integer (heltal).
#> $ efter tables_VFF bruges til at hente kolonnen sa_son. 
#> |> efter betyder og så... gør noget mere. 
VFF_sæsoner <- tables_VFF$Sæson |> 
  #> Regex-forklaring af "\\d{4}$".
  #> \\d betyder cifre mellem 0-9. 
  #> {4} betyder præcis fire cifre f.eks. 2026.
  #> $ slutningen af strengen.
  #> $ tvinger regex til at tage de sidste 4 cifre, ikke de første.
  #> Uden $ ville str_extract tage startåret (f.eks. 2016 i 2016/2017).
  str_extract("\\d{4}$") |> 
  as.integer()
#> Resultatet:  [1] 2026 2025 2024 2023 2022 2017 2016 2014 2008 2007 2006 2005 2004 2003 2002 2001 2000 1999 1997 1996 1994.
#> Så nu er VFF_sæsoner en vektor med start året for alle de år VFF har været i superligaen.


# Opretter en  list og gemmer i en container, som skal indeholde kampdata for superligakampe i de valgte år/sæsoner.
kampliste_samlede <- list()

#> For-loopet looper gennem alle årene i vektoren VFF_sæsoner.
#> # VFF_sæsoner indeholder slutåret for hver sæson (f.eks. 2024 = sæson 2023/2024)
for (y in VFF_sæsoner) {
  
  #> Bygger URL'en til superstats for en bestemt sæson.
  #> y svarer til året.
  #> paste0 binder URL'en og året sammen uden mellemrum.
  #> Resultatet bliver f.eks. "https://superstats.dk/program?aar=2026" hvis y = 2026.
  url <- paste0("https://superstats.dk/program?aar=", y)
  
  #> read_html() henter hele hjemmesidens html-kode med det tilføjede år.
  #> encoding = "UTF-8" sikre korrekt visning af alle tegn, som f.eks. æ, ø og å.
  kampliste_alle <- read_html(url, encoding = "UTF-8") |> 
    
    # html_elements("table") finder alle tabeller på siden i html koden.
    html_elements("table") |>
    
    #> html_table() konverterer hver tabel til en tibble.
    #> convert = FALSE = alle kolonner læses som tekst, så kan datatyper bestemmes senere.
    #> header = FALSE betyder at den første række i tabellen ikke skal bruges som kolonnenavne.
    #> Da det er Runde 1 f.eks. og ikke hold, resultat osv. 
    html_table(header = FALSE, convert = FALSE)
  
  #> På hver side med kampprogrammet ligger der flere tabeller, men de sidste 4 tabeller
  #> er ikke kampe. Det er f.eks. stilling, optakt osv.
  #> 1:(length(kampliste_alle) - 4) laver en sekvens fra 1 og frem til
  #> det totale antal tabeller i kampliste_alle minus de sidste 4 tabeller.
  kampliste_valgte <- kampliste_alle[1:(length(kampliste_alle) - 4)]
  
  #> Tilføjer en sæson-kolonne til hver tabel i listen.
  #> lapply() = løber alle tabeller igennem en ad gangen.
  #> hver_tabel_i_listen = betyder bare den aktuelle tabel.
  #> Sæson skal f.eks. være 2023/2024 når y = 2024 derfor paste0(y-1, "/", y). Paste0 sætter sæsonerne sammen f.eks. "2024/2025".
  #> function(hver_tabel_i_listen) { ... } er en funktion,
  #> som modtager et element ad gangen (en tabel).
  kampliste_valgte <- lapply(kampliste_valgte, function(hver_tabel_i_listen) {
    hver_tabel_i_listen |> mutate(sæson = paste0(y-1, "/", y))
  })
  # Gemmer kamp-tabellerne i en stor liste, hvor navnet på hvert element er sæsonen.
  # F.eks. kampliste_samlede[["2023/2024"]] = alle runder for sæsonen 2023/2024.
  kampliste_samlede[[paste0(y-1, "/", y)]] <- kampliste_valgte
}


# Gemmer data -------------------------------------------------------------

# Gemmer data i en RDS-fil i data mappen.
saveRDS(kampliste_samlede, "VFFdata/superstatsliste.rds")

# Indlæs data igen fra RDS-filen. Alle andre steps før nu kan udkommenteres. 
gemt_kampliste <- readRDS("VFFdata/superstatsliste.rds")


# Konverterer til dataframe -----------------------------------------------

#> Kombinerer alle lister til en samlet dataframe.
#> bind_rows() vi binder alle tabellerne sammen under hinanden og laver et stort datasæt i en tibble.
kampe_df <- gemt_kampliste |> 
  bind_rows()


# Rensning af data --------------------------------------------------------


# Først skal rækkerne der kun indeholder runde nummer transformeres til en kolonne for sig selv.
kampe_df <- kampe_df |> 
  mutate(
    #> Opretter en hjælpekollonne find_runde med mutate, som kun indeholder tekst fra de rækker,
    #> hvor X1 starter med "Runde" (f.eks. "Runde 1", "Runde 2", osv.).
    #> ^ = betyder "i starten af teksten".
    #> str_detect(X1, "^Runde"): returnerer TRUE for rækker,
    #> hvor X1 starter med ordet "Runde" (f.eks. "Runde 1", "Runde 12").
    #> if_else(TRUE) = gemmer i containeren find_runde, ellers NA = FALSE gemmer ikke.
    find_runde = if_else(str_detect(X1, "^Runde"), X1, NA)) |> 
  
  
  #> fill(find_runde) kopierer den seneste ikke-NA værdi nedad.
  #> Resultat: alle kampe under "Runde 1" får find_runde = runde 1,
  #> alle kampe under "Runde 2" får find_runde = runde 2, osv. Hele vejen igennem.
  fill(find_runde) |>
  
  #> Opretter selve runde-kolonnen der skal være i dataframen igen med mutate.
  #>
  #> str_extract henter tallet (f.eks. "1") fra "Runde 1".
  #> \\d+ finder det første tal (en eller flere cifre) i teksten, f.eks. "1" i "Runde 1".
  #> as.integer() gør det numerisk, altså konverterer tallet fra tekst til heltal (integer).
  mutate(runde = str_extract(find_runde, "\\d+") |> as.integer()) |> 
  
  #> Fjerner rækker der selv er runde-overskrifter:
  #> str_detect(X1, "^Runde") finder rækker hvor kolonnen X1 starter med ordet "Runde" man kunne også have valgt X2.
  #> ! foran betyder ikke, så filter(!str_detect) bevarer alle rækker,
  #> undtagen rækkerne med "Runde 1", "Runde 2", osv.
  #> 1 Runde 1 Runde 1     ""        ""    ""       ""                      ""    2025/2026
  #> 2 Fre     18/07 19:00 "VFF-FCK" "2-3" "6.696"  "Sandi Putros"          ""    2025/2026
  #> Så her fjernes række 1, osv. 
  filter(!str_detect(X1, "^Runde")) |>  
  
  #> Minus fordi vi fjerner hjælpeakolonnen find_runde, så den ikke optræder i dataframen.
  #> Kolonnen blev kun brugt som mellemtrin til at finde og udfylde rundenummeret,
  #> select(-...) svarer til at fravælge. 
  select(-find_runde)                      


#> Her renser vi dataene, giver kolonnerne passende navne, fjerner kolonne 7 (TV-logoer i billedformat på hjemmesiden),
#> Vi fjerner overflødige rækker, filtrerer kun de kampe hvor der findes et resultat,
#> og rydder op i tekstformater såsom tilskuertal og ugedage.
kampe_df <- kampe_df |>
  
  # Fjerner kolonnen X7, kolonnen indeholdte TV-logoer i billedformat på hjemmesiden.
  #> select(-...) svarer til at fravælge.
  select(-X7) |>
  
  #> Beholder kun rækker hvor kolonnen X5 indeholder et tal (tilskuertallet) viser at kampen er spillet.
  #> Regex-forklaring: ^ = start af teksten.
  #> [0-9\\.]+ = en eller flere cifre og eller punktum.
  #> $ = markering af slutning.
  #> Så værdien må kun bestå af tal og punktummer.
  #> str_detect(X5, "^[0-9\\.]+$"): returnerer TRUE for rækker:
  #> Med f.eks. 12.000, 100, 0 osv. men returner FALSE ved f.eks. NA, "", "-" osv. 
  #> Filtrerer kampene så kun færdigspillede kampe (med tilskuertal) bevares.
  filter(str_detect(X5, "^[0-9\\.]+$")) |> 
  
  #> Nu giver vi kolonnerne nye navne. 
  #> rename() syntaks: new name = old name f.eks. ugedag = X1.
  #> Så hedder kolonnen nu ugedag osv.  
  rename(
    ugedag = X1,
    dato_og_tidspunkt = X2,
    kamp = X3,
    resultat = X4,
    tilskuere = X5,
    dommer = X6) |> 
  
  mutate(
    #> Fuldender ugedagsnavnene:
    #> paste0() sætter teksten sammen uden mellemrum.
    #> Her tilføjes ordet "dag" direkte efter den eksisterende ugedag.
    #> Lør bliver til Lørdag osv. 
    ugedag = paste0(ugedag, "dag"),
    tilskuere = tilskuere |> 
      #> Fjerner punktummer i tal f.eks. 1.000, så det bliver 1000.
      #> "\\." “et puktum" og "" "ingenting" = byt punktum ud med ingenting = fjern punktummet.
      #> str_replace_all(fjerner alle ., erstatter alle med "ingenting"). 
      str_replace_all("\\.", "") |>  
      # Konverterer tekststrengen f.eks. 1000 til et rigtigt heltal (integer). 
      as.integer())


# Her gemmer vi ændringer -------------------------------------------------------------

saveRDS(kampe_df, "VFFdata/superstats.rds")

kampe_df <- readRDS("VFFdata/superstats.rds")

# Nye variabler og fortsat rensning af data -----------------------------------------------------------

#> Her opdeles variablen dato_og_tidspunkt, som består af både dato og klokkeslæt,
#> i to separate kolonner: dato og klokkeslæt.
kampe_df <- kampe_df |>  
  # separate() splitter teksten ved mellemrum (sep = " ").
  separate(dato_og_tidspunkt, into = c("dato", "klokkeslæt"), sep = " ") |> 
  #> Efter split konverteres klokkeslæt fra en tekst streng til tidsformatet hms.
  #> med hm(), så tiden behandles korrekt i beregninger og ved dannelse af dttm senere.
  mutate(klokkeslæt = hm(klokkeslæt))

#> Trækker månederne ud - med unique, så f.eks. 05 kun optræder en gang i outputted osv.
#> substr(dato, 4, 5) henter tegn 4 og 5 i dato-strengen, dvs. måneden.
unique(substr(kampe_df$dato, 4, 5))
#> Output: [1] "07" "08" "09" "10" "11" "03" "04" "05" "06" "12" "02"
#> Superligaen starter typisk i juli/07 og slutter typisk i maj/05 / start juni/06


#> Tilføj år til dato og ændrer formatet fra <chr> til <date>.
#> Dato-kolonnen indeholder kun dag og måned (dd/mm), derfor skal det bestemmes,
#> hvilket år kampene hører til. Det afhænger af Superligaens struktur, som blev kigget på lige før.
kampe_df <- kampe_df |>
  mutate(dato = paste0(
    # Sætter året ind efter datoen og / med paste0.
    dato, "/", 
    if_else(
      #> Hent månedstallene fra datoen.
      #> substr(dato, 4, 5) tager tegn 4 og 5 i dd/mm dvs. mm.
      #> as.integer(...) konverterer "07" til 7, "03" til 3 osv.
      #> Hvis måneden er 7 eller større (juli–december),
      #> så betyder det, at kampen hører til sæsonens startår.
      as.integer(substr(dato, 4, 5)) >= 7,
      #> Hvis ovenstående betingelse er TRUE:
      #> så bruges sæsonens startår (før "/")
      #>   f.eks. "2023/2024" så det "2023"
      as.integer(substr(sæson, 1, 4)),
      #> Hvis betingelsen er FALSE (måned < 7 = januar–juni):
      #> så bruges sæsonens slutår (efter "/")
      #> f.eks. "2023/2024" så det "2024"
      as.integer(substr(sæson, 6, 9)))) |> 
      #> Efter årstallet er tilføjet bruges dmy() til at lave datoen om til et
      #> rigtigt datoobjekt med formatet <date>.
      dmy())


#> Vi opretter en samlet datetime-variabel (dttm), som gør det nemmere når datasættet skal merges med andre datasæt senere.
#> dato er et <date> objekt og klokkeslæt er et <period> objekt.
#> Når de lægges sammen, så bliver det et <dttm> objekt i stedet.
kampe_df <- kampe_df |> 
  mutate(
    #> Opretter et samlet tidsstempel (date + time i dttm-format) 
    #> as_datetime konverterer til dato + klokkeslæt
    tidsstempel = as_datetime(dato + klokkeslæt)) |> 
  # Fjerner de gamle kolonner med select(-x, -x1), da tidsstempel nu indeholder begge dele
  select(-dato, -klokkeslæt)


# Trækker timerne ud med unique, så f.eks. 15 kun optræder en gang i outputted osv.
unique(hour(kampe_df$tidsstempel))
# Output: [1] 19 14 16 18 20 15 12 17 21 13 11


# Vi laver en ny kategorivariabel for tidsperiode i forhold til kampens starttidspunkt på dagen.
kampe_df <- kampe_df |> 
  mutate(
    #> hour(tidsstempel) henter klokkeslættets time-del som et heltal (0–23).
    #> if-else før kl. 14 -> formiddag, Kl. 14-17 -> eftermiddag og efter kl. 17 -> aften.
    #> paste0() sætter ugedag + tidskategori(_for-, eftermiddag og aften) sammen,
    #> f.eks. Søndag + _aften = Søndag_aften osv.
    tidsperiode = if_else(
      hour(tidsstempel) < 14,
      paste0(ugedag, "_formiddag"),
      if_else(
        hour(tidsstempel) <= 17,
        paste0(ugedag, "_eftermiddag"),
        paste0(ugedag, "_aften"))))


#> Her deles resultat-kolonnen op i to nye kolonner:
#> separate() splitter tekst baseret på et separatortegn, her er det "-", som er mellem mål 
#> for hjemmeholdet og udeholdet.
#> c("mål_hjemme", "mål_ude") er en charactervektor med navnet på de to nye kolonner,
#> som resultatet skal splittes ud i.
#> convert = TRUE betyder, at R automatisk konverterer tekst til tal, altså <int> i stedet for <chr>. 
kampe_df <- kampe_df |> 
  separate(resultat, into = c("mål_hjemme", "mål_ude"), sep = "-", convert = TRUE)


#> Vi laver nu to nye kolonner med point til hjemme- og udeholdet.
#> point_hjemme med if_else 
#> 3 point hvis hjemmeholdets mål > udeholdets mål.
#> 1 point hvis mål_hjemme == mål_ude, ellers 0 point.
#> point_ude fungerer omvendt for udeholdet. 
kampe_df <- kampe_df |> 
  mutate(
    point_hjemmehold = ifelse(mål_hjemme > mål_ude, 3, ifelse(mål_hjemme == mål_ude, 1, 0)),
    point_udehold = ifelse(mål_hjemme < mål_ude, 3, ifelse(mål_hjemme == mål_ude, 1, 0)))


#> separate() deler kampnavnet i hjemme- og udehold,
#> men på superstats hjemmeside er der mellemrum efter "-" ved hold med kun to bogstaver.
#> str_trim() fjerner mellemrum i starten og slutningen af teksten,
#> så " OB" bliver til "OB" og " VB" bliver til "VB".
kampe_df <- kampe_df |> 
  separate(kamp, into = c("hjemmehold", "udehold"), sep = "-") |> 
  mutate(hjemmehold = str_trim(hjemmehold), udehold   = str_trim(udehold))


# Gemmer ændringer -------------------------------------------------------------

saveRDS(kampe_df, "VFFdata/superstats.rds")

kampe_df <- readRDS("VFFdata/superstats.rds")


# Sum af mål i de sidste 3 hjemmekampe lag  -----------------------------------

#lag: Henter værdier fra tidliger rækker / kigger bagud i tiden. 

#> Mål i sidste tre hjemmekampe.
#> Skal vise målene scoret i de sidste tre hjemmekampe op til den nye hjemmekamp.
kampe_df <- kampe_df |>
  #> arrange(sæson, tidsstempel): Sorterer alle kampe kronologisk inden for hver sæson.
  #> Det er vigtigt, fordi lag() ellers henter fra en forkert kamp.
  arrange(desc(sæson), tidsstempel) |>
  #> group_by(sæson, hjemmehold): beregningen laves separat for:
  #> Hver sæson så sæsonerne ikke overlapper hinanden.
  #> Hvert hjemmehold så hvert hold, så FCK's og VFF's hjemmekampes mål ikke lægges sammen.
  group_by(sæson, hjemmehold) |> 
  mutate(
    mål_seneste_3_hjemme =
      # lag(mål_hjemme, 1) Mål i den forrige hjemmekamp.
      lag(mål_hjemme, 1, default = 0) +
      # lag(mål_hjemme, 2) Mål i de to forrige hjemmekampe.
      lag(mål_hjemme, 2, default = 0) +
      # lag(mål_hjemme, 3) Mål i de tre forrige kampe. 
      lag(mål_hjemme, 3, default = 0)
    #> Det der sker:
    #> Runde 1: vil altid vise 0 (ingen tidligere kampe).
    #> Runde 2: mål i kamp 1
    #> Runde 3: mål i kamp 1 + kamp 2
    #> Fra runde 4 i hver sæson vises summen af de tre seneste hjemmekampe. 
  ) |> 
  #> ungroup() fjerner den grouping, der blevet lavet med group_by().
  #> Så det igen bliver et almindeligt datasæt uden grupper.
  ungroup()

# desc = descending, så datasættet vises i faldende rækkefølge igen. 
kampe_df |> 
  arrange(desc(sæson), tidsstempel)


# Antallet af tilskuere ved seneste møde ----------------------------------

#> Antallet af tilskuere sidst hjemmeholdet mødte samme modstander 
#> Indtil videre bliver default sat til 0. 
kampe_df <- kampe_df |> 
  #> Sorterer alle kampe kronologisk på tværs af alle sæsoner.
  #> Dette er vigtigt, fordi lag()-funktionen skal vide hvilken kamp der kronologisk var den seneste.
  arrange(tidsstempel) |>   
  #> Grupperer data efter hjemmehold og udehold.
  #> Det betyder, at lag() kan finde den seneste hjemmekamp mod præcis samme modstander.
  group_by(hjemmehold, udehold) |> 
  #> lag(tilskuere) Henter tilskuertallet fra den foregående kamp mod samme modstander.
  #> default = 0 Bruges, indtil en bedre løsning findes, når der ikke er en tidligere hjemmekamp mod modstanderen.
  #> Sker ved det historiske første møde med en modstander.
  mutate(
    tilskuere_hold_lag = 
      lag(tilskuere, default = 0)) |> 
  # Fjerner gruppering.
  ungroup()
# desc = descending, så datasættet vises i faldende rækkefølge igen. 
kampe_df <- kampe_df |> 
  arrange(desc(sæson), tidsstempel)


# Point i sidste 3 hjemmekampe --------------------------------------------------

# Viser summen af point i de tre seneste hjemmekampe før den aktuelle kamp.
kampe_df <- kampe_df |>
  #> Sorterer kampe kronologisk inden for hver sæson.
  #> Gør at lag() henter point fra de rigtige hjemmekampe.
  arrange(desc(sæson), tidsstempel) |>
  #> Gruppen laves pr. sæson og hjemmehold.
  #> Hver sæson behandles for sig og hvert hjemmehold får sin egen hjemmebane historik.
  group_by(sæson, hjemmehold) |>
  mutate(
    form_seneste_3_hjemme =
      # Point i seneste hjemmekamp.
      lag(point_hjemmehold, 1, default = 0) +
      # Point i de to seneste.
      lag(point_hjemmehold, 2, default = 0) +
      # Point i de tre seneste.
      lag(point_hjemmehold, 3, default = 0)
    #> Det der sker:
    #> Runde 1: 0 (ingen tidligere hjemmekampe).
    #> Runde 2: point fra runde 1.
    #> Runde 3: point fra runde 1 + runde 2.
    #> Runde 4+: point fra tre seneste hjemmekampe.
  ) |>
  #> Fjerner grouping og går tilbage til normalt datasæt.
  ungroup()


# Gemmer ændringer -------------------------------------------------------------

saveRDS(kampe_df, "VFFdata/superstats.rds")

kampe_df <- readRDS("VFFdata/superstats.rds")


# Placering lag -----------------------------------------------------------

#> Fjerner sæsonen 1993/1994 fra datasættet.
#> I denne sæson spiller VFF kun 18 kampe.
kampe_df <- kampe_df |>  
  # Beholder kun de kampe der IKKE!= er 1993/1994.
  filter(sæson != "1993/1994")

#> Her ændres datasættet til en række pr. kamp pr. hold med pivot_longer() = dobbelt så mange rækker. 
#> Det giver 2 rækker pr. kamp (en for hjemmeholdet og en for udeholdet)
#> Det gør det muligt at beregne point, målscore osv. pr. hold.
kampe_kloning <- kampe_df |>   
  pivot_longer(cols = c(hjemmehold, udehold),                                   # De to kolonner laves om til rækker.
               names_to = "hjemme_ude",                                                    # Her gemmes navnet på kolonerne hjemmehold og udehold.
               values_to = "hold") |>                                                      # Denne nye kolonne indeholder selve holdnavnet f.eks. VFF.
  #> Opretter en ny kolonne = point.
  #> if_else tjekker om denne række er hjemmeholdets række?
  #> Hvis ja, så bruges point_hjemmehold. Hvis nej, så bruges point_udehold.
  mutate(point = if_else(hjemme_ude == "hjemmehold", point_hjemmehold, point_udehold),
         #> mål_for = mål scoret af dette hold.
         #> Hvis denne række er hjemmeholdets række, så bruges mål_hjemme.
         #> Ellers udeholdet, så bruges mål_ude. 
         mål_for = if_else(hjemme_ude == "hjemmehold", mål_hjemme, mål_ude),
         #> mål_imod = antal mål modstanderen scorede mod holdet i kampen.
         #> Hvis det er hjemmeholdets række, så bruges mål imod = mål_ude (modstanderen = udeholdet).
         #> Hvis dette er udeholdets række, så bruges mål imod = mål_hjemme (modstanderen = hjemmeholdet).
         mål_imod = if_else(hjemme_ude == "hjemmehold", mål_ude, mål_hjemme))

#> Her beregnes stillingen løbende for hver runde.
stillinger <- kampe_kloning |>
  #> Starter med en række pr. hold pr. kamp og der grupperes efter sæson og hold.
  #> Så stillingen bliver indefor de seperate sæsoner og hold pr. kamp.
  group_by(sæson, hold) |>
  #> Kampene kommer i kronologisk rækkefølge for hvert hold.
  #> arrange efter tidspunktet for kampene, indefor grupperne.
  #> Så sorteres kampene kronologisk for hvert hold i hver sæson med .by_group = TRUE.  
  arrange(tidsstempel, .by_group = TRUE) |> 
  #> Beregn løbende totaler cumulative summer, altså løbende sum.
  #> Løbende point, løbende mål for, løbende mål i mod og løbende mål difference for holdene.
  #> Efterfølgende fjernes grouping for at omgruppere på sæson, runde og hold.
  mutate(
    point_sum    = cumsum(point),
    mål_for_sum  = cumsum(mål_for),
    mål_imod_sum = cumsum(mål_imod),
    måldiff_sum  = mål_for_sum - mål_imod_sum) |> 
  ungroup() |> 
  #> Laver en række pr. sæson, pr. runde, pr. hold.
  #> For at få point sum osv. for en bestemt runde for holdene i en given sæson.
  #> last() henter de nyeste summer for holdet i den pågældende runde.
  group_by(sæson, runde, hold) |> 
  # summarise() laver en række for denne kombination af point, mål_for osv.
  summarise(
    point_sum    = last(point_sum),
    mål_for_sum  = last(mål_for_sum),
    mål_imod_sum = last(mål_imod_sum),
    måldiff_sum  = last(måldiff_sum),
    # .groups = "drop" gør, at resultatet ikke fortsætter med at være grupperet.
    .groups = "drop") |> 
  
  #> For hver runde rangeres holdene korrekt.
  #> 1) flest point
  #> 2) bedst måldifference
  #> 3) flest scorede mål
  #> 4) alfabetisk (som sidste udvej. Kan forbedres)
  #> I faldende rækkefølge med desc, så det er dem med flest point, mål difference osv. som kommer først.
  group_by(sæson, runde) |> 
  arrange(
    desc(point_sum),
    desc(måldiff_sum),
    desc(mål_for_sum),
    hold,
    # Sorterer inde i gruppen .by_group = TRUE.
    .by_group = TRUE) |>
  
  # Tildeler holdenes normale placering, altså den rigtige stilling.
  mutate(plac_norm = row_number()) |>  # giver korrekt placering i stillingen pr. runde.
  ungroup()                           # fjerner grupperingen bagefter.


#> Strukturen i superligaen ændrer sig igennem årene.
#> For VFF er sæsonen 2016/2017 helt unik, mens sæsonerne de er i Superligaen
#> Før 2016/2017 er anderldes og sæsonerne efter også er en kategori for sig. 
#> I sæsonen 2016/2017 er der et split i tabellen og det samme i sæsonerne efter.
#> Først skal split runderne identificeres for hver sæson.
splitinfo <- stillinger |>  
  # Gruppere pr. sæson hvor splittet skal findes. 
  group_by(sæson) |>  
  summarise(
    max_runde  = max(runde),
    split_runde = if_else(max_runde == 32, 22,    # Split ved runde 22 i 12-holdssæsoner med mesterskabsspil og nedrykningsspil.
                          if_else(max_runde == 36, 26,                  # split ved runde 26 i sæsonen 2016/17, sæson med 14 hold og mesterskabs.... osv.
                                  NA))) |>                                      # Ellers ingen split. 
  ungroup()                                       # Fjerner gruppering efter summarise


# Tilføjer split-information til stillingerne for hver sæson.
# left_join sikrer at alle rækker i stillinger bevares,
# og at max_runde samt split_runde bliver tilføjet ud fra sæsonen.
stillinger_split <- stillinger |>  
  left_join(splitinfo, by = "sæson")


#> Top og bund skal låses fast ved split i de sæsoner med opdeling efter x-antal runder.
#> Sådan at et hold i f.eks. bund 6 eller bund 8 ikke kan komme i top 6 og omvendt.
stillinger_split <- stillinger_split |> 
  
  #> Grupper efter sæson og hold, så hvert holds runder behandles separat.
  #>  Altså et hold ad gangen. Inden for en sæson.
  group_by(sæson, hold) |>  
  
  #> Sørger for at runderne står i korrekt rækkefølge INDEN FOR hver gruppe.
  #> .by_group = TRUE betyder: arrange fungerer separat for hver runde. 
  #> Det vil altså sige at kampene sorteres kronologisk for hvert hold hver for sig.
  arrange(runde, .by_group = TRUE) |> 
  mutate(
    
    #> Ved splitrunden: Her indeles i 2 gruppe kategorier, top og bund. 
    #> Top hvis plac_norm ≤ 6bund" hvis plac_norm > 6 og NA i alle andre runder.
    #> Split runde er NA i sæsoner uden split. 
    gruppe = if_else(!is.na(split_runde) & runde == split_runde & plac_norm <= 6, "top",
                     if_else(!is.na(split_runde) & runde == split_runde & plac_norm > 6, "bund", NA))) |>  
  
  #> Hvis et hold f.eks. bliver top ved splitrunden, så bliver det top i alle de efterfølgende runder.
  #> Dette er efter strukturen i de sæsoner med split.
  #> fill() går nedad i rækkefølgen af runder (I stigende orden nedad).
  tidyr::fill(gruppe, .direction = "down") |>  
  # Fjern gruppering — resten af pipeline kræver et ikke-grupperet datasæt.
  ungroup()

#> Efter splitrunden er top og bund grupperne låst.
#> Nu skal holdene sorteres inden for deres gruppe i hver runde.
stillinger_split <- stillinger_split |>  
  
  #> Grupper efter (sæson, runde, gruppe). 
  #> Så rangeres top- og bund-grupperne separat i hver runde.
  group_by(sæson, runde, gruppe) |>
  
  #> Sorter hver gruppe med arrange:
  #> 1) flest point.
  #> 2) bedst måldifference.
  #> 3) flest scorede mål.
  #> 4) alfabetisk (som sidste udvej. Kan forbedres).
  #> I faldende rækkefølge med desc, så det er dem med flest point, mål difference osv. som kommer først.
  arrange(desc(point_sum),
          desc(måldiff_sum),
          desc(mål_for_sum),
          hold,
          .by_group = TRUE) |> 
  #> Intern placering I gruppen:
  #> top-gruppe = 1,2,3,4,5,6.
  #> bund-gruppe = 1,2,3,4,5,6 og ...8 i 2016/2017.
  #> Hvis gruppe = NA (sæsoner uden split) gives NA.
  mutate(intern = if_else(!is.na(gruppe), row_number(), NA),
         
         #> Global placering:
         #> I sæsoner uden split bruges plac_norm.
         #> Ellers top-gruppe med intern placering (1–6).
         #> Ellers bund-gruppe → intern + 6 (7–12 og 7-14 i 2016/2017).
         global_plac = if_else(is.na(gruppe), plac_norm,           
                               if_else(gruppe == "top", intern, intern + 6))) |>  
  # Fjern grupperingen igen.
  ungroup()


#> Henter bund-placeringer i runde 32 for 2016/2017.
#> Da de efter 32 runde spiller play-off, som ikke giver point. 
bund32 <- stillinger_split |> 
  
  #> Her udtrækkes derfor bund holdenes globale placering i runde 32.
  #> Den gemmes som frys_plac, så den senere kan bruges som fast låst placering.
  filter(sæson=="2016/2017", runde==32, gruppe=="bund") |> 
  select(sæson, hold, frys_plac = global_plac)


#> Vi fryser bundholdenes placering efter runde 32 i sæsonen 2016/2017.
#> I denne sæson spiller kun top-6 videre efter runde 32 om placeringer. 
#> Vi joiner først deres placering fra runde 32 (frys_plac),
#> og bruger den til at overskrive global_plac i runder efter 32.
stillinger_split <- stillinger_split |> 
  left_join(bund32, by = c("sæson","hold")) |> 
  mutate(
    
    #> Derefter bruges if_else til at overskrive global_plac KUN for:
    #> Sæsonen 2016/2017. Gruppe == "bund" (altså bund-8) efter 32 runder.
    #> Alle andre sæsoner, top-hold, eller runder 1–32 forbliver uændret.
    global_plac = if_else(
      sæson=="2016/2017" & gruppe=="bund" & runde>32,
      frys_plac, global_plac)) |> 
  
  #> Til sidst fjernes frys_plac igen, da den ikke længere er nødvendig. 
  select(-frys_plac)


# Beregner placeringen før kampen (lag).
stilling_lag <- stillinger_split |>  
  
  # Grupper pr. sæson og hold, så hver klub behandles separat.
  group_by(sæson, hold) |> 
  
  # Sorterer runder kronologisk inden for hvert hold.
  arrange(runde, .by_group = TRUE) |>  
  
  #> lag(global_plac) henter placeringen fra runden før.
  #> default = 0L betyder: ingen placering før runde 1.
  mutate(placering_lag = lag(global_plac, default = 0)) |> 
  ungroup()


#> Udtrækker lagget placering for hjemme- og udehold separat,
#> så de kan joines tilbage på det originale kampdatasæt i to separate kolonner.
plac_hjemme <- stilling_lag |>  
  select(sæson, runde, hjemmehold = hold,
         placering_lag_hjemme = placering_lag)

plac_ude <- stilling_lag |>  
  select(sæson, runde, udehold = hold,
         placering_lag_ude = placering_lag)

#> Tilføjer placering-lag til kampdata.
#> Joiner på (sæson, runde, hjemmehold) og (sæson, runde, udehold).
kampe_df <- kampe_df |>  
  left_join(plac_hjemme, by = c("sæson", "runde", "hjemmehold")) |>  
  left_join(plac_ude,    by = c("sæson", "runde", "udehold"))


# Form i sidste 3 kampe ude og hjemme -------------------------------------

form_total <- kampe_kloning |>
  # Sorterer alle kampe kronologisk inden for hver sæson.
  arrange(sæson, tidsstempel) |>
  # Grupperer efter sæson og hold, så hvert hold behandles for sig.
  group_by(sæson, hold) |>
  mutate(
    # Summen af point i de seneste 3 kampe (uanset hjemme/ude).
    # lag(point, 1) = kamp før den aktuelle.
    # lag(point, 2) = to kampe før.
    # lag(point, 3) = tre kampe før.
    form_seneste_3_total =
      lag(point, 1, default = 0) +
      lag(point, 2, default = 0) +
      lag(point, 3, default = 0)) |>
  ungroup()

form_hjemme <- form_total |>
  select(sæson, runde, hjemmehold = hold,
         form_seneste_3_hjemmehold = form_seneste_3_total)

form_ude <- form_total |>
  select(sæson, runde, udehold = hold,
         form_seneste_3_udehold = form_seneste_3_total)

kampe_df <- kampe_df |>
  left_join(form_hjemme, by = c("sæson", "runde", "hjemmehold")) |>
  left_join(form_ude,    by = c("sæson", "runde", "udehold"))


# Mål i sæsonen hjemme + ude lagget før kampen

# Beregn løbende sæsonmål pr. hold uanset hjemme/ude.
mål_sæson_lag <- kampe_kloning |>
  # Kampene sorteres kronologisk inden for hver sæson.
  arrange(sæson, tidsstempel) |>
  
  # Et samlet forløb pr. sæson og hold.
  group_by(sæson, hold) |>
  
  mutate(
    # Samlede mål i sæsonen EFTER kampen.
    mål_sæson_total = cumsum(mål_for),
    
    # Lagget værdi = mål i sæsonen inden kampen.
    mål_sæson_lag = lag(mål_sæson_total, default = 0)) |>
  ungroup()

# Klargør data til join med hjemmehold
mål_sæson_hjemme <- mål_sæson_lag |>
  select(
    sæson,
    runde,
    hjemmehold = hold,
    mål_sæson_lag_hjemmehold = mål_sæson_lag)

# Klargør data til join med udehold
mål_sæson_ude <- mål_sæson_lag |>
  select(
    sæson,
    runde,
    udehold = hold,
    mål_sæson_lag_udehold = mål_sæson_lag)

# Join mål lag tilbage på kampdatasættet.
kampe_df <- kampe_df |>
  left_join(mål_sæson_hjemme, by = c("sæson", "runde", "hjemmehold")) |>
  left_join(mål_sæson_ude,    by = c("sæson", "runde", "udehold"))


# Gemmer ændringer -------------------------------------------------------------

saveRDS(kampe_df, "VFFdata/superstats.rds")

kampe_df <- readRDS("VFFdata/superstats.rds")

# VFF hjemmekampe ---------------------------------------------------------


#> Udvælger alle rækker hvor hjemmeholdet er VFF.
#> Det giver en oversigt over alle VFF's hjemmekampe i datasættet.
vff_hjemmekampe <- kampe_df |> 
  filter(hjemmehold == "VFF")

paste("Antal hjemmekampe i perioden for VFF er:", nrow(vff_hjemmekampe))
#> Output: 318. Det er godt, da VFF ifølge Superstats har spillet 327 hjemmekampe all time.
#> Sæsonen 1993/1994 indeholdte 9 hjemmekampe, så alle kampene er succesfuldt kommet med i tabellen.


# Tilføjer A, B, C og D kampe ---------------------------------------------

vff_hjemmekampe <- vff_hjemmekampe |> 
  # Opretter "udehold_rang" baseret på modstanderens styrke.
  mutate(udehold_rang = case_when(
    udehold %in% c("AGF", "FCM", "BIF", "FCK") ~ "A",
    udehold %in% c("Aab", "RFC", "SIF", "VB", "OB") ~ "B",
    udehold %in% c("FCN", "SJF", "FCF", "LBK") ~ "C",
    TRUE ~ "D")) |> # Alle andre.
  mutate(udnyttelsesgrad_tilskuere = tilskuere / 9695) # Udnyttelsesgrad: en simpel proxy = tilskuer / stadionkapaciteten.
# https://www.dbu.dk/resultater/stadium/1293 ifølge DBU er VFF's kapacitet på Energi Viborg Arena 9.695. 

# Gemmer ændringer -------------------------------------------------------------

saveRDS(vff_hjemmekampe, "VFFdata/superstatsvff.rds")

vff_hjemmekampe <- readRDS("VFFdata/superstatsvff.rds")

# Ændre rækkefølgen på kolonnerne  ----------------------------------------

# Ser navnene på alle kolonnerne. 
names(vff_hjemmekampe) 

# Tæller antallet af kolonner.
ncol(vff_hjemmekampe) # Output: 24.

# Vælger rækkefølgen af kolonnerne med select. 
vff_hjemmekampe <- vff_hjemmekampe |>
  select(ugedag, tidsperiode, tidsstempel, runde, sæson,
         hjemmehold, udehold, mål_hjemme, mål_ude, mål_sæson_lag_hjemmehold, mål_sæson_lag_udehold,
         tilskuere, point_hjemmehold, point_udehold, placering_lag_hjemme,
         placering_lag_ude, mål_seneste_3_hjemme, form_seneste_3_hjemme,
         form_seneste_3_hjemmehold, form_seneste_3_udehold, tilskuere_hold_lag,
         udnyttelsesgrad_tilskuere, udehold_rang, dommer
  ) |>
  arrange(desc(tidsstempel)) # Faldende rækkefølge.    

# Gemmer ændringer -------------------------------------------------------------

saveRDS(vff_hjemmekampe, "VFFdata/superstatsvff.rds")








