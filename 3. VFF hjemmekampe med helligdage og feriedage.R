pacman::p_load(httr, jsonlite, lubridate, tidyverse)


# HENT HELLIGDAGE FRA NAGER.DATE API --------------------------------------

#Her defineres URL og parameteren
BASE_URL <- "https://date.nager.at/api/v3/PublicHolidays/"
LAND <- "/DK" # Landekode for Danmark

# Her defineres årsintervallet for dataindsamlingen (Matcher kamp-data perioden)
aar_super <- 2003:2026  

# Her oprettes en tom tibble under "helligdage" objektet.
# Variablene konveteres til de rigtige formater (navn = "chr" og dato = "date")
helligdage <- tibble(
  navn = character(), 
  dato = as.Date(character())
)

# Loop gennem hvert år og hent hellige dage fra date.nager
# Url laves, så den kører de specifikke år igennem
for (aar in aar_super) {
  url <- paste0(BASE_URL, aar, LAND)
  response <- httr::GET(url)
  
  
# Hent og parse JSON-data fra response
  data_raw <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(data_raw)

# Udvælg og omdøb relevante kolonner fra dataudtrækket
  helligdage_aar <- parsed |> 
    as_tibble() |>
    transmute(
      navn = localName, # Dansk navn på helligdagen
      dato = as.Date(date) # Konvertér til Date-format
    )
  
  # Her laver vi et samlet datasæt
  helligdage <- bind_rows(helligdage, helligdage_aar)
}

# Overblik over hvilke helligedage, som funktionen tager med
print(head(helligdage))
glimpse(helligedage)
# LOAD VFFs hjemmekampe  --------------------------------------------------
#Load af kamp-data RDS-fil
VFF_Hjemmekampe <- read_rds("VFFdata/superstatsvff.rds")
glimpse(VFF_Hjemmekampe)


# TILFØJ HELLIGDAGSINFORMATION TIL VFF-KAMPDATA ---------------------------
glimpse(helligdage)

#Tjek af datatyper for de kolonner deer skal joines
class(VFF_Hjemmekampe$tidsstempel) # returnere POSIXct
class(helligdage$dato) # returnere Date

# Vi konverterer tidsstempel til date-format for at muliggøre join mellem de to tabeller
VFF_Hjemmekampe1 <- VFF_Hjemmekampe |> 
  mutate(
    Dato_join = as.Date(tidsstempel)  #da det kun skal være datoen, så fjerne vi tidspunktet, så vi har identiske ID.
  )

glimpse(VFF_Hjemmekampe1)

# Left join for at tilføje helligdagsinformation
VFF_Komplet <- VFF_Hjemmekampe1 |> 
  left_join(
    helligdage,
    by = c("Dato_join" = "dato")
  ) |> 
  # Opret nye variable baseret på join-resultatet
  mutate(
    Er_helligdag = if_else(!is.na(navn), 1, 0), # Binær variabel: 1 = helligdag, 0 = ikke helligdag
    Helligdag_navn = navn # Behold helligdagens navn i separat kolonne
  ) |> 
  
  # Fjern den midlertidige 'navn' kolonne
  dplyr::select(-navn)

# Se resultatet
glimpse(VFF_Komplet)

# Opgør antal kampe på helligdage vs. almindelige dage
VFF_Komplet |> 
  count(Er_helligdag)
# Resultat: 15 kampe spillet på helligdage

# Fjern hjælpekolonne og filtrér data fra sæson 2003/2004 og fremefter
VFF_Komplet <- VFF_Komplet |> 
  dplyr::select(-Dato_join) |> # Fjern midlertidig join-kolonne
  filter(as.numeric(str_extract(sæson, "^\\d{4}")) >= 2003) # Ekstraher startår fra sæson-streng og filtrér

# Se resultatet igen
glimpse(VFF_Komplet)


# Tilføjelse af feriedage -------------------------------------------------

VFF_Komplet1 <- VFF_Komplet |>
  mutate(
    # Konvertér tidsstempel til Date-objekt
    Dato_kamp = as_date(ymd_hms(tidsstempel)),  # eller ymd_hms(tidsstempel) for POSIXct
    
    # Udtræk måned og ISO-ugenummer fra kampdato
    maaned = month(Dato_kamp),
    uge    = isoweek(Dato_kamp),
    
    # Definér ferieperioder baseret på danske skoleferier
    
    # Sommerferie: 1. juli til 15. august
    i_sommerferie   = Dato_kamp >= make_date(year(Dato_kamp), 7, 1) &
      Dato_kamp <  make_date(year(Dato_kamp), 8, 15),
    
    # Efterårsferie: Uge 42 + weekend i uge 41 (lørdag-søndag)
    i_efterårsferie = uge == 42 | (uge == 41 & wday(Dato_kamp, week_start = 1) >= 6),
    
    # Vinterferie: Uge 7 (typisk i februar)
    i_vinterferie   = uge == 7,
    
    # Kategorisér ferieperiode i tekstformat
    ferieperiode    = case_when(
      i_sommerferie   ~ "Sommerferie",
      i_efterårsferie ~ "Efterårsferie",
      i_vinterferie   ~ "Vinterferie",
      TRUE            ~ "Ingen"
    ),
    
    # Ekstraher klokkeslæt fra tidsstempel (format HH:MM)
    Tid_kamp = format(ymd_hms(tidsstempel), "%H:%M"))

# Tjek datoformat ved at kombinere dag/måned og år
head(paste0(VFF_Komplet1$dag_maaned, "/", VFF_Komplet1$kamp_aar))

# Inspicer de første tidsstempler

head(VFF_Komplet$tidsstempel, 10)

# Tjek komplette datasæt
glimpse(VFF_Komplet1)


# Gem endelige datasæt, som en RDS-fil ---------------------------------------------------------

saveRDS(VFF_Komplet1, file = "VFFdata/VFF_Komplet.rds")



