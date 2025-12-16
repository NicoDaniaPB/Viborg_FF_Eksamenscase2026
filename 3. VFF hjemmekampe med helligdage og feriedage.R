pacman::p_load(httr, jsonlite, lubridate, tidyverse)


# HENT HELLIGDAGE FRA NAGER.DATE API --------------------------------------

BASE_URL <- "https://date.nager.at/api/v3/PublicHolidays/"
LAND <- "/DK"

# Samme periode som dine kampe (2000–2026)
aar_super <- 2003:2026  

helligdage <- tibble(
  navn = character(),
  dato = as.Date(character())
)

for (aar in aar_super) {
  url <- paste0(BASE_URL, aar, LAND)
  response <- httr::GET(url)
  
  if (httr::status_code(response) != 200) {
    warning("Fejl ved hentning for år: ", aar)
    next
  }
  
  data_raw <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(data_raw)
  
  helligdage_aar <- parsed |> 
    as_tibble() |>
    transmute(
      navn = localName,
      dato = as.Date(date)
    )
  
  helligdage <- bind_rows(helligdage, helligdage_aar)
}

# Tjek
print(head(helligdage))

### GØR VFF-KAMPDATO KLAR TIL MERGE


# LOAD VFFs hjemmekampe  --------------------------------------------------

VFF_Hjemmekampe <- read_rds("VFFdata/superstatsvff.rds")
view(VFF_Hjemmekampe)


# TILFØJ HELLIGDAGSINFORMATION TIL VFF-KAMPDATA ---------------------------

view(VFF_Hjemmekampe)
view(helligdage)

class(VFF_Hjemmekampe$tidsstempel)   # returnere POSIXct
class(helligdage$dato)             # returnere Date

# Vi konverterer POSXICT til Date
VFF_Hjemmekampe1 <- VFF_Hjemmekampe |> 
  mutate(
    Dato_join = as.Date(tidsstempel)  # kun dato, fjern tid
  )

view(VFF_Hjemmekampe1)

# Left join for at tilføje helligdagsinformation
VFF_Komplet <- VFF_Hjemmekampe1 |> 
  left_join(
    helligdage,
    by = c("Dato_join" = "dato")
  ) |> 
  # Opret en binær variabel for om det er helligdag
  mutate(
    Er_helligdag = if_else(!is.na(navn), 1, 0),
    Helligdag_navn = navn
  ) |> 
  # Fjern den midlertidige 'navn' kolonne
  dplyr::select(-navn)

# Se resultatet
View(VFF_Komplet)
glimpse(VFF_Komplet)

# Tjek hvor mange kampe der er på helligdage
VFF_Komplet |> 
  count(Er_helligdag)
# 15 helligdage

# Vi fjerner nu kolonnen dato_join og filtre fra sæson 2003/2004
VFF_Komplet <- VFF_Komplet |> 
  dplyr::select(-Dato_join) |> 
  filter(as.numeric(str_extract(sæson, "^\\d{4}")) >= 2003)

# Se resultatet igen
View(VFF_Komplet)
glimpse(VFF_Komplet)


# Tilføjelse af feriedage -------------------------------------------------

VFF_Komplet1 <- VFF_Komplet |>
  mutate(
    # Konverter tidsstempel til Date
    Dato_kamp = as_date(ymd_hms(tidsstempel)),  # eller ymd_hms(tidsstempel) for POSIXct
    
    # Udtræk måned og uge
    maaned = month(Dato_kamp),
    uge    = isoweek(Dato_kamp),
    
    # Ferievariabler
    i_sommerferie   = Dato_kamp >= make_date(year(Dato_kamp), 7, 1) &
      Dato_kamp <  make_date(year(Dato_kamp), 8, 15),
    i_efterårsferie = uge == 42 | (uge == 41 & wday(Dato_kamp, week_start = 1) >= 6),
    i_vinterferie   = uge == 7,
    ferieperiode    = case_when(
      i_sommerferie   ~ "Sommerferie",
      i_efterårsferie ~ "Efterårsferie",
      i_vinterferie   ~ "Vinterferie",
      TRUE            ~ "Ingen"
    ),
    
    # Tidspunkt for kamp
    Tid_kamp = format(ymd_hms(tidsstempel), "%H:%M"))
    
head(paste0(VFF_Komplet1$dag_maaned, "/", VFF_Komplet1$kamp_aar))
head(VFF_Komplet$tidsstempel, 10)

view(VFF_Komplet1)


# Gem som RDS-fil ---------------------------------------------------------

saveRDS(VFF_Komplet1, file = "VFFdata/VFF_Komplet.rds")



