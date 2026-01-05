pacman::p_load("tidyverse", "rvest", "stringr", "janitor", "lubridate", "RSQLite")

# DMI-API -----------------------------------------------------------------

# Vi starter med at indlæse Superstatsdata, da kampdatoerne skal bruges til at hente vejrdata.
vff_hjemmekampe <- read_rds("VFFdata/superstatsvff.rds")

# Når man har en .Renviron-fil, så indlæses den her:
if (file.exists(".Renviron")) readRenviron(".Renviron")

#En .Renviron-fil bruges til, at R automatisk kan læse værdier (feks. API-nøgler), selvom de ikke er synlige i selve koden.


#> Sys.getenv() bruges i R til at hente oplysninger gemt i f.eks. .Renviron.
#> Nøglen er døbt MY_API_KEY i .Renviron og derfor den der hentes.
#> Den returerner i dette tilfælde en API-nøgle til DMI, da det er den der er gemt i filen. 
api_nøgle <- Sys.getenv("MY_API_KEY")


#> Kampdatoerne for VFF's hjemmekampe gemmes i kamp_datoer.
#> Skal bruges i et loop, så der kun kommer vejrobservationer fra DMI på kampdage.
kamp_datoer <- vff_hjemmekampe$tidsstempel

# Opretter en tom liste og gemmer i dmi_list. 
dmi_list <- list()   

#> Looper gennem alle kampdatoer en ad gangen og henter DMI data.
#> seq_along(kamp_datoer) laver en sekvens af 1, 2, 3, osv. 
#> i bliver derfor hvert enkelt kampnummer i rækkefølgen. 
for (i in seq_along(kamp_datoer)) {
  #> Formaterer kampens tidsstempel til det format DMI-API'en kræver.
  #> format() omdanner dato/tid til en tekststreng i et bestemt format.
  dato_format <- format(kamp_datoer[i], "%Y-%m-%dT%H:%M:%SZ")
  #> URL-koder datoen så den kan bruges i API-kaldet.
  #> URLencode() oversætter tegn som specialtegn.
  #> F.eks. bliver 2023-07-12 19:00 til "2023-07-12%2019%3A00".så datoerne kan anvendes i URL'en.
  dato_encode <- URLencode(dato_format)
  #> Bygger URL'en der henter temperatur og vind for kampen index i. Alle kampe. 
  #> paste0() sætter tekst sammen uden mellemrum 
  url_temp_vind <- paste0(
    "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
    "stationId=06060",                 
    "&datetime=", dato_encode,
    "&api-key=", "d074f877-5fe5-44d1-a473-21a6be80a0c2")
  #> Henter DMI’s data for kampene.
  #> httr::GET() sender en HTTP-forespørgsel af typen GET.
  #> Det betyder, at serveren bliver bedt om at sende data fra den angivne URL.
  api_kald <- httr::GET(url_temp_vind)
  #> httr::content() trækker indholdet ud som tekst.
  #> Læser API-svaret som ren JSON-tekst
  tekst <- httr::content(api_kald, "text", encoding = "UTF-8")
  #> jsonlite::fromJSON() omdanner JSON-tekst til en R-liste
  #> Konverterer data så de kan gemmes i dmi_list.
  dmi_list[[i]] <- jsonlite::fromJSON(tekst)
  #> cat() skriver til konsollen.
  #> Viser løbende hvilket tidspunkt der hentes data for.
  cat("Henter data for:", dato_format, "\n")
}

#> lapply() kører en funktion på hvert element listen og returnerer en ny liste.
#> function(x) ... hvad der skal trækkes ud fra hvert liste-element.
#> x$features$properties henter den del af JSON-dataen,
#> hvor DMI gemmer selve målingerne (temperatur, vind og andre variabler)
#> $ bruges til at tilgå properties i listen.
#> Resultatet bliver en ny liste (dmi_properties),
#> hvor hvert element indeholder properties for en kamp.
dmi_properties <- lapply(dmi_list, function(x) x$features$properties)

#> bind_rows() samler alle liste-elementer i en samlet data frame.
#> .id = "kamp" tilføjer en kolonne med kampnummer.
dmi_df <- bind_rows(dmi_properties, .id = "kamp") |>
  #> mutate() tilføjer/ændrer kolonner:
  #> kamp = konverterer kamp-id til heltal as.integer.
  #> kamp_dato = henter korrekt kampdato med kampnummeret.
  mutate(kamp = as.integer(kamp), kamp_dato = kamp_datoer[kamp])

#> filter() vælger kun rækker, der opfylder betingelsen:
#> %in% tester om parameterId findes i den angivne liste og
#> beholder kun de to parametre, som skal bruges = temperatur og vind.
dmi_df <- dmi_df |>
  filter(parameterId %in% c("temp_dry", "wind_speed"))

#> group_by() grupperer data efter kamp og parameter = temperatur/vind.
#> slice_max() vælger den senest obsaverede måling.
#> ungroup() fjerner gruppering så df fungerer normalt igen.
#> Ender med en temperatur og en vindmåling per kamp. 
dmi_df <- dmi_df |>
  group_by(kamp, parameterId) |>
  slice_max(observed) |>
  ungroup() 

dmi_df <- dmi_df |>
  #> select() vælger kun de kolonner, der skal bruges videre i tabellen.
  #> value = den målte værdi = temperatur eller vindstyrke.
  select(kamp, kamp_dato, parameterId, value) |>
  #> pivot_wider() omdanner data fra vertikalt format til horisontalt format.
  #> id_cols angiver hvilke kolonner der skal være unikke rækker = kamp + dato.
  pivot_wider(id_cols = c(kamp, kamp_dato),
              # names_from bestemmer hvilke værdier der bliver til nye kolonnenavne = temp/vind.          
              names_from = parameterId,
              # values_from siger hvor måleværdierne skal hentes fra.
              values_from = value)


# Funktion der henter den samlede nedbør 7 timer før en kamp.
nedbør_7t <- function(dt) {
  
  # format() laver dato/tid om til tekst i API-formatet "YYYY-MM-DDTHH:MM:SSZ"
  start <- format(dt - 7*3600, "%Y-%m-%dT%H:%M:%SZ")    # tidspunkt 7 timer før kamp start måling.
  slut  <- format(dt,           "%Y-%m-%dT%H:%M:%SZ")   # tidspunkt for kampstart slutmåling. 
  
  # paste0() sætter URL'en sammen.
  url_nedbør <- paste0(
    "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?",
    "stationId=06060",                                  # Station Karup.
    "&parameterId=precip_past1h",                       # Nedbør målt pr time.
    "&datetime=", start, "/", slut,                     # Tidsinterval.
    "&limit=8",                                         # Max antal observationer.
    "&api-key=", "d074f877-5fe5-44d1-a473-21a6be80a0c2")                             # API-nøgle.
  
  #> httr::GET() henter data fra API.
  #> httr::content() trækker selve svaret ud som tekst.
  tekst1 <- httr::content(httr::GET(url_nedbør), "text", encoding = "UTF-8")
  
  # jsonlite::fromJSON() forsøger at omdanne JSON-tekst til en liste.
  js <- try(jsonlite::fromJSON(tekst1, simplifyVector = TRUE), silent = TRUE)
  
  # Hvis der er fejl i JSON'en (fx tomt svar) returnerer vi NA.
  if (inherits(js, "try-error")) return(NA)
  if (is.null(js$features) || length(js$features) == 0) return(NA)
  
  # Henter selve nedbørsværdierne fra properties$value.
  values <- js$features$properties$value
  
  # Udskriver status i konsollen.
  cat("Henter 7-timers nedbør for:", format(dt, "%Y-%m-%d %H:%M"), "\n")
  
  # Summerer nedbør over perioden (NA ignoreres).
  sum(values, na.rm = TRUE)
}

# Beregn nedbør for alle kampdatoer ved at køre funktionen på hver dato.
nedbør7t <- sapply(kamp_datoer, nedbør_7t)

# Tilføj nedbørsværdien til dmi_df ved at matche på kampnummer.
dmi_df$nedbør_seneste_7_timer  <- nedbør7t[dmi_df$kamp]

# Sorter tabellen efter dato, så de nyeste kampe kommer øverst = faldende rækkefølge.
dmi_df <- dmi_df |>
  arrange(desc(kamp_dato))


# Join med de andre to variabler. 
dmi_df <- dmi_df |>
  left_join(
    vff_hjemmekampe |> 
      mutate(kamp = row_number()) |> 
      select(kamp),     # Joiner kun på kamp.
    by = "kamp")


# Gemmer ændringer -------------------------------------------------------------

saveRDS(dmi_df, "VFFdata/dmi.rds")

#Vi indlæser data igen 
dmi_df <- readRDS("VFFdata/dmi.rds")

#Vi ser tabellen 
view(dmi_df) 
