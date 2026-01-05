#Vi starter med at indlæse de nødvendige pakker. 
#Tidyverse bruges til databehandling.
#RSQlite bruges til at arbejde med databaser.
pacman::p_load("tidyverse", "RSQLite")

# 1. Oprettelse af SQLite database  ---------------------------------------
if (!dir.exists("VFFdata")) dir.create("VFFdata")

#Vi opretter forbindelse til SQLite databasen. 
connection_VFFdata <- dbConnect(SQLite(), "VFFdata/fodbolddata.sqlite")

#Indlæs og rens data
#Vi indlæser VFF kampdata og fjerner helligdag-navn. 
VFF_Komplet <- readRDS("VFFdata/VFF_Komplet.rds") |> 
  select(-Helligdag_navn) 

#Vi indlæser DMI vejrdata og fjerner rækker uden kampdatoer. 
DMI_data <- readRDS("VFFdata/dmi.rds") |> 
  filter(!is.na(kamp_dato))

#Indløser Transfermarkt_data og fjerner rækker uden sæson informationer
Transfermarkt_data <- readRDS("VFFdata/TransfermarktData.rds") |> 
  filter(!is.na(season_format))

#Indlæser informationer om tilskuerne vffkort 
data_vffkort01_rds <- readRDS("VFFdata/vffkort01.rds") 

#Indlæser informationer om modstanderne med fcidk. 
data_fcidk_rds <- readRDS("VFFdata/fcidk.rds")

#Vi smider nu data i SQLite
#dbWriteTable = Vi gemmer en dataframe permanent i en database som en tabel. 
#overwrite = Vi sikre at den gamle data overskrives hver gang vi kører koden. 
dbWriteTable(connection_VFFdata, "vff", data_vffkort01_rds, overwrite = TRUE)
dbWriteTable(connection_VFFdata, "fci", data_fcidk_rds, overwrite = TRUE)
dbWriteTable(connection_VFFdata, "DMI_data", DMI_data, overwrite = TRUE)
dbWriteTable(connection_VFFdata, "VFF_Komplet", VFF_Komplet, overwrite = TRUE)
dbWriteTable(connection_VFFdata, "Transfermarkt_data", Transfermarkt_data, overwrite = TRUE)

#Vi ser alle tabeller der findes i databasen. 
dbListTables(connection_VFFdata)

#Vi lukker databasen. 
dbDisconnect(connection_VFFdata)

# 2. Query database -------------------------------------------------------

#vi genåbner forbindelsen til databasen. 
connection_VFFdata <- dbConnect(SQLite(), "VFFdata/fodbolddata.sqlite")

#dbGetQuery henter data fra databasen og laver dem om til en dataframe. 
#Vi henter alle kolonner med SELECT
SQL_DMI <- dbGetQuery(connection_VFFdata, "SELECT * FROM DMI_data")
SQL_VFF_Komplet <- dbGetQuery(connection_VFFdata, "SELECT * FROM VFF_Komplet")
SQL_Transfermarkt <- dbGetQuery(connection_VFFdata, "SELECT * FROM Transfermarkt_data")
SQL_vff <- dbGetQuery(connection_VFFdata, "SELECT * FROM vff")
SQL_fci <- dbGetQuery(connection_VFFdata, "SELECT * FROM fci")

#Vi lukker databasen. 
dbDisconnect(connection_VFFdata)

# 3. Join ---------------------------------------------------------------

#Vi åbner forbindelsen. 
connection_VFFdata <- dbConnect(SQLite(), "VFFdata/fodbolddata.sqlite")

#Vi samler alle kampdata og får en komplet tabel over alle kampe. 
sql_samlet <- dbGetQuery(connection_VFFdata, "
SELECT
    datetime(D.kamp_dato, 'unixepoch') AS kampdato,
    
    D.temp_dry,
    D.wind_speed,
    D.nedbør_seneste_7_timer,
    
    V.ugedag,
    V.hjemmehold, 
    V.udehold,
    V.mål_hjemme,
    V.mål_ude,
    V.tilskuere,
    dommer, 
    V.point_hjemmehold,
    V.point_udehold,
    V.placering_lag_hjemme,
    V.placering_lag_ude,
    V.mål_seneste_3_hjemme,
    V.mål_sæson_lag_hjemmehold,
    V.mål_sæson_lag_udehold,
    V.form_seneste_3_hjemmehold,
    V.form_seneste_3_udehold,
    V.udnyttelsesgrad_tilskuere,
    V.tilskuere_hold_lag,
    V.udehold_rang,
    V.sæson,
    V.runde,
    V.tidsperiode,
    V.Er_helligdag,
    V.i_sommerferie,
    V.i_efterårsferie,
    V.i_vinterferie,
    V.ferieperiode,

    T.total_income,
    T.total_expense,
    T.net_balance,
    T.antal_transfers,

    C.d10_tilskuere,
    C.d7_tilskuere,
    C.d3_tilskuere,

    F.navn AS udehold_navn,
    F.stadion,
    F.sponsor_stadion_navn,
    F.tilskuere AS stadion_kapacitet

FROM VFF_Komplet AS V

LEFT JOIN DMI_data AS D
    ON D.kamp_dato = V.tidsstempel

LEFT JOIN Transfermarkt_data AS T
    ON V.sæson = T.season_format

LEFT JOIN (
    SELECT
        sæson,
        runde,
        tilskuere,
        MAX(d10_tilskuere) AS d10_tilskuere,
        MAX(d7_tilskuere)  AS d7_tilskuere,
        MAX(d3_tilskuere)  AS d3_tilskuere
    FROM vff
    GROUP BY sæson, runde, tilskuere
) AS C
    ON V.sæson = C.sæson
   AND V.runde = C.runde
   AND V.tilskuere = C.tilskuere

LEFT JOIN fci AS F
    ON CASE
         WHEN V.udehold = 'SJF' THEN 'SDR'
         ELSE V.udehold
       END = F.kort

WHERE V.sæson BETWEEN '2003/2004' AND '2025/2026'
")

#Vi lukker databasen. 
dbDisconnect(connection_VFFdata)


# 4. Vi gemmer som RDS ---------------------------------------------------------
saveRDS(sql_samlet, "VFFdata/VFF_samlet_variabler.rds") 





