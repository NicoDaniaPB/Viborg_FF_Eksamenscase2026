pacman::p_load("tidyverse", "RSQLite")

# 1. Oprettelse af SQLite database ---------------------------------------

if (!dir.exists("VFFdata")) dir.create("VFFdata")

connection_VFFdata <- dbConnect(SQLite(), "VFFdata/fodbolddata.sqlite")

# Indlæs og rens data
VFF_Komplet <- readRDS("VFFdata/VFF_Komplet.rds") |> 
  select(-Helligdag_navn) 

DMI_data <- readRDS("VFFdata/dmi.rds") |> 
  filter(!is.na(kamp_dato))

Transfermarkt_data <- readRDS("VFFdata/TransfermarktData.rds") |> 
  filter(!is.na(season_format))

data_vffkort01_rds <- readRDS("VFFdata/vffkort01.rds") 

data_fcidk_rds <- readRDS("VFFdata/fcidk.rds")

# Vi smider data i SQLite
dbWriteTable(connection_VFFdata, "vff", data_vffkort01_rds, overwrite = TRUE)
dbWriteTable(connection_VFFdata, "fci", data_fcidk_rds, overwrite = TRUE)
dbWriteTable(connection_VFFdata, "DMI_data", DMI_data, overwrite = TRUE)
dbWriteTable(connection_VFFdata, "VFF_Komplet", VFF_Komplet, overwrite = TRUE)
dbWriteTable(connection_VFFdata, "Transfermarkt_data", Transfermarkt_data, overwrite = TRUE)

dbListTables(connection_VFFdata)

dbDisconnect(connection_VFFdata)

# 2. Query database -------------------------------------------------------

connection_VFFdata <- dbConnect(SQLite(), "VFFdata/fodbolddata.sqlite")
SQL_DMI <- dbGetQuery(connection_VFFdata, "SELECT * FROM DMI_data")
SQL_VFF_Komplet <- dbGetQuery(connection_VFFdata, "SELECT * FROM VFF_Komplet")
SQL_Transfermarkt <- dbGetQuery(connection_VFFdata, "SELECT * FROM Transfermarkt_data")
SQL_vff <- dbGetQuery(connection_VFFdata, "SELECT * FROM vff")
SQL_fci <- dbGetQuery(connection_VFFdata, "SELECT * FROM fci")
dbDisconnect(connection_VFFdata)

# 3. Join ---------------------------------------------------------------

connection_VFFdata <- dbConnect(SQLite(), "VFFdata/fodbolddata.sqlite")

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

dbDisconnect(connection_VFFdata)


# 4. Gem som RDS ---------------------------------------------------------

saveRDS(sql_samlet, "VFFdata/VFF_samlet_variabler.rds")

