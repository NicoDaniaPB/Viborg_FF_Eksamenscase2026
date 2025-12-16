library(tidyverse)
library(RSQLite)


data_vffkort01_rds <- readRDS("VFFdata/vffkort01.rds") 
view(data_vffkort01_rds)

data_fcidk_rds <- readRDS("VFFdata/fcidk.rds")
view(data_fcidk_rds)

con <- dbConnect(SQLite(), "VFFdata/fodbolddata.sqlite")

dbWriteTable(con, "vff", data_vffkort01_rds, overwrite = TRUE)
dbWriteTable(con, "fci", data_fcidk_rds, overwrite = TRUE)

dbListTables(con)
dbReadTable(con, "vff") |> View()
dbReadTable(con, "fci") |> View()

dbDisconnect(con)
