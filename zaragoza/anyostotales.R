# años totales q están en las diócesis

sql <- "SELECT diocese_name, total, porcentaje
FROM qgis.anyostotales
ORDER BY porcentaje;"

anyostotales  <- dbGetQuery(con, sql)

ggplot(anyostotales, aes(x = total)) +
    geom_histogram()

Desc(anyostotales$total)

quantile(anyostotales$total, na.rm = T)

## Desc(totalyears$total)
## Desc(totalyears.agg$yearsagg)

## totalyears_filtered <- totalyears %>% drop_na()

## totalyears_sf <- st_as_sf(totalyears_filtered,
##                           coords = c("longitude", "latitude"),
##                           agr = "constant")
## st_write(totalyears_sf, "totalyears_sf.geojson", append = FALSE)
