
sql <- getSQL("../sql/edm/diocesis_rents_edm.sql")
totalrents_ops <- dbGetQuery(con, sql)

sql <- getSQL("../sql/edm/diocesis_rents_all.sql")
totalrents_all <- dbGetQuery(con, sql)

# me salen 261 a pesar de q hay muchas más, habría qmirar cuáles no salen

################################################
# Italia
################################################

totalrents_ops.italy <- filter(totalrents_ops, pais == 'Italy')

Desc(totalrents_ops.italy$mesa_media)
Desc(totalrents_ops.italy$tasa_media)

# estas son las diocess totales de italia
totalrents_all.italy <- filter(totalrents_all, pais == 'Italy')

Desc(totalrents_all.italy$mesa_media)
Desc(totalrents_all.italy$tasa_media)

# ahora miramos lo de lospercentiles
# primero > 0.75 pero tenemos q poner 0.75, 1
# pq escoge lo q está entre esos valores 
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0.75, 1, "Italy"))
rents_percentile_high.italy <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_high.italy$ops <- factor(rents_percentile_high.italy$ops)
Desc(rents_percentile_high.italy$ops)

rents_percentile_high.italy.sf <- st_as_sf(rents_percentile_high.italy,
                                           coords = c("longitude", "latitude"),
                                           agr = "constant")
st_write(rents_percentile_high.italy.sf,
         "/home/igor/geschichte/artikel/obisposdominicos/gis/geojson/rents_percentile_high.italy_sf.geojson",
         append = FALSE)

# ahora miramos lo de lospercentiles

# ahora el top 10
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0.9, 1, "Italy"))
rents_percentile_veryhigh.italy <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_veryhigh.italy$ops <- factor(rents_percentile_veryhigh.italy$ops)
Desc(rents_percentile_veryhigh.italy$ops)

rents_percentile_veryhigh.italy.sf <- st_as_sf(rents_percentile_veryhigh.italy,
                                           coords = c("longitude", "latitude"),
                                           agr = "constant")
st_write(rents_percentile_veryhigh.italy.sf,
         "/home/igor/geschichte/artikel/obisposdominicos/gis/geojson/rents_percentile_veryhigh.italy_sf.geojson",
         append = FALSE)


# y ahora el 0.25
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0, 0.25, "Italy"))
rents_percentile_low.italy <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_low.italy$ops <- factor(rents_percentile_low.italy$ops)
Desc(rents_percentile_low.italy$ops)

# y ahora el 0.1
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0, 0.1, "Italy"))
rents_percentile_verylow.italy <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_verylow.italy$ops <- factor(rents_percentile_verylow.italy$ops)
Desc(rents_percentile_verylow.italy$ops)


################################################
# Adriático conturquía
################################################
totalrents_ops.adriatico <- filter(totalrents_ops, pais %in% c_balcans_with_turkey)

Desc(totalrents_ops.adriatico$mesa_media)
Desc(totalrents_ops.adriatico$tasa_media)

# estas son las diocess totales
totalrents_all.adriatico <- filter(totalrents_all, pais %in% c_balcans_with_turkey)

Desc(totalrents_all.adriatico$mesa_media)
Desc(totalrents_all.adriatico$tasa_media)


################################################
# Francia
################################################

totalrents_ops.france <- filter(totalrents_ops, pais == 'France')

Desc(totalrents_ops.france$mesa_media)
Desc(totalrents_ops.france$tasa_media)

# estas son las diocess totales de francia
totalrents_all.france <- filter(totalrents_all, pais == 'France')

Desc(totalrents_all.france$mesa_media)
Desc(totalrents_all.france$tasa_media)

# ahora miramos lo de lospercentiles
# primero > 0.75 pero tenemos q poner 0.75, 1
# pq escoge lo q está entre esos valores
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0.75, 1, "France"))
rents_percentile_high.france <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_high.france$ops <- factor(rents_percentile_high.france$ops)
Desc(rents_percentile_high.france$ops)

rents_percentile_high.france.sf <- st_as_sf(rents_percentile_high.france,
                                           coords = c("longitude", "latitude"),
                                           agr = "constant")
st_write(rents_percentile_high.france.sf,
         "/home/igor/geschichte/artikel/obisposdominicos/gis/geojson/rents_percentile_high.france_sf.geojson",
         append = FALSE)

# ahora miramos lo de lospercentiles

# ahora el top 10
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0.9, 1, "France"))
rents_percentile_veryhigh.france <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_veryhigh.france$ops <- factor(rents_percentile_veryhigh.france$ops)
Desc(rents_percentile_veryhigh.france$ops)

rents_percentile_veryhigh.france.sf <- st_as_sf(rents_percentile_veryhigh.france,
                                           coords = c("longitude", "latitude"),
                                           agr = "constant")
st_write(rents_percentile_veryhigh.france.sf,
         "/home/igor/geschichte/artikel/obisposdominicos/gis/geojson/rents_percentile_veryhigh.france_sf.geojson",
         append = FALSE)


# y ahora el 0.25
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0, 0.25, "France"))
rents_percentile_low.france <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_low.france$ops <- factor(rents_percentile_low.france$ops)
Desc(rents_percentile_low.france$ops)

# y ahora el 0.1
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0, 0.1, "France"))
rents_percentile_verylow.france <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_verylow.france$ops <- factor(rents_percentile_verylow.france$ops)
Desc(rents_percentile_verylow.france$ops)

################################################
# Ireland
################################################

totalrents_ops.ireland <- filter(totalrents_ops, pais == 'Ireland')

Desc(totalrents_ops.ireland$mesa_media)
Desc(totalrents_ops.ireland$tasa_media)

# estas son las diocess totales de francia
totalrents_all.ireland <- filter(totalrents_all, pais == 'Ireland')

Desc(totalrents_all.ireland$mesa_media)
Desc(totalrents_all.ireland$tasa_media)

# ahora miramos lo de lospercentiles
# primero > 0.75 pero tenemos q poner 0.75, 1
# pq escoge lo q está entre esos valores
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0.75, 1, "Ireland"))
rents_percentile_high.ireland <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_high.ireland$ops <- factor(rents_percentile_high.ireland$ops)
Desc(rents_percentile_high.ireland$ops)

rents_percentile_high.ireland.sf <- st_as_sf(rents_percentile_high.ireland,
                                           coords = c("longitude", "latitude"),
                                           agr = "constant")
st_write(rents_percentile_high.ireland.sf,
         "/home/igor/geschichte/artikel/obisposdominicos/gis/geojson/rents_percentile_high.ireland_sf.geojson",
         append = FALSE)

# ahora miramos lo de lospercentiles

# ahora el top 10
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0.9, 1, "Ireland"))
rents_percentile_veryhigh.ireland <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_veryhigh.ireland$ops <- factor(rents_percentile_veryhigh.ireland$ops)
Desc(rents_percentile_veryhigh.ireland$ops)

rents_percentile_veryhigh.ireland.sf <- st_as_sf(rents_percentile_veryhigh.ireland,
                                           coords = c("longitude", "latitude"),
                                           agr = "constant")
st_write(rents_percentile_veryhigh.ireland.sf,
         "/home/igor/geschichte/artikel/obisposdominicos/gis/geojson/rents_percentile_veryhigh.ireland_sf.geojson",
         append = FALSE)


# y ahora el 0.25
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0, 0.25, "Ireland"))
rents_percentile_low.ireland <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_low.ireland$ops <- factor(rents_percentile_low.ireland$ops)
Desc(rents_percentile_low.ireland$ops)

# y ahora el 0.1
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0, 0.1, "Ireland"))
rents_percentile_verylow.ireland <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_verylow.ireland$ops <- factor(rents_percentile_verylow.ireland$ops)
Desc(rents_percentile_verylow.ireland$ops)


################################################
# Spain
################################################

totalrents_ops.spain <- filter(totalrents_ops, pais == 'Spain')

Desc(totalrents_ops.spain$mesa_media)
Desc(totalrents_ops.spain$tasa_media)

# estas son las diocess totales de italia
totalrents_all.spain <- filter(totalrents_all, pais == 'Spain')

Desc(totalrents_all.spain$mesa_media)
Desc(totalrents_all.spain$tasa_media)

# ahora miramos lo de lospercentiles
# primero > 0.75 pero tenemos q poner 0.75, 1
# pq escoge lo q está entre esos valores
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0.75, 1, "Spain"))
rents_percentile_high.spain <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_high.spain$ops <- factor(rents_percentile_high.spain$ops)
Desc(rents_percentile_high.spain$ops)

rents_percentile_high.spain.sf <- st_as_sf(rents_percentile_high.spain,
                                           coords = c("longitude", "latitude"),
                                           agr = "constant")
st_write(rents_percentile_high.spain.sf,
         "/home/igor/geschichte/artikel/obisposdominicos/gis/geojson/rents_percentile_high.spain_sf.geojson",
         append = FALSE)

# ahora miramos lo de lospercentiles

# ahora el top 10
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0.9, 1, "Spain"))
rents_percentile_veryhigh.spain <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_veryhigh.spain$ops <- factor(rents_percentile_veryhigh.spain$ops)
Desc(rents_percentile_veryhigh.spain$ops)

rents_percentile_veryhigh.spain.sf <- st_as_sf(rents_percentile_veryhigh.spain,
                                           coords = c("longitude", "latitude"),
                                           agr = "constant")
st_write(rents_percentile_veryhigh.spain.sf,
         "/home/igor/geschichte/artikel/obisposdominicos/gis/geojson/rents_percentile_veryhigh.spain_sf.geojson",
         append = FALSE)


# y ahora el 0.25
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0, 0.25, "Spain"))
rents_percentile_low.spain <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_low.spain$ops <- factor(rents_percentile_low.spain$ops)
Desc(rents_percentile_low.spain$ops)

# y ahora el 0.1
sql <- getSQL("../sql/edm/diocesis_rents_percentile.sql")
rs = dbSendQuery(con, sql, params = list(0, 0.1, "Spain"))
rents_percentile_verylow.spain <- dbFetch(rs)
dbClearResult(rs)

rents_percentile_verylow.spain$ops <- factor(rents_percentile_verylow.spain$ops)
Desc(rents_percentile_verylow.spain$ops)
