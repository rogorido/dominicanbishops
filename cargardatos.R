# existe la fución cortarConexiones que está cargada en .Rprofile

con <- dbConnect(drv= "PostgreSQL", dbname = "dominicos", user="igor", host="localhost")

# duraciones 
sql <- "SELECT DISTINCT bishop_id, EXTRACT(year from date_nomination) as inicio, EXTRACT(year from date_end) as fin,
       COALESCE(
                SUM(EXTRACT(year from date_end)
                                 -
                    EXTRACT(year from date_nomination)), 0) AS duracion
FROM bishops
WHERE date_nomination is not null and  date_end is not null
group by bishop_id;"

duracion <- dbGetQuery(con, sql)

bis_per_dioc <- dbGetQuery(con, "SELECT d.diocese_id, d.diocese_name as diocesis,
         p.longitude, p.latitude, count(b.bishop_id) as total 
  FROM bishops b
       JOIN dioceses d
       ON d.diocese_id = b.diocese_id
       JOIN places p
       ON p.place_id = d.place_id
  GROUP BY d.diocese_id, diocesis, p.longitude, p.latitude;")


# todos los obispos no-seculares 
sql <- "select * from b_all_withoutseculars_period"

bish_todos_sinseculares_conafil <- dbGetQuery(con, sql)
total_b_sinseculares_conafil <- dim(bish_todos_sinseculares_conafil)[1]

bish_todos_sinseculares_sinafil <- filter(bish_todos_sinseculares_conafil, affiliated == FALSE)
total_b_sinseculares_sinafil <- dim(bish_todos_sinseculares_sinafil)[1]

###
# Obispso por diócesis con las órdenes más importantes 
## tabla de datos absolutos
sql <- "  WITH a AS 
       (SELECT d.diocese_name, religious_order, COUNT(*) AS cuenta 
               FROM vistas.b_all_withoutseculars_period_most_important b
               JOIN dioceses d USING (diocese_id)
               GROUP BY d.diocese_name, religious_order)
  SELECT a.diocese_name, religious_order, cuenta, cuenta/SUM(cuenta)
  OVER (PARTITION BY a.diocese_name)
  FROM a
  ORDER BY a.diocese_name;"

bish_todos_sinseculares_conafil_por_diocesis <- dbGetQuery(con, sql)
# quitamos la columna de los porcentajes
bish_todos_sinseculares_conafil_por_diocesis <- select(bish_todos_sinseculares_conafil_por_diocesis, -4)
# lo convertirmos en wide
bish_todos_sinseculares_conafil_por_diocesis_totales <- spread(bish_todos_sinseculares_conafil_por_diocesis, religious_order, cuenta)

## tabla con datos relativos
## ie, muestra el porcentaje de obispos de esa orden del total de obispso
## pero solamente considerando obispos regulares 
bish_todos_sinseculares_conafil_por_diocesis <- dbGetQuery(con, sql)
# cambiamos el nombre porque sale com ?column?
colnames(bish_todos_sinseculares_conafil_por_diocesis)[4] <- "porcentaje"
# quitamos la columna de la cuenta 
bish_todos_sinseculares_conafil_por_diocesis <- select(bish_todos_sinseculares_conafil_por_diocesis, -cuenta)
# redondeamso
bish_todos_sinseculares_conafil_por_diocesis$porcentaje <- round(bish_todos_sinseculares_conafil_por_diocesis$porcentaje, 2)

# lo convertirmos en wide
bish_todos_sinseculares_conafil_por_diocesis_porcentaje <- spread(bish_todos_sinseculares_conafil_por_diocesis, religious_order, porcentaje)


###
# Obispso seculares vs. no-seculares: totales y porcentajes por diócesis
####
sql <- "  WITH x AS (
         SELECT diocese_ID, 'sinorder' AS orden, COUNT(*) AS total 
         FROM bishops_ALL b 
         WHERE religious_ORDER IS NULL
         GROUP BY diocese_ID, religious_ORDER
               UNION
         SELECT diocese_id, 'conorder' AS orden, COUNT(*) AS total 
         FROM bishops_ALL b 
         WHERE religious_ORDER IS NOT NULL
         GROUP BY diocese_ID
         )
  SELECT diocese_id, d.diocese_name, p.longitude, p.latitude,
         x.orden, total, total/sum(total)
  OVER (PARTITION BY d.diocese_name)
  FROM dioceses d
  JOIN x USING (diocese_ID)	
  JOIN places p ON p.place_id = d.place_id
  ORDER BY d.diocese_name;"

bish_todos_sec_vs_nonsec <- dbGetQuery(con, sql)

colnames(bish_todos_sec_vs_nonsec)[7] <- "porcentaje"

# y ahora hacemos dos variables por wide 
bish_todos_sec_vs_nonsec_totales <- bish_todos_sec_vs_nonsec %>%
    select(-porcentaje) %>%
    spread(orden, total) %>%
    arrange(diocese_name)

bish_todos_sec_vs_nonsec_porc <- bish_todos_sec_vs_nonsec %>%
    select(-total) %>%
    spread(orden, porcentaje) %>%
    arrange(diocese_name)

source('./edadmoderna.R')
