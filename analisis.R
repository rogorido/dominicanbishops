library(RPostgreSQL)
library(DescTools)

con <- dbConnect(drv= "PostgreSQL", dbname = "dominicos", user="igor", host="localhost")

papas <- dbGetQuery(con, "select * from analysis.bishops_per_pope_days")

papas$media <- papas$duracion / papas$count
# quitamos infinitos
papas$media[!is.finite(papas$media)] <- NA 

# papas dependiendo de la orden religiosa
sql <- 'select bishop_id, religious_order
  from bishops 
  left join popes
       on popes.pope_id = bishops.pope_id;'

org <- dbGetQuery(con, sql)
Desc(org)

# por países
sql <- 'SELECT b.bishop_id, country AS pais FROM bishops b
JOIN dioceses d
     ON b.diocese_id = d.diocese_id
JOIN places p
     ON p.place_id = d.place_id;'

geogr <- dbGetQuery(con, sql)
Desc(geogr)

## confesionalización por años en países
sql <- 'select b.bishop_id, p.country, 
                EXTRACT(year from b.date_end)
                                 -
                    EXTRACT(year from b.date_nomination) AS duracion
FROM bishops b
JOIN dioceses d
     ON b.diocese_id = d.diocese_id
JOIN places p 
     ON d.place_id = p.place_id;'

## cantidad de años en los obispados en el período confesional
sql <- "select b.bishop_id, p.country, 
                EXTRACT(year from b.date_end)
                                 -
                    EXTRACT(year from b.date_nomination) AS duracion
FROM bishops b
JOIN dioceses d
     ON b.diocese_id = d.diocese_id
JOIN places p 
     ON d.place_id = p.place_id
  WHERE b.date_nomination > '1560-01-01' and b.date_nomination < '1660-01-01';"

confe_por_pais <- dbGetQuery(con, sql)

## obispos en cada año

sql <- "  WITH anyos AS
        (SELECT generate_series(1200, 1800, 1) as serie )
  SELECT serie, (SELECT count(*) FROM bishops
         WHERE serie BETWEEN extract(year from date_nomination) AND extract(year from date_end))
  FROM anyos;"

obispos_por_ano <- dbGetQuery(con, sql)
ggplot(obispos_por_ano, aes(x=serie, y=count)) + geom_line()

sql <- "WITH anyos AS
      (SELECT generate_series(1200, 1800, 1) as serie ),
      italia as
      (SELECT diocese_id FROM dioceses JOIN places USING (place_id)
      	      		 WHERE country = 'Italy')
select serie, (SELECT count(*) from bishops JOIN italia USING (diocese_id)
       where serie between extract(year from date_nomination) and extract(year from date_end))
from anyos;"

obispos_por_ano_italia <- dbGetQuery(con, sql)
ggplot(obispos_por_ano_italia, aes(x=serie, y=count)) + geom_line()

# para España
sql <- "WITH anyos AS
      (SELECT generate_series(1200, 1800, 1) as serie ),
      espana as
      (SELECT diocese_id FROM dioceses JOIN places USING (place_id)
      	      		 WHERE country = 'Spain')
select serie, (SELECT count(*) from bishops JOIN espana USING (diocese_id)
       where serie between extract(year from date_nomination) and extract(year from date_end))
from anyos;"

obispos_por_ano_espana <- dbGetQuery(con, sql)
ggplot(obispos_por_ano_espana, aes(x=serie, y=count)) + geom_line()


## bizancio
# por décadas

sql <- "SELECT * from analysis.bishops_bizantium_per_decade"
obispos_bizancio_decada <- dbGetQuery(con, sql)
ggplot(obispos_bizancio_decada, aes(x= r_from, y= total)) + geom_bar(stat="identity")

## Periplos vitales
# Muerte y fin de obispado

sql <- "SELECT bishop_id, date_nomination, max(extract(year from date_end)) AS finobispado,
         CAST (datedeath AS int) AS muerte FROM bishops
         JOIN persons ON bishop_person_id = person_id
  WHERE datedeath IS NOT NULL AND datedeath !='' AND date_end IS NOT NULL 
  GROUP BY bishop_id, datedeath; "

muerte_finobispado <- dbGetQuery(con, sql)
# hay que transformar a integer
muerte_finobispado$finobispado <- as.integer(muerte_finobispado$finobispado)
muerte_finobispado$diferencia <- muerte_finobispado$muerte - muerte_finobispado$finobispado
Desc(muerte_finobispado$diferencia)

dbDisconnect(con)
