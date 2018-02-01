
# esta función devuelve por defecto los no afiliados y los ops
sql <- "SELECT * from obispos_edm();"
em <- dbGetQuery(con, sql)

# en esta variable metemos la lista de órdenes que aparece
# en el combobox. Lo cogemos de vistas.b_all_withoutseculars_early_modern
# que es la query subyacente a obispos_edm() porque así están todas, ya
# la función obispos_edm() filtra por defecto a OPs.
sql <- "SELECT * FROM vistas.b_all_withoutseculars_early_modern;"
ordenes_em <- dbGetQuery(con, sql)
ordenes_em$order_acronym <- factor(ordenes_em$order_acronym)
ordenes_lista <- as.list(levels(ordenes_em$order_acronym))

#### Diócesis: totales 

# pero realmente lo que nos interesa son las dióceiss y su número de obispos:
sql <- "SELECT d.diocese_id, d.diocese_name as diocesis,
       p.longitude, p.latitude,
       count(b.bishop_all_id)
FROM (select * from obispos_edm()) b
     JOIN dioceses d
     ON d.diocese_id = b.diocese_id
     JOIN places p
     ON p.place_id = d.place_id
GROUP BY d.diocese_id, diocesis, p.longitude, p.latitude
order by diocesis;"

# téngase en cuenta que esto coge los valores por defecto de la fucnión
# obispos_edm() que es OP!
em_dioc <- dbGetQuery(con, sql)

# pero definimos una función de R para actualizar
# el total de obispos por diócesis 
actualizar_em_orden <- function(orden) {
    # print(paste0("Current orden: ", orden))

    sql <- paste0("SELECT d.diocese_id, d.diocese_name as diocesis,
p.longitude, p.latitude, count(b.bishop_all_id)
FROM (select * from obispos_edm(false, '", orden, "')) b
     JOIN dioceses d
     ON d.diocese_id = b.diocese_id
     JOIN places p
     ON p.place_id = d.place_id
GROUP BY d.diocese_id, diocesis, p.longitude, p.latitude
order by diocesis;")

    # print(paste0("La sql es: ", sql))

    em_dioc <- dbGetQuery(con, sql)
    em_dioc

    }

#### Evolución de número de obispos

# el valor inicial es OP 
sql <- "WITH anyos AS
        (SELECT generate_series(1500, 1800, 1) as serie )
  SELECT serie, (SELECT count(*) FROM obispos_edm()
         WHERE serie BETWEEN extract(year from date_nomination) AND extract(year from date_end))
  FROM anyos;"

em_bishops_per_year <- dbGetQuery(con, sql)

actualizar_em_evolucion <- function(orden) {
    # print(paste0("Current orden: ", orden))

    sql <- paste0("WITH anyos AS
        (SELECT generate_series(1500, 1800, 1) as serie )
  SELECT serie, (SELECT count(*) FROM obispos_edm(false, '", orden, "')
         WHERE serie BETWEEN extract(year from date_nomination) AND extract(year from date_end))
  FROM anyos;")

    # print(paste0("La sql es: ", sql))
    bishops  <- dbGetQuery(con, sql)
    bishops 
    }

#### Duración en años por diócesis

sql <- "SELECT DISTINCT d.diocese_id, d.diocese_name AS diocesis,
         p.longitude, p.latitude,
         b.order_acronym,
         COALESCE(
                  SUM(EXTRACT(year from b.date_end)
                                   -
                      EXTRACT(year from b.date_nomination)), 0) AS duracion
  FROM  dioceses d
       JOIN obispos_edm() b
       ON d.diocese_id = b.diocese_id
       LEFT OUTER JOIN places p ON p.place_id = d.place_id
  GROUP BY d.diocese_id, diocesis, p.longitude, p.latitude, b.order_acronym;"

em_dioc_duracion <- dbGetQuery(con, sql)

actualizar_em_duracion <- function(orden) {

    sql <- paste0("SELECT DISTINCT d.diocese_id, d.diocese_name AS diocesis,
         p.longitude, p.latitude,
         b.order_acronym,
         COALESCE(
                  SUM(EXTRACT(year from b.date_end)
                                   -
                      EXTRACT(year from b.date_nomination)), 0) AS duracion
  FROM  dioceses d
       JOIN obispos_edm(false, '", orden, "') b
       ON d.diocese_id = b.diocese_id
       LEFT OUTER JOIN places p ON p.place_id = d.place_id
  GROUP BY d.diocese_id, diocesis, p.longitude, p.latitude, b.order_acronym;")

    # print(paste0("La sql es: ", sql))
    bishops  <- dbGetQuery(con, sql)
    bishops 
    }

#### Por países

## Totales 
sql <- "SELECT DISTINCT 
       p.country AS pais, 
       b.order_acronym,
       count(*) as total 
FROM  dioceses d
     JOIN obispos_edm() b
     ON d.diocese_id = b.diocese_id
     LEFT OUTER JOIN places p ON p.place_id = d.place_id
GROUP BY pais, b.order_acronym
order by pais;"

em_por_pais <- dbGetQuery(con, sql)

actualizar_em_porpais <- function(orden) {

    sql <- paste0(sql <- "SELECT DISTINCT 
       p.country AS pais, 
       b.order_acronym,
       count(*) as total 
FROM  dioceses d
     JOIN obispos_edm(false, '", orden, "') b
     ON d.diocese_id = b.diocese_id
     LEFT OUTER JOIN places p ON p.place_id = d.place_id
GROUP BY pais, b.order_acronym
order by pais;") 

    bishops  <- dbGetQuery(con, sql)
    bishops 
    }

## Años totales por país
sql <- "SELECT DISTINCT p.country AS pais, 
         b.order_acronym,
         COALESCE(
                  SUM(EXTRACT(year from b.date_end)
                                   -
                      EXTRACT(year from b.date_nomination)), 0) AS duracion
  FROM  dioceses d
       JOIN obispos_edm() b
       ON d.diocese_id = b.diocese_id
       LEFT OUTER JOIN places p ON p.place_id = d.place_id
  GROUP BY pais, b.order_acronym
  order by pais;"

em_por_pais_duracion <- dbGetQuery(con, sql)

actualizar_em_porpais_duracion <- function(orden) {

    sql <- paste0("SELECT DISTINCT p.country AS pais, 
         b.order_acronym,
         COALESCE(
                  SUM(EXTRACT(year from b.date_end)
                                   -
                      EXTRACT(year from b.date_nomination)), 0) AS duracion
  FROM  dioceses d
       JOIN obispos_edm(false, '", orden, "') b
       ON d.diocese_id = b.diocese_id
       LEFT OUTER JOIN places p ON p.place_id = d.place_id
  GROUP BY pais, b.order_acronym
  order by pais;")

    bishops  <- dbGetQuery(con, sql)
    bishops 
    }
