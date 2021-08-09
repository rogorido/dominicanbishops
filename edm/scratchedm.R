j <- intwodioceses %>%
    group_by(n) %>% count_pct()

Desc(factor(j$n))


op_series_edm %>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= totalobispos)) +
    geom_line(size = 1.3, color = "#a12828") +
    facet_wrap(~country) + 
    labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 


pp <- op_series_edm %>%
    filter(country == "Spain") %>%
    group_by(serie, country) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

pp <- op_series_edm %>%
    filter(country == "Spain") %>%
    group_by(serie, country) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

j <- "WITH anyos AS (
      SELECT generate_series(1500, 1799, 1) AS serie)
SELECT serie,
       bishops_order_per_year_edm_without_country($1, serie) AS totalobispos
FROM anyos"

rs = dbSendQuery(con, j, params = list(121))
k <- dbFetch(rs)


j <- "WITH anyos AS (
      SELECT generate_series(1500, 1799, 1) AS serie),
d AS
      (SELECT DISTINCT b.diocese_id FROM vistas.bishops_individuals_edm_op b
       JOIN dioceses USING (diocese_id)
       JOIN places P USING (place_id)
       WHERE country='Spain') 
SELECT serie, d.diocese_id,
       dd.diocese_name, p.longitude, p.latitude,
       bishops_order_per_year_edm_per_dioceses(serie, d.diocese_id, 121) as total
FROM anyos, d
JOIN dioceses dd USING (diocese_id)
JOIN places P USING (place_id)"

rs = dbSendQuery(con, j)
k <- dbFetch(rs)

pp <- k %>%     na_if(0) %>% 
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
    facet_wrap(~diocese_name) +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

pp <- k %>% filter(serie < 1600) %>%    na_if(0) %>% 
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
    facet_wrap(~diocese_name) +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))


j <- "WITH anyos AS (
      SELECT generate_series(1500, 1799, 1) AS serie),
d AS
      (SELECT DISTINCT b.diocese_id FROM vistas.bishops_individuals_edm_op b
       JOIN dioceses USING (diocese_id)
       JOIN places P USING (place_id)
       WHERE country in ($1) ) 
SELECT serie, d.diocese_id,
       dd.diocese_name, p.longitude, p.latitude,
       bishops_order_per_year_edm_per_dioceses(serie, d.diocese_id, 121)
FROM anyos, d
JOIN dioceses dd USING (diocese_id)
JOIN places P USING (place_id)"

k <- c("Portugal", "Brasil")
rs = dbSendQuery(con, j, params = list(k))
kk <- dbFetch(rs)
dbClearResult(rs)

j <- "select * from bishops.dioceses_global_fl_estimado WHERE pais = 'Italy'"
rs = dbSendQuery(con, j)
kk <- dbFetch(rs)
dbClearResult(rs)

kk <- kk %>% filter(pais == 'Italy')

kk_sf <- st_as_sf(kk,
                          coords = c("longitude", "latitude"),
                          agr = "constant")
st_write(kk_sf, "kk_sf.geojson", append = FALSE)


j <- "select *, latitude, longitude from general.dioceses join places using (place_id) where country = 'Italy'"
rs = dbSendQuery(con, j)
kk <- dbFetch(rs)
dbClearResult(rs)

kk <- kk %>% filter(pais == 'Italy')

kk_sf <- st_as_sf(kk,
                          coords = c("longitude", "latitude"),
                          agr = "constant")

st_write(kk_sf, "italy_dioceses_sf.geojson", append = FALSE)

j <-"WITH conjunta AS (
       SELECT diocese_id, 'conorder' AS orden, COUNT(*) AS total 
       FROM vistas.b_edm_cs_sa
       WHERE religious_order IS NOT NULL
       GROUP BY diocese_id
             UNION
       SELECT diocese_id, 'OPs' AS orden, COUNT(*) AS total 
       FROM vistas.b_edm_cs_sa
       WHERE religious_order_id = 121
       GROUP BY diocese_id)

SELECT diocese_id, d.diocese_name, p.country, p.longitude, p.latitude,
       c.orden, total,
       total/sum(total) OVER (PARTITION BY d.diocese_name) as porcentaje 
FROM general.dioceses d
JOIN conjunta c USING (diocese_id)	
JOIN general.places p ON p.place_id = d.place_id
WHERE p.country = 'Italy'
ORDER BY d.diocese_name"

rs = dbSendQuery(con, j)
kk <- dbFetch(rs)
dbClearResult(rs)

kk_piv <- pivot_wider(kk, names_from = c(diocese_name, orden), values_from = total)

kk_sf <- st_as_sf(kk,
                          coords = c("longitude", "latitude"),
                          agr = "constant")

st_write(kk_sf, "italy_percentage_ops_as_bishops_vs_other_orders_sf.geojson", append = FALSE)



j <- "SELECT percentile_cont(array[0.75, 1]) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais = 'Italy'"
rs = dbSendQuery(con, j)
kk <- dbFetch(rs)
dbClearResult(rs)


j <- "SELECT percentile_cont(array[$1, $2]) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais = $3"
rs = dbSendQuery(con, j, params = list(0.75, 1, "Italy"))
kk <- dbFetch(rs)
dbClearResult(rs)

j <- "SELECT percentile_cont($1) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais = $2"
rs = dbSendQuery(con, j, params = list(0.75, "Italy"))
kk <- dbFetch(rs)
dbClearResult(rs)



j <- "SELECT dg.*,
       CASE WHEN
            (SELECT diocese_id
                    IN (SELECT DISTINCT diocese_id
                        FROM vistas.b_edm_ss_sa b
                        WHERE order_id = 121)) THEN TRUE
       ELSE FALSE
       END AS ops
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
WHERE dg.tasa_media between (
SELECT percentile_cont($1) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais in ($3)) AND
     (
SELECT percentile_cont($2) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais in ($3))
AND pais in ($3);"

rs = dbSendQuery(con, j, params = list(0.75, 1, c_balcans_with_turkey))
kk <- dbFetch(rs)
dbClearResult(rs)


j <- "WITH j AS
(SELECT *
FROM vistas.bishops_individuals_edm_op
ORDER BY diocese_id, date_nomination),
K AS
(SELECT diocese_id, diocese_name,
        date_nomination,
        LAG(date_end, 1) OVER (PARTITION BY diocese_id),
        date_nomination - LAG(date_end, 1) OVER (PARTITION BY diocese_id) AS diferencia
FROM j)
SELECT *, diferencia / 365.25 AS anyos FROM K
WHERE diferencia IS NOT NULL
ORDER BY diocese_name;"

rs = dbSendQuery(con, j)
kk <- dbFetch(rs)
dbClearResult(rs)


## kk <- kk %>%
##     group_by(diocese_name) %>%
##     mutate(indice = harmonic.mean(diferencia),
##            media = mean(anyos),
##            stdev = sd(anyos),
##            iqr = IQR(anyos),
##            vcoef = sd(anyos) / mean(anyos))

kk <- kk %>%
    filter (anyos > 0) %>%
    group_by(diocese_name) %>%
    mutate(n = n(),
           total = sum(anyos),
           media = mean(anyos),
           stdev = sd(anyos),
           hmean = harmonic.mean(anyos),
           gmean = geometric.mean(anyos, na.rm = T))


harmonic.mean(1.56, 192, 68.4)
harmonic.mean(75.3, 37.7)

harmonic.mean(c(569, 70087, 24990))


mintotal = min(kk$total)
maxtotal = max(kk$total)

kk <-
    kk %>% mutate(cojones = (total - mintotal) / maxtotal - mintotal,
                  vcoef =   sd(anyos) / mean(anyos))

minvcoef = min(kk$vcoef, na.rm = TRUE)
maxvcoef = max(kk$vcoef, na.rm = TRUE)

kk <-
    kk %>% mutate(cojones2 = (vcoef - minvcoef ) / maxvcoef - minvcoef)


kk %>% mutate(cojones = total - min(total))



min(kk$total)

max(kk$total)

e <- c(77.2813141683778234, 10.7214236824093087)

geometric.mean(e)
mean(e)

ke <- c()
