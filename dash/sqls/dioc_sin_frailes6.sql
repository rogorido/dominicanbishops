-- Agregamos por países las orrg y su ausencia en diócesis 

CREATE OR replace TEMP VIEW tv_diocesissinfrailes_porcentajes_edm AS 
WITH paisestotales AS (
     SELECT country, COUNT(d.diocese_id) AS countrytotal
     FROM places
     JOIN dioceses d USING(place_id)
     GROUP BY country),
     
orrgtotales AS (
     SELECT d.order_id, r.order_acronym, p.country, COUNT(*) AS dioctotales
     FROM tv_diocesissinfrailes_edm d -- esto es temp view anterior! 
     JOIN religious_orders r USING(order_id)
     JOIN dioceses dd USING(diocese_id)
     JOIN places p USING(place_id)
     WHERE d.order_id IN (SELECT DISTINCT religious_order_id
                          FROM vistas.b_edm_ss_sa_top10orders)
     GROUP BY d.order_id, r.order_acronym, p.country)

SELECT order_id, order_acronym AS orden, o.country AS pais,
       countrytotal AS totalpais, dioctotales AS faltan,
       round(dioctotales::numeric / countrytotal::NUMERIC, 3) AS percentage
FROM orrgtotales o
JOIN paisestotales USING(country);
