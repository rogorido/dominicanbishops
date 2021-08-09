periplo_edm_op

CREATE OR REPLACE FUNCTION order_absent_dioceses_edm (order_searching INT) 
  RETURNS TABLE (
    r_order_id int, r_order_acronym varchar, r_order_nickname varchar,
    r_diocese_id INT, r_diocese_name varchar, r_country VARCHAR, 
    r_longitude REAL, r_latitude real) 
LANGUAGE  plpgsql
AS $$

BEGIN 
  RETURN query 
  WITH diocconfrailes AS 
   (SELECT DISTINCT 
     diocese_id, order_id
     FROM vistas.b_edm_ss_sa b), 
-- cruzamos todos los datosde diócesis y órdenes
  cruzada AS 
    (SELECT r.order_id,
      CASE
        WHEN r.year_foundation = '4th Century' THEN '350'
        WHEN r.year_foundation = '6th Century' THEN '550'
        WHEN r.year_foundation = '11th Century' THEN '1050'
        WHEN r.year_foundation = '13th Century' THEN '1250'
        WHEN r.year_foundation = '14th Century' THEN '1350'
        ELSE r.year_foundation
      END AS fundacionorder,
      diocese_id, (other_data->'gcatholic'->>'foundation')::integer AS fundaciondioc 
     FROM general.dioceses d, general.religious_orders r),
-- miramos donde noestán 
   diocsinfrailes AS (
     SELECT order_id, fundacionorder, diocese_id, fundaciondioc 
     FROM cruzada c
     WHERE NOT EXISTS (SELECT 1, 2 FROM diocconfrailes df
                       WHERE df.diocese_id = c.diocese_id AND df.order_id = c.order_id )),
-- lo juntamostodo
    diocorrg AS (
     SELECT order_id, fundacionorder, diocese_id, fundaciondioc
     FROM diocsinfrailes) --WHERE fundacionorder::integer > fundaciondioc;
-- lo hacemosmás presentable 
  SELECT d.order_id, r.order_acronym, r.order_nickname,
         diocese_id, dd.diocese_name, p.country, 
         p.longitude, p.latitude 
  FROM diocorrg d
  JOIN general.religious_orders r USING(order_id)
  JOIN general.dioceses dd USING(diocese_id)
  LEFT JOIN general.places p USING(place_id)
  WHERE d.order_id = order_searching
  ORDER BY dd.diocese_name;  
END;
$$

DROP VIEW vistas.bishops_individuals_edm_op CASCADE;
CREATE OR replace VIEW vistas.bishops_individuals_edm_op AS 
WITH concretos AS
  (SELECT url,
          bishop_surname || bishop_name AS bishop_fullname,
          diocese_id, date_nomination, date_end,
          order_id, order_acronym, order_name_english, order_type,
          reason_begin, reason_end
  FROM vistas.b_edm_cs_sa
  WHERE order_id = 121)

SELECT url, bishop_fullname,
       order_id, order_acronym, order_name_english, order_type,
       diocese_ID, dioceses.diocese_name,
       longitude, latitude,
       reason_begin, reason_end,
       date_nomination, date_end,
       date_end - date_nomination AS duracion,
       round(((date_end - date_nomination)::NUMERIC/365)::DECIMAL, 2) AS anos,
       ROW_NUMBER() OVER (PARTITION BY url ORDER BY date_nomination ) AS ordinal
FROM concretos
JOIN general.dioceses USING(diocese_id)
LEFT JOIN general.places USING (place_id)
ORDER BY url, date_nomination;

SELECT DISTINCT url 
FROM vistas.bishops_individuals_edm_op;

SELECT url, bishop_fullname, COUNT(*) AS total 
FROM vistas.bishops_individuals_edm_op
GROUP BY url, bishop_fullname;

SELECT b.*, p.country
FROM vistas.bishops_individuals_edm_op b
JOIN 
 (SELECT url, COUNT(*) AS total 
  FROM vistas.bishops_individuals_edm_op
  GROUP BY url
  HAVING COUNT(*)= 2) j USING (url)
JOIN dioceses d USING (diocese_id)
LEFT JOIN places P USING (place_id);

CREATE OR REPLACE FUNCTION bishops_order_per_year_edm_with_country (order_searching INT,
       year_searching INT, country_searching VARCHAR) 
  RETURNS int
LANGUAGE  plpgsql STABLE
AS $$
 DECLARE total INT;
BEGIN
 SELECT INTO total
  (SELECT COUNT(*) FROM vistas.b_edm_ss_sa
   JOIN general.dioceses USING (diocese_id)
   LEFT JOIN general.places p USING (place_id)
   WHERE year_searching BETWEEN EXTRACT(YEAR FROM date_nomination)
                        AND EXTRACT(YEAR FROM date_end)
      AND order_id = order_searching
      AND p.country = country_searching);

   RETURN total;
END;
$$

CREATE OR REPLACE FUNCTION bishops_order_per_year_edm_without_country (order_searching INT,
       year_searching INT) 
  RETURNS int
LANGUAGE plpgsql IMMUTABLE 
AS $$
 DECLARE total INT;
BEGIN
 SELECT INTO total
  (SELECT COUNT(*) FROM vistas.b_edm_ss_sa
   WHERE year_searching BETWEEN EXTRACT(YEAR FROM date_nomination)
                        AND EXTRACT(YEAR FROM date_end)
      AND order_id = order_searching);

   RETURN total;
END;
$$


WITH anyos AS (
      SELECT generate_series(1500, 1799, 1) AS serie)
SELECT serie,
       bishops_order_per_year_edm_without_country(121, serie) AS totalobispos
FROM anyos;

WITH anyos AS (
      SELECT generate_series(1500, 1799, 1) AS serie)
j AS
(SELECT *
FROM anyos a
LEFT JOIN LATERAL
(SELECT COUNT(*) FROM vistas.b_edm_ss_sa ) ON true
)
SELECT serie,
       bishops_order_per_year_edm_without_country(121, serie) AS totalobispos
FROM anyos;


  (SELECT COUNT(*) FROM vistas.b_edm_ss_sa
   JOIN general.dioceses USING (diocese_id)
   LEFT JOIN general.places p USING (place_id)
   WHERE year_searching BETWEEN EXTRACT(YEAR FROM date_nomination)
                        AND EXTRACT(YEAR FROM date_end)
      AND order_id = order_searching
      AND p.country = country_searching);


WITH anyos AS (
      SELECT generate_series(1500, 1799, 1) AS serie)
SELECT serie,
       bishops_order_per_year_edm_without_country(121, serie) AS totalobispos
FROM anyos;

--- esto parece funcionar... pero creo q paso
--- me he basado en esto
--- https://stackoverflow.com/questions/38332433/how-to-include-missing-data-for-multiple-groupings-within-the-time-span
SELECT *
FROM generate_series(1500, 1799, 1) AS anyo 
CROSS JOIN
      (SELECT DISTINCT b.diocese_id FROM vistas.bishops_individuals_edm_op b
       JOIN dioceses USING (diocese_id)
       JOIN places P USING (place_id)
       WHERE country='Spain') a
LEFT JOIN LATERAL (SELECT COUNT(*) AS total
FROM vistas.bishops_individuals_edm_op b
WHERE anyo  BETWEEN EXTRACT(YEAR FROM b.date_nomination)
                        AND EXTRACT(YEAR FROM b.date_end)
      AND b.diocese_id = a.diocese_id) K
      ON TRUE;




EXPLAIN analyze
 WITH concretos AS (
         SELECT b_edm_cs_sa.url,
            b_edm_cs_sa.bishop_surname::text || b_edm_cs_sa.bishop_name::text AS bishop_fullname,
            b_edm_cs_sa.diocese_id,
            b_edm_cs_sa.date_nomination,
            b_edm_cs_sa.date_end,
            b_edm_cs_sa.order_id,
            b_edm_cs_sa.order_acronym,
            b_edm_cs_sa.order_name_english,
            b_edm_cs_sa.order_type
           FROM b_edm_cs_sa
          WHERE b_edm_cs_sa.order_id = 121
        )
 SELECT concretos.url,
    concretos.bishop_fullname,
    concretos.order_id,
    concretos.order_acronym,
    concretos.order_name_english,
    concretos.order_type,
    concretos.diocese_id,
    dioceses.diocese_name,
    places.longitude,
    places.latitude,
    concretos.date_nomination,
    concretos.date_end,
    concretos.date_end - concretos.date_nomination AS duracion,
    round((concretos.date_end - concretos.date_nomination)::numeric / 365::numeric, 2) AS anos,
    row_number() OVER (PARTITION BY concretos.url ORDER BY concretos.date_nomination) AS ordinal
   FROM concretos
     JOIN dioceses USING (diocese_id)
     LEFT JOIN places USING (place_id)
  ORDER BY concretos.url, concretos.date_nomination;



WITH anyos AS (
      SELECT generate_series(1500, 1799, 1) AS serie),
d AS
      (SELECT DISTINCT b.diocese_id FROM vistas.bishops_individuals_edm_op b
       JOIN dioceses USING (diocese_id)
       JOIN places P USING (place_id)
       WHERE country='Spain') 
SELECT serie, d.diocese_id,
       dd.diocese_name, p.longitude, p.latitude,
       bishops_order_per_year_edm_per_dioceses(serie, d.diocese_id, 121)
FROM anyos, d
JOIN dioceses dd USING (diocese_id)
JOIN places P USING (place_id);


CREATE OR REPLACE FUNCTION bishops_order_per_year_edm_per_dioceses (
       year_searching INT, diocese_searching INT, order_searching INT) 
  RETURNS int
LANGUAGE  plpgsql STABLE
AS $$
 DECLARE total INT;
BEGIN
 SELECT INTO total
  (SELECT COUNT(*) FROM vistas.b_edm_ss_sa b
   WHERE year_searching BETWEEN EXTRACT(YEAR FROM date_nomination)
                        AND EXTRACT(YEAR FROM date_end)
      AND order_id = order_searching
      AND b.diocese_id = diocese_searching);

   RETURN total;
END;
$$

-- mostrar serie temporal por año y diócesis escogiendo país
WITH anyos AS (
      SELECT generate_series(1500, 1799, 1) AS serie),
d AS
      (SELECT DISTINCT b.diocese_id FROM vistas.bishops_individuals_edm_op b
       JOIN dioceses USING (diocese_id)
       JOIN places P USING (place_id)
       WHERE country='Spain') 
SELECT serie, d.diocese_id,
       dd.diocese_name, p.longitude, p.latitude,
       bishops_order_per_year_edm_per_dioceses(serie, d.diocese_id, 121)
FROM anyos, d
JOIN dioceses dd USING (diocese_id)
JOIN places P USING (place_id);


-- una prueba
CREATE materialized VIEW vistas.ops_per_diocese_peryear as
WITH anyos AS (
      SELECT generate_series(1500, 1799, 1) AS serie),
d AS
      (SELECT DISTINCT b.diocese_id FROM vistas.bishops_individuals_edm_op b) 
SELECT serie, d.diocese_id,
       dd.diocese_name, p.longitude, p.latitude,
       bishops_order_per_year_edm_per_dioceses(serie, d.diocese_id, 121)
FROM anyos, d
JOIN dioceses dd USING (diocese_id)
JOIN places P USING (place_id);


SELECT relname   AS objectname
     , relkind   AS objecttype
     , reltuples AS entries
     , pg_size_pretty(pg_table_size(oid)) AS size  -- depending - see below
FROM   pg_class
WHERE  relkind IN ('m')
ORDER  BY pg_table_size(oid) DESC;

SELECT DISTINCT r.order_id, r.order_acronym, r.order_name_english
FROM religious_orders r
JOIN vistas.b_edm_ss_sa b USING (order_id)
ORDER  BY r.order_acronym;


  CREATE OR REPLACE VIEW vistas.b_edm_ss_sa
    AS
  SELECT b.*, r.order_id, r.order_acronym, r.order_name_english, r.order_type
  FROM b_bishops_reducido b
  LEFT JOIN religious_orders r ON b.religious_order_id = r.order_id
  WHERE religious_order_id IS NOT NULL and affiliated = FALSE and date_nomination > '1500-01-01'
        AND date_nomination < '1800-01-01';


SELECT DISTINCT bishop_id, d.diocese_name AS diocesis,
              p.country AS pais, 
       COALESCE(
                SUM(EXTRACT(year from date_end)
                                 -
                    EXTRACT(year from date_nomination)), 0) AS duracion
FROM bishops b
     JOIN dioceses d ON d.diocese_id = b.diocese_id
     join places p on d.place_id = p.place_id
where date_nomination is not null and date_end is not null
group by bishop_id, diocesis, pais;

SELECT d.diocese_name AS diocesis,
              p.country AS pais, 
       COALESCE(
                SUM(EXTRACT(year from date_end)
                                 -
                    EXTRACT(year from date_nomination)), 0) AS duracion
FROM vistas.b_edm_ss_sa b
     JOIN dioceses d ON d.diocese_id = b.diocese_id
     join places p on d.place_id = p.place_id
WHERE (date_nomination is not null and date_end is not NULL) AND order_id = 121
group by diocesis, pais;


SELECT d.diocese_name AS diocesis,
       p.country AS pais,
       p.longitude, p.latitude,
       COALESCE(
                SUM(EXTRACT(year from date_end)
                                 -
                    EXTRACT(year from date_nomination)), 0) AS duracion
FROM vistas.b_edm_ss_sa b
     JOIN dioceses d ON d.diocese_id = b.diocese_id
     join places p on d.place_id = p.place_id
WHERE (date_nomination is not null and date_end is not NULL) AND order_id = 121
      AND country= 'Italy'
group by diocesis, pais, longitude, latitude
ORDER BY duracion DESC;

--- lo mismo pero con timestamps 
SELECT d.diocese_name AS diocesis,
       p.country AS pais,
       p.longitude, p.latitude,       
       COALESCE(
                (SUM(date_end - date_nomination) / 365.0), 0) AS duracion
FROM vistas.b_edm_ss_sa b
     JOIN dioceses d ON d.diocese_id = b.diocese_id
     join places p on d.place_id = p.place_id
WHERE (date_nomination is not null and date_end is not NULL) AND order_id = 121
group by 1, 2, 3, 4
ORDER BY duracion DESC;

select * from bishops.dioceses_global_fl_estimado
WHERE country = 'Italy';



-- calcula el % de OPs pero no del total de obispos en cada dićoesis
-- sino del total de frailes 

CREATE TEMP VIEW leches1 AS 
WITH conjunta AS (
       SELECT diocese_id, COUNT(*) AS totalorders
       FROM vistas.b_edm_cs_sa
       WHERE religious_order IS NOT NULL
       GROUP BY diocese_id),
ops AS (
       SELECT diocese_id,  COUNT(*) AS totalops
       FROM vistas.b_edm_cs_sa
       WHERE religious_order_id = 121
       GROUP BY diocese_id)

SELECT diocese_id, d.diocese_name, p.country,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
       totalorders, totalops,
       (totalops::real/totalorders::real)::REAL   as porcentaje 
FROM general.dioceses d
JOIN conjunta c USING (diocese_id)	
JOIN ops o USING (diocese_id)	
JOIN general.places p ON p.place_id = d.place_id
ORDER BY d.diocese_name;

SELECT COUNT(DISTINCT diocese_id) FROM leches1;

SELECT d.diocese_name AS diocesis,
       p.country AS pais,
       p.longitude, p.latitude,       
       COALESCE(
                (SUM(date_end - date_nomination) / 365.0), 0) AS duracion
FROM vistas.b_edm_ss_sa b
     JOIN dioceses d ON d.diocese_id = b.diocese_id
     join places p on d.place_id = p.place_id
WHERE (date_nomination is not null and date_end is not NULL) AND order_id = 121
      AND d.diocese_id = 181
group by 1, 2, 3, 4
ORDER BY duracion DESC;

SELECT d.diocese_name AS diocesis,
       p.country AS pais,
       p.longitude, p.latitude,
       r.tasa, r.tasa_currency,
       r.mepisc, r.mensa_currency
FROM vistas.b_edm_ss_sa b
     JOIN dioceses d USING (diocese_id) 
     join places p on d.place_id = p.place_id
     JOIN dioceses_rents r USING (diocese_id)
WHERE (date_nomination is not null and date_end is not NULL) AND order_id = 121
group by 1, 2, 3, 4, 5, 6, 7, 8
ORDER BY diocesis DESC;

SELECT dg.*
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b WHERE order_id = 121) j
     USING (diocese_id);

WITH media AS 
(SELECT AVG(dg.tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
WHERE pais = 'Italy')
SELECT * FROM media;

SELECT dg.diocese_id, dg.diocese_name,
       CASE WHEN (SELECT diocese_id IN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b WHERE order_id = 121)) THEN 'true'
       ELSE 'false'
       END AS ops
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id);

EXPLAIN analyze
SELECT dg.diocese_id, dg.diocese_name,
       CASE WHEN (SELECT diocese_id IN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b WHERE order_id = 121)) THEN 'true'
       ELSE 'false'
       END AS ops
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id);


SELECT dg.diocese_name, dg.tasa_media, ntile(10) OVER (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b WHERE order_id = 121) j
     USING (diocese_id);

SELECT dg.diocese_name, dg.tasa_media, percentile_cont(0.75) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b WHERE order_id = 121) j
     USING (diocese_id)
GROUP BY 1,2;

SELECT percentile_cont(0.75) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_cs_sa b) j
     USING (diocese_id)
     WHERE pais = 'Italy';


SELECT dg.*,
       CASE WHEN
            (SELECT diocese_id
                    IN (SELECT DISTINCT diocese_id
                        FROM vistas.b_edm_ss_sa b
                        WHERE order_id = 121)) THEN 'true'
       ELSE 'false'
       END AS ops
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
WHERE dg.tasa_media >= (
SELECT percentile_cont(0.75) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais = 'Italy')
AND pais = 'Italy';


---  versión mejorada 
SELECT dg.*,
       CASE WHEN
            (SELECT diocese_id
                    IN (SELECT DISTINCT diocese_id
                        FROM vistas.b_edm_ss_sa b
                        WHERE order_id = 121)) THEN 'true'
       ELSE 'false'
       END AS ops
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
WHERE dg.tasa_media between (
SELECT percentile_cont(array[0.75,1]) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais = 'Italy')[1] AND
     (
SELECT percentile_cont(array[0.75,1]) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais = 'Italy')[2]
AND pais = 'Italy';

SELECT dg.*,
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
SELECT percentile_cont(0.75) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais = 'Italy') AND
     (
SELECT percentile_cont(1) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais = 'Italy')
AND pais = 'Italy';


---  versión mejorada: otro intento
WITH valores AS (
SELECT dg.*,
       CASE WHEN
            (SELECT diocese_id
                    IN (SELECT DISTINCT diocese_id
                        FROM vistas.b_edm_ss_sa b
                        WHERE order_id = 121)) THEN 'true'
       ELSE 'false'
       END AS ops,
       (SELECT percentile_cont(array[0.75,1]) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais = 'Italy') AS percentilcalculado
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id))
SELECT * FROM valores 
WHERE tasa_media BETWEEN percentilcalculado[1] AND percentilcalculado[2]
AND pais = 'Italy';



CREATE TEMP VIEW opsmonopol AS 
WITH conjunta AS (
       SELECT diocese_id, COUNT(*) AS totalorders
       FROM vistas.b_edm_cs_sa
       WHERE religious_order IS NOT NULL
       GROUP BY diocese_id),
ops AS (
       SELECT diocese_id,  COUNT(*) AS totalops
       FROM vistas.b_edm_cs_sa
       WHERE religious_order_id = 121
       GROUP BY diocese_id)
SELECT diocese_id, d.diocese_name, p.country,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
       totalorders, totalops,
       (totalops::real/totalorders::real)::REAL   as porcentaje 
FROM general.dioceses d
JOIN conjunta c USING (diocese_id)	
JOIN ops o USING (diocese_id)	
JOIN general.places p ON p.place_id = d.place_id
ORDER BY d.diocese_name;

SELECT * FROM opsmonopol 
JOIN (
SELECT b.diocese_id, COUNT(*) AS total
FROM vistas.bishops_individuals_edm_op b
GROUP BY b.diocese_id
HAVING COUNT(*) >= 2) t USING (diocese_id)
WHERE porcentaje > 0.5;

SELECT * FROM opsmonopol 
JOIN (
SELECT b.diocese_id, COUNT(*) AS total
FROM vistas.bishops_individuals_edm_op b
GROUP BY b.diocese_id
HAVING COUNT(*) = 1) t USING (diocese_id)
WHERE porcentaje >= 0.75;

WITH j AS 
   (SELECT * FROM opsmonopol 
   JOIN (
   SELECT b.diocese_id, COUNT(*) AS total
   FROM vistas.bishops_individuals_edm_op b
   GROUP BY b.diocese_id
   HAVING COUNT(*) >= 2) t USING (diocese_id)
   WHERE porcentaje > 0.5)
SELECT country, COUNT(*) AS total
FROM j GROUP BY country
ORDER BY total DESC;


-- los q están menos de dos años
SELECT COUNT(*)
FROM bishops_individuals_edm_op;
--WHERE anos < 2;

CREATE OR REPLACE VIEW bishops_individuals_edm_op_more_two_years AS
SELECT * FROM bishops_individuals_edm_op WHERE anos > 2;

WITH j AS 
(SELECT b.diocese_id, COUNT(*) AS totalabsoluto 
FROM bishops_individuals_edm_op b
GROUP BY b.diocese_id),
K AS
(SELECT b.diocese_id, COUNT(*) AS totalpoco
FROM bishops_individuals_edm_op b
WHERE anos < 2
GROUP BY b.diocese_id)
SELECT d.diocese_id, d.diocese_name,
       totalabsoluto, totalpoco,
       totalpoco::REAL / totalabsoluto::real AS porcentaje
FROM dioceses d
JOIN j USING (diocese_id)
JOIN K USING (diocese_id)
ORDER BY porcentaje DESC;

-- lo mismo pero menos de un año 
WITH j AS 
(SELECT b.diocese_id, COUNT(*) AS totalabsoluto 
FROM bishops_individuals_edm_op b
GROUP BY b.diocese_id),
K AS
(SELECT b.diocese_id, COUNT(*) AS totalpoco
FROM bishops_individuals_edm_op b
WHERE anos < 1
GROUP BY b.diocese_id)
SELECT d.diocese_id, d.diocese_name,
       totalabsoluto, totalpoco,
       totalpoco::REAL / totalabsoluto::real AS porcentaje
FROM dioceses d
JOIN j USING (diocese_id)
JOIN K USING (diocese_id)
ORDER BY porcentaje DESC;

--- lomismo pero 3 años
WITH j AS 
(SELECT b.diocese_id, COUNT(*) AS totalabsoluto 
FROM bishops_individuals_edm_op b
GROUP BY b.diocese_id),
K AS
(SELECT b.diocese_id, COUNT(*) AS totalpoco
FROM bishops_individuals_edm_op b
WHERE anos < 3
GROUP BY b.diocese_id)
SELECT d.diocese_id, d.diocese_name,
       totalabsoluto, totalpoco,
       totalpoco::REAL / totalabsoluto::real AS porcentaje
FROM dioceses d
JOIN j USING (diocese_id)
JOIN K USING (diocese_id)
ORDER BY porcentaje DESC;


WITH j AS 
(SELECT b.diocese_id, COUNT(*) AS totalabsoluto 
FROM bishops_individuals_edm_op b
GROUP BY b.diocese_id),
K AS
(SELECT b.diocese_id, COUNT(*) AS totalpoco
FROM bishops_individuals_edm_op b
WHERE anos < 2
GROUP BY b.diocese_id)
SELECT d.diocese_id, d.diocese_name,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
       totalabsoluto, totalpoco,
       totalpoco::REAL / totalabsoluto::real AS porcentaje
FROM dioceses d
JOIN j USING (diocese_id)
JOIN K USING (diocese_id)
JOIN places P USING (place_id)
ORDER BY porcentaje DESC;

WITH j AS 
(SELECT b.diocese_id, COUNT(*) AS totalabsoluto 
FROM bishops_individuals_edm_op b
GROUP BY b.diocese_id),
K AS
(SELECT b.diocese_id, COUNT(*) AS totalpoco
FROM bishops_individuals_edm_op b
WHERE anos > 2 AND anos < 3
GROUP BY b.diocese_id)
SELECT d.diocese_id, d.diocese_name,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
       totalabsoluto, totalpoco,
       totalpoco::REAL / totalabsoluto::real AS porcentaje
FROM dioceses d
JOIN j USING (diocese_id)
JOIN K USING (diocese_id)
JOIN places P USING (place_id)
ORDER BY porcentaje DESC;

WITH j AS
(SELECT b.diocese_id, COUNT(*) AS totalabsoluto
FROM bishops_individuals_edm_op b
GROUP BY b.diocese_id),
K AS
(SELECT b.diocese_id, COUNT(*) AS totalpoco
FROM bishops_individuals_edm_op b
WHERE anos < 1
GROUP BY b.diocese_id)
SELECT d.diocese_id, d.diocese_name,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
       totalabsoluto, totalpoco,
       totalpoco::REAL / totalabsoluto::real AS porcentaje
FROM dioceses d
JOIN j USING (diocese_id)
JOIN K USING (diocese_id)
JOIN places P USING (place_id)
ORDER BY porcentaje DESC;


SELECT url, bishop_fullname, ordinal, reason_end
FROM bishops_individuals_edm_op b
WHERE anos < 2;

SELECT TRIM(reason_end), COUNT(*) AS total
FROM bishops_individuals_edm_op b
WHERE anos < 2
GROUP BY 1
ORDER BY total DESC;

WITH reasons AS
   (SELECT ordinal, reason_end, COUNT(*) AS totalreason
   FROM bishops_individuals_edm_op b
   WHERE anos < 2
   GROUP BY 1, 2),
conjunto AS
   (SELECT ordinal,  COUNT(*) AS total
   FROM bishops_individuals_edm_op b
   WHERE anos < 2
   GROUP BY 1)
SELECT r.*, c.total,
       totalreason::REAL / total::REAL AS porcentaje 
FROM reasons r
JOIN conjunto C USING(ordinal)
ORDER BY ordinal ASC, totalreason DESC;


WITH reasons AS
   (SELECT ordinal, reason_end, COUNT(*) AS totalreason
   FROM bishops_individuals_edm_op b
   WHERE anos < 1
   GROUP BY 1, 2),
conjunto AS
   (SELECT ordinal,  COUNT(*) AS total
   FROM bishops_individuals_edm_op b
   WHERE anos < 1
   GROUP BY 1)
SELECT r.*, c.total,
       totalreason::REAL / total::REAL AS porcentaje 
FROM reasons r
JOIN conjunto C USING(ordinal)
ORDER BY ordinal ASC, totalreason DESC;

WITH reasons AS
   (SELECT ordinal, reason_end, COUNT(*) AS totalreason
   FROM bishops_individuals_edm_op b
   WHERE anos < 4
   GROUP BY 1, 2),
conjunto AS
   (SELECT ordinal,  COUNT(*) AS total
   FROM bishops_individuals_edm_op b
   WHERE anos < 4
   GROUP BY 1)
SELECT r.*, c.total,
       totalreason::REAL / total::REAL AS porcentaje 
FROM reasons r
JOIN conjunto C USING(ordinal)
ORDER BY ordinal ASC, totalreason DESC;

SELECT DISTINCT b.diocese_id, b.diocese_name,
       p.place_id, p.place,
       press, pop1500, bishop, laymag,
       marketpot1500, water,
       univ1450, indepcity
FROM bishops_individuals_edm_op b
JOIN dioceses d USING (diocese_id)
JOIN analysis.cities_with_socioeconomicdata P USING (place_id);


CREATE OR REPLACE TEMP VIEW tv_periplo_emd_ss_sa AS
WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez
     (SELECT url, COUNT(*) AS total
      FROM vistas.b_emd_ss_sa
      GROUP BY url
      HAVING COUNT(url) > 1),

-- luego con lo anterior seleccionamos las diócesis y las fechas
     diocconcretas AS
     (SELECT url, diocese_id, date_nomination, date_end, order_id, order_acronym, order_name_english, order_type
     FROM vistas.b_emd_ss_sa
     JOIN obispostotales USING(url))

-- finalmente componemos todo y contamos en días y en años las duraciones
SELECT url, order_id, order_acronym, order_name_english, order_type, diocese_ID, dioceses.diocese_NAME,
       longitude, latitude,
       date_nomination, date_end,
       date_end - date_nomination AS duracion,
       round(((date_end - date_nomination)::NUMERIC/365)::DECIMAL, 2) AS anos,
       ROW_NUMBER() OVER (PARTITION BY url ORDER BY date_nomination ) AS ordinal
FROM diocconcretas
JOIN dioceses USING(diocese_id)
LEFT JOIN places USING (place_id)
ORDER BY url, date_nomination;


CREATE OR replace temp VIEW tv_agregadodiocesis_edm_OPs AS
  WITH obispostotales AS
  -- escogemos solo los obispos que aparecen más de una vez
       (SELECT url, COUNT(*) AS total
        FROM vistas.b_edm_ss_sa
        WHERE religious_order = 'O.P.'
        GROUP BY url
        HAVING COUNT(url) > 1),

  -- luego con lo anterior seleccionamos las diócesis y las fechas
       diocconcretas AS
       (SELECT url, diocese_id, date_nomination
       FROM vistas.b_edm_ss_sa
       JOIN obispostotales USING(url))

  -- finalmente hacemos un agregado de las diócesis por las q pasan
  SELECT url,
         ARRAY_AGG(diocese_name ORDER BY date_nomination) AS sucesion
  FROM diocconcretas
  JOIN dioceses USING(diocese_id)
  GROUP BY url
  ORDER BY url;

  CREATE OR REPLACE TEMP VIEW tv_paresdiocesis_edm_OPs AS

  --- juntamos todas los pares
  WITH juntas AS (
       SELECT url, sucesion[1] AS dioc1, sucesion[2] AS dioc2
       FROM tv_agregadodiocesis_edm_OPs
       UNION ALL
       SELECT url, sucesion[2] AS dioc1, sucesion[3] AS dioc2
       FROM tv_agregadodiocesis_edm_OPs
       UNION ALL
       SELECT url, sucesion[3] AS dioc1, sucesion[4] AS dioc2
       FROM tv_agregadodiocesis_edm_OPs
       UNION ALL
       SELECT url, sucesion[4] AS dioc1, sucesion[5] AS dioc2
       FROM tv_agregadodiocesis_edm_OPs),

  --- quitamos los pares donde haya algún null
  sinnulls AS (
      SELECT * FROM juntas
      WHERE dioc1 IS NOT NULL AND dioc2 IS NOT NULL)

  --- hacemos el recuento
  SELECT dioc1, dioc2, COUNT(*) as total
  FROM sinnulls
  GROUP BY dioc1, dioc2
  ORDER BY COUNT(*) DESC;


--hay algún caso de diócesis q sean las primeras así fctt?
DROP MATERIALIZED VIEW IF EXISTS vistas.related_dioceses_edm_ops CASCADE;

CREATE materialized VIEW vistas.related_dioceses_edm_ops
AS
WITH d_basic AS   --- CTE 1
     (SELECT a.diocese_id, d.diocese_name, p.latitude, p.longitude,
             f_related_dioceses(a.diocese_id, '"b_edm_ss_sa"', true) AS vinculadas
     FROM (SELECT DISTINCT diocese_id
           FROM vistas.b_edm_ss_sa
           WHERE religious_order = 'O.P.') A
     JOIN dioceses d USING(diocese_id)
     JOIN places P ON d.place_id = p.place_id
     GROUP BY a.diocese_id, d.diocese_name, p.latitude, p.longitude
     ),

d_related_unnested AS  --- CTE2
(SELECT diocese_id, diocese_name, latitude, longitude,
        UNNEST(vinculadas) AS dioc_vinc
 FROM d_basic)

SELECT dr.diocese_id AS dioc_id_A, dr.diocese_name AS diocesis_A,
       dr.latitude AS lat_A, dr.longitude AS long_A,
       dr.dioc_vinc AS dioc_id_B, dd.diocese_name AS diocesis_B,
       p.latitude AS lat_B, p.longitude AS long_B
FROM d_related_unnested dr
JOIN dioceses dd ON dr.dioc_vinc = dd.diocese_id
JOIN places P ON dd.place_id = p.place_id;


SELECT diocesis_a || ' - ' || diocesis_b,
       st_makeline(ST_SetSRID(ST_MakePoint(long_a, lat_a),4326),
                  ST_SetSRID(ST_MakePoint(long_b, lat_b),4326))
FROM vistas.related_dioceses_edm_ops;

-- leinas
SELECT diocesis_a || ' - ' || diocesis_b,
       st_makeline(ST_SetSRID(ST_MakePoint(long_a, lat_a),4326),
                  ST_SetSRID(ST_MakePoint(long_b, lat_b),4326))
FROM vistas.related_dioceses_edm_ops;

CREATE OR REPLACE VIEW leches1 as
SELECT diocesis_a || ' - ' || diocesis_b AS conexion,
       st_makeline(ST_SetSRID(ST_MakePoint(long_a, lat_a),4326),
                  ST_SetSRID(ST_MakePoint(long_b, lat_b),4326)) AS linea
FROM vistas.related_dioceses_edm_ops;

--- agregando el asunto
WITH j AS
(SELECT diocesis_a AS diocese_name,
       ST_SetSRID(ST_MakePoint(long_a, lat_a),4326) AS coord
FROM vistas.related_dioceses_edm_ops
)
SELECT diocese_name, coord,
       COUNT(*) AS total
FROM j
GROUP BY 1,2
ORDER BY total DESC;

WITH j AS
(SELECT diocesis_a AS diocese_name,
       ST_SetSRID(ST_MakePoint(long_a, lat_a),4326) AS coord
FROM vistas.related_dioceses_edm_ops
)
SELECT diocese_name,
       COUNT(*) AS total
FROM j
GROUP BY 1
ORDER BY total DESC;

WITH j AS
(SELECT diocesis_a AS diocese_name,
       ST_SetSRID(ST_MakePoint(long_a, lat_a),4326) AS coord
FROM vistas.related_dioceses_edm_ops
)
SELECT diocese_name,
       st_length(coord) AS largura
FROM j
ORDER BY largura DESC;


--- midiendo las líenas
SELECT diocesis_a || ' - ' || diocesis_b,
       st_length(st_makeline(ST_SetSRID(ST_MakePoint(long_a, lat_a),4326),
                  ST_SetSRID(ST_MakePoint(long_b, lat_b),4326))) AS largura
FROM vistas.related_dioceses_edm_ops
ORDER BY largura DESC;


--- mirando si hay alguna regularidad en 1as y 2as, 3as diócesis
SELECT DISTINCT ordinal
FROM vistas.bishops_individuals_edm_op;

WITH j AS (
SELECT diocese_id, diocese_name,
       CASE
          WHEN ordinal > 1 THEN 'segundas'
          ELSE 'primeras'
        END AS periplos
FROM vistas.bishops_individuals_edm_op)
SELECT diocese_id, diocese_name, periplos, COUNT(*)
FROM j
GROUP BY 1, 2, 3
ORDER BY diocese_name;

-- lo mismo más fácil
SELECT diocese_id, diocese_name,
       COUNT(1) FILTER (WHERE ordinal = 1) AS primeras,
       COUNT(1) FILTER (WHERE ordinal > 1) AS segundas
FROM vistas.bishops_individuals_edm_op
GROUP BY 1,2;


-- le añadimos los datos geo
WITH j AS
(SELECT diocese_id, diocese_name, ordinal,
       ST_SetSRID(ST_MakePoint(longitude, latitude),4326) AS coord
FROM vistas.bishops_individuals_edm_op
)
SELECT diocese_id, diocese_name, coord,
       COUNT(1) FILTER (WHERE ordinal = 1) AS primeras,
       COUNT(1) FILTER (WHERE ordinal > 1) AS segundas
FROM j
GROUP BY 1, 2, 3;

DROP VIEW adriatic;
CREATE OR REPLACE TEMP VIEW adriatic AS
SELECT b.*,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord, p.country
FROM vistas.bishops_individuals_edm_op b
JOIN dioceses d USING (diocese_id)
JOIN places P USING (place_id)
WHERE country = ANY(array['Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey']);


SELECT diocese_id, diocese_name, coord,
       COUNT(*) AS total
FROM adriatic
GROUP BY 1, 2, 3
ORDER BY total DESC;

WITH j AS
(SELECT b.*,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord, p.country
FROM vistas.bishops_individuals_edm_op b
JOIN dioceses d USING (diocese_id)
JOIN places P USING (place_id)
WHERE country = ANY(array['Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey']))
SELECT diocese_id, diocese_name, coord,
       COUNT(*) AS total
FROM j
GROUP BY 1, 2, 3
ORDER BY total DESC;

--- sacando frecuencias
WITH j AS
(SELECT b.*,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord, p.country
FROM vistas.bishops_individuals_edm_op b
JOIN dioceses d USING (diocese_id)
JOIN places P USING (place_id)
WHERE country = ANY(array['Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey'])),
totales AS
(SELECT diocese_id, diocese_name, coord,
       COUNT(*) AS total
FROM j GROUP BY 1, 2, 3)
SELECT total, COUNT(*) AS totalgrupeado
FROM totales
GROUP BY total;

SELECT DISTINCT b.diocese_id, b.diocese_name
FROM vistas.bishops_individuals_edm_op b
JOIN dioceses d USING (diocese_id)
JOIN places P USING (place_id)
WHERE country = ANY(array['Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey']);
