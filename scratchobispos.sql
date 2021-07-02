CREATE OR REPLACE VIEW vistas.b_cph_cs_sa
  AS
SELECT b.*, r.order_id, r.order_acronym, r.order_name_english, r.order_type
FROM b_bishops_reducido b
LEFT JOIN religious_orders r ON b.religious_order_id = r.order_id
WHERE affiliated = FALSE and date_nomination > '1300-01-01' AND
      (date_end < '1599-12-31' OR date_end IS  NULL); 

SELECT EXTRACT(YEAR FROM date_nomination) AS anyo,
       COUNT(*)
FROM vistas.b_cph_cs_sa
GROUP BY anyo
ORDER BY anyo ASC;

SELECT * FROM vistas.b_cph_cs_sa WHERE date_nomination = '2019-07-12' AND date_end IS NULL;

CREATE OR REPLACE TEMP VIEW leches1
  AS
SELECT b.*, r.order_id, r.order_acronym, r.order_name_english, r.order_type
FROM b_bishops_reducido b
LEFT JOIN religious_orders r ON b.religious_order_id = r.order_id
WHERE affiliated = FALSE and date_nomination > '1300-01-01' AND
      date_end < '1599-12-31';



SELECT EXTRACT(YEAR FROM date_nomination) AS anyo,
       COUNT(*)
FROM leches1
GROUP BY anyo
ORDER BY anyo ASC;

DROP VIEW IF exists vistas.b_cph_cs_sa;

CREATE OR REPLACE VIEW vistas.b_cph_cs_sa
  AS
SELECT b.*, r.order_id, r.order_acronym, r.order_name_english, r.order_type
FROM b_bishops_reducido b
LEFT JOIN religious_orders r ON b.religious_order_id = r.order_id
WHERE affiliated = FALSE
      AND date_nomination > '1300-01-01'
      AND date_end < '1599-12-31';

WITH x AS (
  SELECT diocese_id,
     EXTRACT(century FROM DATE_nomination) as centuries,
     COUNT(*) AS total
  FROM vistas.b_cph_cs_sa
  GROUP BY diocese_id, centuries)
SELECT DISTINCT d.diocese_name, centuries, total 
FROM x
  JOIN general.dioceses d USING(diocese_id);


SELECT ORDER_id, religious_order, diocese_id,
       d.diocese_name, p.country,
       p.longitude, p.latitude,
       COUNT(*) AS total 
FROM vistas.b_cph_cs_sa
JOIN general.dioceses d USING(diocese_id)
LEFT JOIN general.places P USING(place_id)
WHERE order_ = 'O.P.'
GROUP BY ORDER_id, religious_order, diocese_id, d.diocese_name,
      p.country, p.longitude, p.latitude
ORDER BY d.diocese_name;


SELECT COUNT(DISTINCT url) as total
FROM vistas.b_cph_cs_sa
WHERE religious_order IS NOT NULL;

SELECT order_id, order_acronym || ' (' || order_name_english || ')' AS ordername FROM general.religious_orders ORDER BY ordername;

SELECT order_id, order_acronym || '(' || order_name_english || ')' as ordername FROM general.religious_orders ORDER BY ordername;


--- analizando frecuencias de presencia en diócesis 
SELECT ORDER_id, religious_order, diocese_id,
       d.diocese_name, p.country,
       p.longitude, p.latitude,
       COUNT(*) AS total 
FROM vistas.b_cph_cs_sa
JOIN general.dioceses d USING(diocese_id)
LEFT JOIN general.places P USING(place_id)
WHERE order_id = 121
GROUP BY ORDER_id, religious_order, diocese_id, d.diocese_name,
      p.country, p.longitude, p.latitude
HAVING COUNT(*) = 1
ORDER BY d.diocese_name;

WITH j AS
(SELECT ORDER_id, religious_order, diocese_id,
       d.diocese_name, p.country,
       p.longitude, p.latitude,
       COUNT(*) AS total 
FROM vistas.b_cph_cs_sa
JOIN general.dioceses d USING(diocese_id)
LEFT JOIN general.places P USING(place_id)
WHERE order_id = 121
GROUP BY ORDER_id, religious_order, diocese_id, d.diocese_name,
      p.country, p.longitude, p.latitude
HAVING COUNT(*) = 1
ORDER BY d.diocese_name)
SELECT country, COUNT(*)
FROM j
GROUP BY country;

SELECT country, COUNT(*) FROM dioceses
JOIN places USING (place_id)
WHERE country = 'Germany'
GROUP BY country;

  DROP VIEW IF exists vistas.b_cph_ss_sa;

  CREATE OR REPLACE VIEW vistas.b_cph_ss_sa
    AS
  SELECT b.*, r.order_id, r.order_acronym, r.order_name_english, r.order_type
  FROM b_bishops_reducido b
  LEFT JOIN religious_orders r ON b.religious_order_id = r.order_id
  WHERE affiliated = FALSE
        AND date_nomination > '1300-01-01'
        AND date_end < '1599-12-31'
        AND religious_order_id IS NOT NULL;


CREATE TEMP VIEW cojones1 AS 
WITH diocconfrailes AS 
(SELECT DISTINCT 
       diocese_id, order_id
 FROM vistas.b_cph_ss_sa b), 

cruzada AS (
     SELECT r.order_id,
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

diocsinfrailes AS (
     SELECT order_id, fundacionorder, diocese_id, fundaciondioc 
     FROM cruzada c
     WHERE NOT EXISTS (SELECT 1, 2 FROM diocconfrailes df
                       WHERE df.diocese_id = c.diocese_id AND df.order_id = c.order_id ))

SELECT order_id, fundacionorder, diocese_id, fundaciondioc
FROM diocsinfrailes;
--WHERE fundacionorder::integer > fundaciondioc;


  CREATE OR REPLACE VIEW vistas.b_cph_ss_sa_top10orders
  AS
  WITH x AS (
       SELECT b.order_id, COUNT(*) cuenta
       FROM b_cph_ss_sa b
       GROUP BY b.order_id
       ORDER BY cuenta
       DESC LIMIT 10)
  SELECT b.* FROM b_cph_ss_sa b
  JOIN x ON b.religious_order_id = x.order_id;


--- diez órdenes más ipt
--- coger d3
SELECT b.order_id, r.order_acronym, r.order_name_english,
       r.order_nickname,
       COUNT(*) cuenta
FROM b_cph_ss_sa b
JOIN religious_orders r USING(order_id)
GROUP BY b.order_id, r.order_acronym, r.order_name_english,
         r.order_nickname
ORDER BY cuenta
DESC LIMIT 10;

--- total de posiciones de seculares a 
SELECT COUNT(*) total 
FROM b_cph_cs_sa b
WHERE order_id IS NULL;

--- total de posiciones todos jusnto 
SELECT COUNT(*) total 
FROM b_cph_cs_sa b;

--- total de posiciones de seculares a 
SELECT COUNT(*) total 
FROM b_cph_cs_sa b
WHERE order_id IS not NULL;

--- total de diócesis 
SELECT COUNT(DISTINCT diocese_id) cuenta
FROM b_cph_cs_sa b;


--- otro
WITH diocconfrailes AS 
(SELECT DISTINCT 
       diocese_id, order_id
 FROM vistas.b_cph_ss_sa b), 

cruzada AS (
     SELECT r.order_id,
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

diocsinfrailes AS (
     SELECT order_id, fundacionorder, diocese_id, fundaciondioc 
     FROM cruzada c
     WHERE NOT EXISTS (SELECT 1, 2 FROM diocconfrailes df
                       WHERE df.diocese_id = c.diocese_id AND df.order_id = c.order_id )),
diocorrg AS (
SELECT order_id, fundacionorder, diocese_id, fundaciondioc
FROM diocsinfrailes) --WHERE fundacionorder::integer > fundaciondioc;

SELECT d.order_id, r.order_acronym, r.order_nickname,
       diocese_id, dd.diocese_name, p.country, 
       p.longitude, p.latitude 
FROM diocorrg d
JOIN general.religious_orders r USING(order_id)
JOIN general.dioceses dd USING(diocese_id)
LEFT JOIN general.places p USING(place_id)
WHERE d.order_id = 121
ORDER BY dd.diocese_name;


-- otro 
WITH paisestotales AS (
     SELECT country, COUNT(d.diocese_id) AS countrytotal
     FROM general.places
     JOIN general.dioceses d USING(place_id)
     GROUP BY country),
     
orrgtotales AS (
     SELECT d.order_id, r.order_acronym, p.country, COUNT(*) AS dioctotales
     FROM tv_diocesissinfrailes_edm d -- esto es temp view anterior! 
     JOIN general.religious_orders r USING(order_id)
     JOIN general.dioceses dd USING(diocese_id)
     JOIN general.places p USING(place_id)
     WHERE d.order_id = 121
     GROUP BY d.order_id, r.order_acronym, p.country)

SELECT order_id, order_acronym AS orden, o.country AS pais,
       countrytotal AS totalpais, dioctotales AS faltan,
       round(dioctotales::numeric / countrytotal::NUMERIC, 3) AS percentage
FROM orrgtotales o
JOIN paisestotales USING(country);



create or replace function get_absences (
  orderid INT 
) 
	returns TABLE (
        r_order_id int, r_order_acronym varchar, r_order_nickname varchar,
       r_diocese_id INT, r_diocese_name varchar, r_country VARCHAR, 
       r_longitude REAL, r_latitude real
	) 
	language plpgsql
as $$
begin
	return query 
WITH diocconfrailes AS 
(SELECT DISTINCT 
       diocese_id, order_id
 FROM vistas.b_cph_ss_sa b), 

cruzada AS (
     SELECT r.order_id,
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

diocsinfrailes AS (
     SELECT order_id, fundacionorder, diocese_id, fundaciondioc 
     FROM cruzada c
     WHERE NOT EXISTS (SELECT 1, 2 FROM diocconfrailes df
                       WHERE df.diocese_id = c.diocese_id AND df.order_id = c.order_id )),
diocorrg AS (
SELECT order_id, fundacionorder, diocese_id, fundaciondioc
FROM diocsinfrailes) --WHERE fundacionorder::integer > fundaciondioc;

SELECT d.order_id, r.order_acronym, r.order_nickname,
       diocese_id, dd.diocese_name, p.country, 
       p.longitude, p.latitude 
FROM diocorrg d
JOIN general.religious_orders r USING(order_id)
JOIN general.dioceses dd USING(diocese_id)
LEFT JOIN general.places p USING(place_id)
WHERE d.order_id = 121
ORDER BY dd.diocese_name;
END;$$


WITH paisestotales AS (
     SELECT country, COUNT(d.diocese_id) AS countrytotal
     FROM general.places
     JOIN general.dioceses d USING(place_id)
     GROUP BY country),
     
orrgtotales AS (
     SELECT r_order_id, r_order_acronym, r_country, COUNT(*) AS dioctotales
     FROM get_absences(121)
     GROUP BY r_order_id, r_order_acronym, r_country)

SELECT o.r_order_id, o.r_order_acronym AS orden, o.r_country AS pais,
       countrytotal AS totalpais, dioctotales AS faltan,
       round(dioctotales::numeric / countrytotal::NUMERIC, 3) AS percentage
FROM orrgtotales o
JOIN paisestotales P ON p.country = o.r_country;


CREATE OR REPLACE FUNCTION order_absent_dioceses (order_searching INT) 
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
     FROM vistas.b_cph_ss_sa b), 
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


  WITH diocconfrailes AS 
   (SELECT DISTINCT 
     diocese_id, order_id
     FROM vistas.b_cph_ss_sa b), 
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
     FROM diocsinfrailes WHERE fundaciondioc < 1600 OR fundaciondioc IS null)
-- lo hacemosmás presentable 
  SELECT d.order_id, r.order_acronym, r.order_nickname,
         diocese_id, dd.diocese_name, p.country, 
         p.longitude, p.latitude 
  FROM diocorrg d
  JOIN general.religious_orders r USING(order_id)
  JOIN general.dioceses dd USING(diocese_id)
  LEFT JOIN general.places p USING(place_id)
  WHERE d.order_id = 121
  ORDER BY dd.diocese_name;  


--- ver fechas de fundción en espña 
SELECT diocese_id,
      diocese_name, (d.other_data->'gcatholic'->>'foundation')::integer AS fundaciondioc 
FROM general.dioceses d
JOIN places P USING (place_id)
WHERE p.country='Spain'
ORDER BY diocese_name;

SELECT diocese_id,
      diocese_name, (d.other_data->'gcatholic'->>'foundation')::integer AS fundaciondioc 
FROM general.dioceses d
JOIN places P USING (place_id)
WHERE p.country='Malta'
ORDER BY diocese_name;

SELECT diocese_id,
      diocese_name, (d.other_data->'gcatholic'->>'foundation')::integer AS fundaciondioc 
FROM general.dioceses d
JOIN places P USING (place_id)
WHERE p.country='France'
ORDER BY fundaciondioc;


CREATE OR replace VIEW vistas.bishops_individuals_cph_op AS 
WITH concretos AS
  (SELECT url,
          bishop_surname || bishop_name AS bishop_fullname,
          diocese_id, date_nomination, date_end,
          order_id, order_acronym, order_name_english, order_type
  FROM vistas.b_cph_cs_sa
  WHERE order_id = 121)

SELECT url, bishop_fullname,
       order_id, order_acronym, order_name_english, order_type,
       diocese_ID, dioceses.diocese_name,
       longitude, latitude,
       date_nomination, date_end,
       date_end - date_nomination AS duracion,
       round(((date_end - date_nomination)::NUMERIC/365)::DECIMAL, 2) AS anos,
       ROW_NUMBER() OVER (PARTITION BY url ORDER BY date_nomination ) AS ordinal
FROM concretos
JOIN general.dioceses USING(diocese_id)
LEFT JOIN general.places USING (place_id)
ORDER BY url, date_nomination;



SELECT url, bishop_fullname, COUNT(*) AS total 
FROM periplo_cph_op
GROUP BY url, bishop_fullname;

SELECT ordinal, round(AVG(anos),2) FROM vistas.bishops_individuals_cph_op
GROUP BY ordinal
ORDER BY ordinal;

SELECT ordinal, round(stddev(anos),2) FROM vistas.bishops_individuals_cph_op
GROUP BY ordinal
ORDER BY ordinal;

SELECT round(stddev(anos),2) FROM vistas.bishops_individuals_cph_op;

SELECT * FROM vistas.bishops_individuals_cph_op WHERE ordinal = 5;

SELECT COUNT(url) FROM bishops_individuals_cph_op;
SELECT COUNT(DISTINCT url) FROM bishops_individuals_cph_op;

SELECT COUNT(DISTINCT url) FROM vistas.periplo_cph_op;

SELECT * FROM vistas.periplo_cph_op LIMIT 10;


WITH huecos AS (
  SELECT diocese_id, d.diocese_name,
         date_end AS fin,
         LEAD(date_nomination) OVER (PARTITION BY diocese_id ORDER BY date_nomination) AS siguiente
         FROM vistas.b_edm_cs_sa
         JOIN dioceses d USING(diocese_id))
  SELECT diocese_id, diocese_name, fin, siguiente,
         siguiente - fin AS diferencia, round((siguiente - fin)::NUMERIC/365,2) AS anos
  FROM huecos
  WHERE siguiente > fin
  ORDER BY siguiente;


DROP MATERIALIZED VIEW IF EXISTS vistas.related_dioceses_ops_cph CASCADE; 

CREATE materialized VIEW vistas.related_dioceses_ops_cph
AS
WITH d_basic AS   --- CTE 1
     (SELECT a.diocese_id, d.diocese_name, p.latitude, p.longitude,
            f_related_dioceses(a.diocese_id, '"b_cph_ss_sa"', true) AS vinculadas
     FROM (SELECT DISTINCT diocese_id
           FROM vistas.b_cph_ss_sa
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


--- cuántos

SELECT url, bishop_fullname, COUNT(*) AS total 
FROM vistas.periplo_cph_op -- view 
GROUP BY url, bishop_fullname;

SELECT url, bishop_fullname, COUNT(*) AS total 
FROM vistas.bishops_individuals_cph_op
GROUP BY url, bishop_fullname;

-- mirar con un poco más de detalle los q están en dos
SELECT b.*, p.country
FROM vistas.bishops_individuals_cph_op b
JOIN 
 (SELECT url, COUNT(*) AS total 
  FROM vistas.bishops_individuals_cph_op
  GROUP BY url
  HAVING COUNT(*)= 2) j USING (url)
JOIN dioceses d USING (diocese_id)
JOIN places P USING (place_id);

SELECT b.diocese_name, p.country, b.longitude, b.latitude, SUM(anos)
FROM vistas.bishops_individuals_cph_op b
JOIN dioceses d USING (diocese_id)
LEFT JOIN places P USING (place_id)
GROUP BY 1,2,3,4
ORDER BY SUM(anos) DESC;

--- contando diócesis por su número de OPs
SELECT b.diocese_name, p.country, b.longitude, b.latitude, count(*)
FROM vistas.bishops_individuals_cph_op b
JOIN dioceses d USING (diocese_id)
LEFT JOIN places P USING (place_id)
WHERE b.longitude IS NOT null
GROUP BY 1,2,3,4
ORDER BY COUNT(*) DESC;

SELECT diocesis_a, COUNT(*) AS total 
FROM vistas.related_dioceses_ops_cph
GROUP BY diocesis_a
ORDER BY total DESC;

--- serie temporal
WITH anyos AS (
      SELECT generate_series('1300-01-01'::TIMESTAMP,
                             '1600-01-01'::TIMESTAMP,
                             '1 year'::interval) AS serie )
SELECT order_acronym, serie, p.country, COUNT(*) AS totalobispos
FROM anyos, vistas.b_cph_ss_sa
JOIN general.dioceses USING (diocese_id)
LEFT JOIN general.places p USING (place_id)
WHERE EXTRACT(YEAR FROM serie) BETWEEN EXTRACT(YEAR FROM date_nomination)
                        AND EXTRACT(YEAR FROM date_end)
      AND order_id = 121 
GROUP BY order_acronym, anyos.serie, p.country
ORDER BY anyos.serie;

WITH anyos AS (
      SELECT generate_series('1300-01-01'::TIMESTAMP,
                             '1600-01-01'::TIMESTAMP,
                             '1 year'::interval) AS serie ),
j AS 
(SELECT serie, p.country, COUNT(*) AS totalobispos
FROM anyos, vistas.b_cph_ss_sa
JOIN general.dioceses USING (diocese_id)
LEFT JOIN general.places p USING (place_id)
WHERE EXTRACT(YEAR FROM serie) BETWEEN EXTRACT(YEAR FROM date_nomination)
                        AND EXTRACT(YEAR FROM date_end)
      AND order_id = 121 
GROUP BY anyos.serie, p.country
ORDER BY anyos.serie)
SELECT * FROM j
WHERE totalobispos = 0;


CREATE OR REPLACE TEMP VIEW cojones2 AS
WITH anyos AS (
      SELECT generate_series('1300-01-01'::TIMESTAMP,
                             '1600-01-01'::TIMESTAMP,
                             '1 year'::interval) AS serie )
SELECT order_acronym, serie, p.country, COUNT(*) AS totalobispos
FROM anyos, vistas.b_cph_ss_sa
JOIN general.dioceses USING (diocese_id)
LEFT JOIN general.places p USING (place_id)
WHERE EXTRACT(YEAR FROM serie) BETWEEN EXTRACT(YEAR FROM date_nomination)
                        AND EXTRACT(YEAR FROM date_end)
      AND order_id = 121 
GROUP BY order_acronym, anyos.serie, p.country
ORDER BY anyos.serie;


SELECT *
FROM  (
   SELECT day::date
   FROM   generate_series(timestamp '2007-12-01'
                        , timestamp '2008-12-01'
                        , interval  '1 month') day
   ) d
LEFT   JOIN (
   SELECT date_trunc('month', date_col)::date AS day
        , count(*) AS some_count
   FROM   tbl
   WHERE  date_col >= date '2007-12-01'
   AND    date_col <= date '2008-12-06'
-- AND    ... more conditions
   GROUP  BY 1
   ) t USING (day)
ORDER  BY DAY;

-----
SELECT *
FROM ( SELECT generate_series(1300,1600,1) AS anyo) anyos
LATERAL LEFT JOIN (
     SELECT order_acronym, COUNT(*) AS totalobispos
     FROM vistas.b_cph_ss_sa
     JOIN anyos A ON a.anyo BETWEEN EXTRACT(YEAR FROM date_nomination)
               AND EXTRACT(YEAR FROM date_end)
     WHERE order_id = 121
      GROUP BY 1, 2
) ops USING (anyo)
ORDER BY anyo;

--- otra opción
SELECT *
FROM (
      SELECT EXTRACT( YEAR FROM generate_series('1300-01-01'::TIMESTAMP,
                             '1600-01-01'::TIMESTAMP,
                             '1 year'::interval)) AS anyo
) anyos
LEFT JOIN (
     SELECT EXTRACT(YEAR FROM date_nomination) AS anyo,
            order_acronym, COUNT(*) AS totalobispos
     FROM vistas.b_cph_ss_sa
     WHERE order_id = 121
      GROUP BY 1, 2
) ops USING (anyo)
ORDER BY anyo;


WITH anyos AS (
SELECT generate_series(1300, 1599, 1) AS serie ),
j AS
(SELECT order_acronym, a.serie,
        p.country, COUNT(*) AS totalobispos
FROM vistas.b_cph_ss_sa
JOIN anyos A  ON a.serie BETWEEN EXTRACT(YEAR FROM date_nomination)
               AND EXTRACT(YEAR FROM date_end)
JOIN general.dioceses USING (diocese_id)
LEFT JOIN general.places p USING (place_id)
WHERE  order_id = 121 
GROUP BY 1, 2, 3)
SELECT anyos.serie, country, COALESCE(totalobispos, 0)
FROM anyos 
LEFT JOIN j USING(serie)
ORDER BY serie;



CREATE temp TABLE testy (a int,b text);
INSERT INTO testy VALUES (3,'test');
SELECT testy.*,generate_series(1,a) from testy;  --returns 3 rows


CREATE OR REPLACE FUNCTION bishops_order_per_year (order_searching INT,
       year_searching INT, country_searching VARCHAR) 
  RETURNS int
LANGUAGE  plpgsql STABLE
AS $$
 DECLARE total INT;
BEGIN
 SELECT INTO total
  (SELECT COUNT(*) FROM vistas.b_cph_ss_sa
   JOIN general.dioceses USING (diocese_id)
   LEFT JOIN general.places p USING (place_id)
   WHERE year_searching BETWEEN EXTRACT(YEAR FROM date_nomination)
                        AND EXTRACT(YEAR FROM date_end)
      AND order_id = order_searching
      AND p.country = country_searching);

   RETURN total;
END;
$$


CREATE OR REPLACE FUNCTION bishops_order_per_year_without_country (order_searching INT,
       year_searching INT) 
  RETURNS int
LANGUAGE plpgsql IMMUTABLE 
AS $$
 DECLARE total INT;
BEGIN
 SELECT INTO total
  (SELECT COUNT(*) FROM vistas.b_cph_ss_sa
   WHERE year_searching BETWEEN EXTRACT(YEAR FROM date_nomination)
                        AND EXTRACT(YEAR FROM date_end)
      AND order_id = order_searching);

   RETURN total;
END;
$$

EXPLAIN analyze
WITH anyos AS (
      SELECT generate_series(1300, 1599, 1) AS serie),
j AS (
  SELECT DISTINCT p.country 
  FROM dioceses
  JOIN places P USING(place_id)
)
SELECT serie, country, bishops_order_per_year(121, serie, country)
FROM anyos, j;

DROP INDEX places_country_idx;

WITH anyos AS (
      SELECT generate_series(1300, 1599, 1) AS serie)
SELECT serie, bishops_order_per_year_without_country(121, serie)
FROM anyos;

WITH anyos AS (
      SELECT generate_series(1300, 1599, 1) AS serie)
SELECT serie, bishops_order_per_year(121, serie, 'Portugal')
FROM anyos;


SELECT * FROM vistas.bishops_individuals_cph_op
JOIN dioceses d USING (diocese_id)
JOIN places P USING (place_id)
WHERE p.country = 'Denmark';

SELECT CASE
       WHEN religious_order IS NOT NULL THEN 'Non-secular'
       WHEN religious_order IS NULL THEN 'Secular'
       END AS bishoptype,
       COUNT(*) AS total 
FROM vistas.b_cph_cs_sa
GROUP BY bishoptype;

SELECT COUNT(DISTINCT diocese_id) FROM vistas.b_cph_cs_sa;

SELECT COUNT(*) FROM bishops_individuals_cph_op;



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

CREATE OR replace VIEW vistas.bishops_individuals_cph_op AS 
WITH concretos AS
  (SELECT url,
          bishop_surname || bishop_name AS bishop_fullname,
          diocese_id, date_nomination, date_end,
          order_id, order_acronym, order_name_english, order_type
  FROM vistas.b_cph_cs_sa
  WHERE order_id = 121)

SELECT url, bishop_fullname,
       order_id, order_acronym, order_name_english, order_type,
       diocese_ID, dioceses.diocese_name,
       longitude, latitude,
       date_nomination, date_end,
       date_end - date_nomination AS duracion,
       round(((date_end - date_nomination)::NUMERIC/365)::DECIMAL, 2) AS anos,
       ROW_NUMBER() OVER (PARTITION BY url ORDER BY date_nomination ) AS ordinal
FROM concretos
JOIN general.dioceses USING(diocese_id)
LEFT JOIN general.places USING (place_id)
ORDER BY url, date_nomination;
