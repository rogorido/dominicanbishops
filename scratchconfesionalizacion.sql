CREATE OR REPLACE VIEW b_bishops_reducido as
SELECT * FROM bishops_all
WHERE url NOT IN
     ('/bishop/bcadcl.html',
     '/bishop/bcarafao.html',
     '/bishop/bcesa.html',
     '/bishop/bciboi.html',
     '/bishop/bcolonnap.html',
     '/bishop/bdalbret.html',
     '/bishop/bdaragl.html',
     '/bishop/bdarmg.html',
     '/bishop/bdemg.html',
     '/bishop/bdeste.html',
     '/bishop/bcesipe.html',
     '/bishop/bderovs.html',
     '/bishop/bvaldes.html',
     '/bishop/bbourv.html',
     '/bishop/bcioc.html',
     '/bishop/bbellay.html',
     '/bishop/bdestou.html',
     '/bishop/bdvallea.html',
     '/bishop/bcupis.html',
     '/bishop/bfarna.html',
     '/bishop/bfieschin.html',
     '/bishop/bgrimanim.html',
     '/bishop/bkrop.html',
     '/bishop/blopezdcy.html',
     '/bishop/blorrj.html',
     '/bishop/bremol.html',
     '/bishop/bridolfin.html',
     '/bishop/brov.html',
     '/bishop/bsalviatg.html',
     '/bishop/bsari.html',
     '/bishop/bsoderinf.html',
     '/bishop/btrivua.html');

CREATE OR REPLACE VIEW vistas.b_cnf_cs_ca
  AS
SELECT b.*, r.order_id, r.order_acronym, r.order_name_english, r.order_type
FROM b_bishops_reducido b
LEFT JOIN religious_orders r ON b.religious_order_id = r.order_id
WHERE date_nomination > '1560-01-01'
      AND (date_end < '1650-01-01' OR date_end IS  NULL);

CREATE OR REPLACE VIEW vistas.b_cnf_ss_ca
  AS
SELECT b.*, r.order_id, r.order_acronym, r.order_name_english, r.order_type
FROM b_bishops_reducido b
LEFT JOIN religious_orders r ON b.religious_order_id = r.order_id
WHERE religious_order_id IS NOT NULL and date_nomination > '1560-01-01'
      AND (date_end < '1650-01-01' OR date_end IS  NULL);

CREATE OR REPLACE VIEW vistas.b_cnf_ss_sa
  AS
SELECT b.*, r.order_id, r.order_acronym, r.order_name_english, r.order_type
FROM b_bishops_reducido b
LEFT JOIN religious_orders r ON b.religious_order_id = r.order_id
WHERE religious_order_id IS NOT NULL and affiliated = FALSE and date_nomination > '1560-01-01'
      AND (date_end < '1650-01-01' OR date_end IS  NULL);

CREATE OR REPLACE VIEW vistas.b_cnf_cs_sa
  AS
SELECT b.*, r.order_id, r.order_acronym, r.order_name_english, r.order_type
FROM b_bishops_reducido b
LEFT JOIN religious_orders r ON b.religious_order_id = r.order_id
WHERE affiliated = FALSE and date_nomination > '1560-01-01' AND
      (date_end < '1650-01-01' OR date_end IS  NULL);

CREATE OR REPLACE VIEW vistas.b_cnf_cs_sa
  AS
SELECT b.*, r.order_id, r.order_acronym, r.order_name_english, r.order_type
FROM b_bishops_reducido b
LEFT JOIN religious_orders r ON b.religious_order_id = r.order_id
WHERE affiliated = FALSE
      AND date_nomination > '1560-01-01'
      AND date_nomination < '1650-01-01';

DROP VIEW vistas.bishops_individuals_cnf_op CASCADE;
CREATE OR replace VIEW vistas.bishops_individuals_cnf_op AS
WITH concretos AS
  (SELECT url,
          bishop_surname || bishop_name AS bishop_fullname,
          diocese_id, date_nomination, date_end,
          order_id, order_acronym, order_name_english, order_type,
          reason_begin, reason_end
  FROM vistas.b_cnf_cs_sa
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

CREATE OR REPLACE VIEW vistas.b_all_ss_sa_ofm
  AS
SELECT b.*, (case
                when r.order_id = 118 then 118
                when r.order_id = 120 then 118
                else r.order_id
              end)
                ,
                (case
                        when r.order_acronym = 'O.F.M. Conv.' then 'O.F.M.'
                        else r.order_acronym
                end)
                 ,
                 (case
                        when r.order_name_english = 'Order of Friars Minor Conventual' then 'Order of Friars Minor'
                        else r.order_name_english
                  end),
                  r.order_type
FROM b_bishops_reducido b
JOIN religious_orders r ON b.religious_order_id = r.order_id
WHERE religious_order_id IS NOT NULL and affiliated = FALSE;

CREATE OR REPLACE VIEW vistas.b_cnf_ss_sa_ofm
  AS
SELECT *
FROM vistas.b_all_ss_sa_ofm
WHERE date_nomination > '1560-01-01'
      AND (date_end < '1650-01-01' OR date_end IS  NULL);

CREATE OR REPLACE VIEW vistas.b_all_ss_sa_top10orders
AS
WITH x AS (
     SELECT b.order_id, COUNT(*) cuenta
     FROM b_all_ss_sa b
     GROUP BY b.order_id
     ORDER BY cuenta
     DESC LIMIT 10)
SELECT b.* FROM b_all_ss_sa b
JOIN x ON b.religious_order_id = x.order_id;

CREATE OR REPLACE VIEW vistas.b_cnf_ss_sa_top10orders
AS
WITH x AS (
     SELECT b.order_id, COUNT(*) cuenta
     FROM b_edm_ss_sa b
     GROUP BY b.order_id
     ORDER BY cuenta
     DESC LIMIT 10)
SELECT b.* FROM b_cnf_ss_sa b
JOIN x ON b.religious_order_id = x.order_id;

SELECT DISTINCT r.order_acronym,
       r.order_name_english AS order_name,
       r.order_type AS type_order,
       r.year_foundation,
       COUNT(bishop_all_id)
FROM religious_orders r
JOIN vistas.b_emd_ss_sa USING(order_id)
GROUP BY r.order_acronym,
      order_name,
      type_order,
      r.year_foundation
ORDER BY order_acronym;

CREATE OR REPLACE TEMP VIEW tv_consultageneral_ss_sa_edm AS
SELECT
       '<a href="http://www.catholic-hierarchy.org' || url || '" target="_blank">' || bishop_name || ' ' || bishop_surname || '</a>' AS url,
       religious_order,
       date_nomination,
       date_end,
       d.diocese_name,
       '<a href="' || url_hierarchy || '" target="_blank">' || d.diocese_name || '</a>' AS diocesis_url,
       p.country
FROM vistas.b_edm_ss_sa
JOIN dioceses d USING(diocese_id)
LEFT JOIN places P USING(place_id);

SELECT order_acronym, order_name_english, COUNT(*),
       ((count(*) * 100)::numeric/ (SELECT COUNT(*) FROM b_cnf_ss_sa))::decimal AS total
FROM b_cnf_ss_sa
GROUP BY order_acronym, order_name_english
ORDER BY total DESC;

SELECT order_acronym, order_name_english, COUNT(*),
       ((count(*) * 100)::numeric/ (SELECT COUNT(*) FROM b_cnf_ss_sa))::decimal AS total
FROM b_cnf_ss_sa
GROUP BY order_acronym, order_name_english
HAVING        ((count(*) * 100)::numeric/ (SELECT COUNT(*) FROM b_cnf_ss_sa))::decimal > 2
ORDER BY total DESC;

WITH a AS
     (SELECT d.diocese_name, religious_order, COUNT(*) AS cuenta
             FROM bishops_all b
             JOIN dioceses d USING (diocese_id)
             GROUP BY d.diocese_name, religious_order)
SELECT a.diocese_name, religious_order, cuenta, cuenta/SUM(cuenta)
OVER (PARTITION BY a.diocese_name)
FROM a
ORDER BY a.diocese_name;

WITH a AS
     (SELECT d.diocese_name, religious_order, COUNT(*) AS cuenta
             FROM vistas.b_all_withoutseculars_period_most_important b
             JOIN dioceses d USING (diocese_id)
             GROUP BY d.diocese_name, religious_order)
SELECT a.diocese_name, religious_order, cuenta, cuenta/SUM(cuenta)
OVER (PARTITION BY a.diocese_name)
FROM a
ORDER BY a.diocese_name;

CREATE TEMP VIEW b_all_sec_nonsec AS (
     SELECT diocese_ID, 'sinorder' AS orden, COUNT(*) AS total
     FROM bishops_ALL b
     WHERE religious_ORDER IS NULL
     GROUP BY diocese_ID, religious_ORDER
           UNION
     SELECT diocese_ID, 'conorder' AS orden, COUNT(*) AS total
     FROM bishops_ALL b
     WHERE religious_ORDER IS NOT NULL
     GROUP BY diocese_ID
     );

CREATE TEMP VIEW  b_all_sec_nonsec_withoutaffiliated AS (
     SELECT diocese_ID, 'sinorder' AS orden, COUNT(*) AS total
     FROM vistas.b_all_withoutaffiliated b
     WHERE religious_ORDER IS NULL
     GROUP BY diocese_ID, religious_ORDER
           UNION
     SELECT diocese_ID, 'conorder' AS orden, COUNT(*) AS total
     FROM vistas.b_all_withoutaffiliated b
     WHERE religious_ORDER IS NOT NULL
     GROUP BY diocese_ID
     );

WITH x AS ( SELECT * FROM b_all_sec_nonsec )
SELECT diocese_ID, d.diocese_NAME, x.orden, total
FROM dioceses d
JOIN x USING (diocese_ID)
ORDER BY d.diocese_NAME;

WITH x AS ( SELECT * FROM b_all_sec_nonsec )
SELECT diocese_id, d.diocese_name, ST_SetSRID(ST_MakePoint(p.longitude, p.latitude),4326) as coord,
       x.orden, total, total/sum(total)
OVER (PARTITION BY d.diocese_name)
FROM dioceses d
JOIN x USING (diocese_ID)
JOIN places p ON p.place_id = d.place_id
ORDER BY d.diocese_name;

WITH x AS ( SELECT * FROM b_all_sec_nonsec )
SELECT diocese_id, d.diocese_name, p.longitude, p.latitude,
       x.orden, total, total/sum(total)
OVER (PARTITION BY d.diocese_name)
FROM dioceses d
JOIN x USING (diocese_ID)
JOIN places p ON p.place_id = d.place_id
ORDER BY d.diocese_name;

WITH cruzada AS (
SELECT * FROM crosstab (
  $$SELECT diocese_ID, orden, total FROM b_ALL_sec_nonsec ORDER BY 1,2$$,
  $$SELECT DISTINCT orden FROM b_ALL_sec_nonsec ORDER BY 1$$
  )
  AS FINAL_RESULT(diocese_ID INT, conorder INT, sinorder INT)
  )
SELECT C.diocese_ID, d.diocese_NAME,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord, C.conorder, C.sinorder FROM cruzada C
JOIN dioceses d USING(diocese_ID)
     JOIN places P
     ON P.place_ID = d.place_ID
ORDER BY d.diocese_NAME;

WITH anyos AS (
      SELECT generate_series('1560-01-01'::TIMESTAMP,
                              '1650-01-01'::TIMESTAMP,
                              '1 year'::interval) AS serie )
SELECT order_acronym, serie, p.country, COUNT(*) AS totalobispos
FROM anyos, vistas.b_cnf_ss_sa_top10orders
JOIN dioceses USING (diocese_id)
LEFT JOIN places p USING (place_id)
WHERE serie BETWEEN EXTRACT(YEAR FROM date_nomination) AND EXTRACT(YEAR FROM date_end)
GROUP BY order_acronym, anyos.serie, p.country
ORDER BY anyos.serie;

SELECT DISTINCT d.diocese_id, d.diocese_name AS diocesis,
       p.country AS pais,
       ST_SetSRID(ST_MakePoint(p.longitude, p.latitude),4326) AS coord,
       b.order_acronym,
       COALESCE(
                SUM(EXTRACT(year from b.date_end)
                                 -
                    EXTRACT(year from b.date_nomination)), 0) AS duracion
FROM  dioceses d
     JOIN obispos_edm() b
     ON d.diocese_id = b.diocese_id
     LEFT OUTER JOIN places p ON p.place_id = d.place_id
GROUP BY d.diocese_id, diocesis, pais, coord, b.order_acronym;



CREATE OR replace temp VIEW tv_periplo_cnf_cs_sa AS
WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez
     (SELECT url, COUNT(*) AS total
      FROM vistas.b_cnf_cs_sa
      GROUP BY url
      HAVING COUNT(url) > 1),

-- luego con lo anterior seleccionamos las diócesis y las fechas
     diocconcretas AS
     (SELECT url, diocese_id, date_nomination, date_end, order_id, order_acronym, order_name_english, order_type
     FROM vistas.b_cnf_cs_sa
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

CREATE OR REPLACE TEMP VIEW tv_periplo_cnf_ss_sa AS
WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez
     (SELECT url, COUNT(*) AS total
      FROM vistas.b_cnf_ss_sa
      GROUP BY url
      HAVING COUNT(url) > 1),

-- luego con lo anterior seleccionamos las diócesis y las fechas
     diocconcretas AS
     (SELECT url, diocese_id, date_nomination, date_end, order_id, order_acronym, order_name_english, order_type
     FROM vistas.b_cnf_ss_sa
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

CREATE OR REPLACE TEMP VIEW tv_periplo_cnf_ss_sa AS
WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez
     (SELECT url, COUNT(*) AS total
      FROM vistas.b_cnf_ss_sa
      GROUP BY url
      HAVING COUNT(url) > 1),

-- luego con lo anterior seleccionamos las diócesis y las fechas
     diocconcretas AS
     (SELECT url, diocese_id, date_nomination, date_end, order_id, order_acronym, order_name_english, order_type
     FROM vistas.b_cnf_ss_sa
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

CREATE OR REPLACE TEMP VIEW vt_periplo_cnf_ss_sa AS
WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez
     (SELECT url, COUNT(*) AS total
      FROM vistas.b_cnf_ss_sa
      GROUP BY url
      HAVING COUNT(url) > 1),

-- luego con lo anterior seleccionamos las diócesis y las fechas
     diocconcretas AS
     (SELECT url, diocese_id, date_nomination, date_end, order_id, order_acronym, order_name_english, order_type
     FROM vistas.b_cnf_ss_sa
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

CREATE OR REPLACE TEMP VIEW tv_periplo_cnf_cs_sa_decadas AS
WITH obispostotales AS
  -- escogemos solo los obispos que aparecen más de una vez
       (SELECT url, COUNT(*) AS total
               FROM vistas.b_cnf_cs_sa
               GROUP BY url
               HAVING COUNT(url) > 1),
  -- luego con lo anterior las fechas
       diocconcretas AS
       (SELECT url, diocese_ID, date_nomination, date_end, order_id, order_acronym, order_name_english, order_type,
       ROW_NUMBER() OVER (PARTITION BY url ORDER BY date_nomination)
       FROM vistas.b_cnf_cs_sa
       JOIN obispostotales USING(url)),

  -- escogemos solo los primeros obispados de cada uno...
  todojunto as (
  SELECT url, order_id, order_acronym, order_name_english, order_type, diocese_ID, dioceses.diocese_NAME,
         date_nomination
  FROM diocconcretas
  JOIN dioceses USING(diocese_id)
  JOIN places USING (place_id)
  WHERE row_number = 1
  ORDER BY url, date_nomination),
  series AS
      (SELECT generate_series(1560, 1650, 10) AS r_from),
      range AS (
      SELECT r_from, (r_from + 9) AS r_to FROM series)
SELECT r_from, r_to,
       (SELECT count(*) FROM todojunto WHERE extract(year from date_nomination) BETWEEN r_from AND r_to) as total
FROM range;

CREATE OR replace temp VIEW tv_periplo_emd_cs_sa_media_decadas AS
WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez
     (SELECT url, COUNT(*) AS total
             FROM vistas.b_emd_cs_sa
             GROUP BY url
             HAVING COUNT(url) > 1),
-- luego con lo anterior seleccionamos las diócesis y las fechas
   concreto as (
     SELECT url, order_id, order_acronym, order_name_english, order_type, o.total, min(date_nomination) as date_nomination
     FROM vistas.b_emd_cs_sa
     JOIN obispostotales o USING(url)
     GROUP BY 1, 2, 3, 4, 5, 6 order by url),

   series AS
             (SELECT generate_series(1200, 1800, 10) AS r_from),
             range AS (
                   SELECT r_from, (r_from + 9) AS r_to FROM series)
SELECT r_from, r_to,
       (SELECT avg(total) FROM concreto WHERE extract(year from date_nomination) BETWEEN r_from AND r_to) as media,
       (SELECT stddev(total) FROM concreto WHERE extract(year from date_nomination) BETWEEN r_from AND r_to) as sd
FROM range;

CREATE OR replace temp VIEW tv_agregadodiocesis_emd_cs_sa AS

WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez
     (SELECT url, COUNT(*) AS total
      FROM vistas.b_emd_cs_sa
      GROUP BY url
      HAVING COUNT(url) > 1),

-- luego con lo anterior seleccionamos las diócesis y las fechas
     diocconcretas AS
     (SELECT url, diocese_id, date_nomination
     FROM vistas.b_emd_cs_sa
     JOIN obispostotales USING(url))

-- finalmente hacemos un agregado de las diócesis por las q pasan
SELECT url,
       ARRAY_AGG(diocese_name ORDER BY date_nomination) AS sucesion
FROM diocconcretas
JOIN dioceses USING(diocese_id)
GROUP BY url
ORDER BY url;

SELECT MAX(ARRAY_DIMS(sucesion)) from tv_agregadodiocesis_emd_cs_sa;

CREATE OR REPLACE TEMP VIEW tv_paresdiocesis_emd_cs_sa AS

--- juntamos todas los pares
WITH juntas AS (
     SELECT url, sucesion[1] AS dioc1, sucesion[2] AS dioc2
     FROM tv_agregadodiocesis_emd_cs_sa
     UNION ALL
     SELECT url, sucesion[2] AS dioc1, sucesion[3] AS dioc2
     FROM tv_agregadodiocesis_emd_cs_sa
     UNION ALL
     SELECT url, sucesion[3] AS dioc1, sucesion[4] AS dioc2
     FROM tv_agregadodiocesis_emd_cs_sa
     UNION ALL
     SELECT url, sucesion[4] AS dioc1, sucesion[5] AS dioc2
     FROM tv_agregadodiocesis_emd_cs_sa
     UNION ALL
     SELECT url, sucesion[5] AS dioc1, sucesion[6] AS dioc2
     FROM tv_agregadodiocesis_emd_cs_sa),

--- quitamos los pares donde haya algún null
sinnulls AS (
    SELECT * FROM juntas
    WHERE dioc1 IS NOT NULL AND dioc2 IS NOT NULL)

--- hacemos el recuento
SELECT dioc1, dioc2, COUNT(*) as total
FROM sinnulls
GROUP BY dioc1, dioc2
ORDER BY COUNT(*) DESC;

CREATE OR replace temp VIEW tv_agregadodiocesis_cnf_cs_sa AS

WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez
     (SELECT url, COUNT(*) AS total
      FROM vistas.b_cnf_cs_sa
      GROUP BY url
      HAVING COUNT(url) > 1),

-- luego con lo anterior seleccionamos las diócesis y las fechas
     diocconcretas AS
     (SELECT url, diocese_id, date_nomination
     FROM vistas.b_cnf_cs_sa
     JOIN obispostotales USING(url))

-- finalmente hacemos un agregado de las diócesis por las q pasan
SELECT url,
       ARRAY_AGG(diocese_name ORDER BY date_nomination) AS sucesion
FROM diocconcretas
JOIN dioceses USING(diocese_id)
GROUP BY url
ORDER BY url;

SELECT MAX(ARRAY_DIMS(sucesion)) from tv_agregadodiocesis;

CREATE OR REPLACE TEMP VIEW tv_paresdiocesis_cnf_cs_sa AS

--- juntamos todas los pares
WITH juntas AS (
     SELECT url, sucesion[1] AS dioc1, sucesion[2] AS dioc2
     FROM tv_agregadodiocesis_cnf_cs_sa
     UNION ALL
     SELECT url, sucesion[2] AS dioc1, sucesion[3] AS dioc2
     FROM tv_agregadodiocesis_cnf_cs_sa
     UNION ALL
     SELECT url, sucesion[3] AS dioc1, sucesion[4] AS dioc2
     FROM tv_agregadodiocesis_cnf_cs_sa
     UNION ALL
     SELECT url, sucesion[4] AS dioc1, sucesion[5] AS dioc2
     FROM tv_agregadodiocesis_cnf_cs_sa
     UNION ALL
     SELECT url, sucesion[5] AS dioc1, sucesion[6] AS dioc2
     FROM tv_agregadodiocesis_cnf_cs_sa),

--- quitamos los pares donde haya algún null
sinnulls AS (
    SELECT * FROM juntas
    WHERE dioc1 IS NOT NULL AND dioc2 IS NOT NULL)

--- hacemos el recuento
SELECT dioc1, dioc2, COUNT(*) as total
FROM sinnulls
GROUP BY dioc1, dioc2
ORDER BY COUNT(*) DESC;

CREATE OR replace temp VIEW tv_agregadodiocesis_emd_ss_sa AS

WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez
     (SELECT url, COUNT(*) AS total
      FROM vistas.b_emd_ss_sa
      GROUP BY url
      HAVING COUNT(url) > 1),

-- luego con lo anterior seleccionamos las diócesis y las fechas
     diocconcretas AS
     (SELECT url, diocese_id, date_nomination
     FROM vistas.b_emd_ss_sa
     JOIN obispostotales USING(url))

-- finalmente hacemos un agregado de las diócesis por las q pasan
SELECT url,
       ARRAY_AGG(diocese_name ORDER BY date_nomination) AS sucesion
FROM diocconcretas
JOIN dioceses USING(diocese_id)
GROUP BY url
ORDER BY url;

SELECT MAX(ARRAY_DIMS(sucesion)) from tv_agregadodiocesis_emd_cs_sa;

CREATE OR REPLACE TEMP VIEW tv_paresdiocesis_emd_cs_sa AS

--- juntamos todas los pares
WITH juntas AS (
     SELECT url, sucesion[1] AS dioc1, sucesion[2] AS dioc2
     FROM tv_agregadodiocesis_emd_cs_sa
     UNION ALL
     SELECT url, sucesion[2] AS dioc1, sucesion[3] AS dioc2
     FROM tv_agregadodiocesis_emd_cs_sa
     UNION ALL
     SELECT url, sucesion[3] AS dioc1, sucesion[4] AS dioc2
     FROM tv_agregadodiocesis_emd_cs_sa
     UNION ALL
     SELECT url, sucesion[4] AS dioc1, sucesion[5] AS dioc2
     FROM tv_agregadodiocesis_emd_cs_sa
     UNION ALL
     SELECT url, sucesion[5] AS dioc1, sucesion[6] AS dioc2
     FROM tv_agregadodiocesis_emd_cs_sa),

--- quitamos los pares donde haya algún null
sinnulls AS (
    SELECT * FROM juntas
    WHERE dioc1 IS NOT NULL AND dioc2 IS NOT NULL)

--- hacemos el recuento
SELECT dioc1, dioc2, COUNT(*) as total
FROM sinnulls
GROUP BY dioc1, dioc2
ORDER BY COUNT(*) DESC;

CREATE OR replace temp VIEW tv_agregadodiocesis_cnf_ss_sa AS

WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez
     (SELECT url, COUNT(*) AS total
      FROM vistas.b_cnf_ss_sa
      GROUP BY url
      HAVING COUNT(url) > 1),

-- luego con lo anterior seleccionamos las diócesis y las fechas
     diocconcretas AS
     (SELECT url, diocese_id, date_nomination
     FROM vistas.b_cnf_ss_sa
     JOIN obispostotales USING(url))

-- finalmente hacemos un agregado de las diócesis por las q pasan
SELECT url,
       ARRAY_AGG(diocese_name ORDER BY date_nomination) AS sucesion
FROM diocconcretas
JOIN dioceses USING(diocese_id)
GROUP BY url
ORDER BY url;

SELECT MAX(ARRAY_DIMS(sucesion)) from tv_agregadodiocesis_cnf_ss_sa;

CREATE OR REPLACE TEMP VIEW tv_paresdiocesis_cnf_cs_sa AS

--- juntamos todas los pares
WITH juntas AS (
     SELECT url, sucesion[1] AS dioc1, sucesion[2] AS dioc2
     FROM tv_agregadodiocesis_cnf_ss_sa
     UNION ALL
     SELECT url, sucesion[2] AS dioc1, sucesion[3] AS dioc2
     FROM tv_agregadodiocesis_cnf_ss_sa
     UNION ALL
     SELECT url, sucesion[3] AS dioc1, sucesion[4] AS dioc2
     FROM tv_agregadodiocesis_cnf_ss_sa
     UNION ALL
     SELECT url, sucesion[4] AS dioc1, sucesion[5] AS dioc2
     FROM tv_agregadodiocesis_cnf_ss_sa),

--- quitamos los pares donde haya algún null
sinnulls AS (
    SELECT * FROM juntas
    WHERE dioc1 IS NOT NULL AND dioc2 IS NOT NULL)

--- hacemos el recuento
SELECT dioc1, dioc2, COUNT(*) as total
FROM sinnulls
GROUP BY dioc1, dioc2
ORDER BY COUNT(*) DESC;

CREATE OR replace temp VIEW tv_agregadodiocesis_emd_OPs AS

WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez
     (SELECT url, COUNT(*) AS total
      FROM vistas.b_emd_ss_sa
      WHERE religious_order = 'O.P.'
      GROUP BY url
      HAVING COUNT(url) > 1),

-- luego con lo anterior seleccionamos las diócesis y las fechas
     diocconcretas AS
     (SELECT url, diocese_id, date_nomination
     FROM vistas.b_emd_ss_sa
     JOIN obispostotales USING(url))

-- finalmente hacemos un agregado de las diócesis por las q pasan
SELECT url,
       ARRAY_AGG(diocese_name ORDER BY date_nomination) AS sucesion
FROM diocconcretas
JOIN dioceses USING(diocese_id)
GROUP BY url
ORDER BY url;

SELECT MAX(ARRAY_DIMS(sucesion)) from tv_agregadodiocesis_emd_cs_sa;

CREATE OR REPLACE TEMP VIEW tv_paresdiocesis_emd_OPs AS

--- juntamos todas los pares
WITH juntas AS (
     SELECT url, sucesion[1] AS dioc1, sucesion[2] AS dioc2
     FROM tv_agregadodiocesis_emd_OPs
     UNION ALL
     SELECT url, sucesion[2] AS dioc1, sucesion[3] AS dioc2
     FROM tv_agregadodiocesis_emd_OPs
     UNION ALL
     SELECT url, sucesion[3] AS dioc1, sucesion[4] AS dioc2
     FROM tv_agregadodiocesis_emd_OPs
     UNION ALL
     SELECT url, sucesion[4] AS dioc1, sucesion[5] AS dioc2
     FROM tv_agregadodiocesis_emd_OPs),

--- quitamos los pares donde haya algún null
sinnulls AS (
    SELECT * FROM juntas
    WHERE dioc1 IS NOT NULL AND dioc2 IS NOT NULL)

--- hacemos el recuento
SELECT dioc1, dioc2, COUNT(*) as total
FROM sinnulls
GROUP BY dioc1, dioc2
ORDER BY COUNT(*) DESC;

CREATE OR replace temp VIEW tv_agregadodiocesis_cnf_OPs AS

WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez
     (SELECT url, COUNT(*) AS total
      FROM vistas.b_cnf_ss_sa
      WHERE religious_order = 'O.P.'
      GROUP BY url
      HAVING COUNT(url) > 1),

-- luego con lo anterior seleccionamos las diócesis y las fechas
     diocconcretas AS
     (SELECT url, diocese_id, date_nomination
     FROM vistas.b_cnf_ss_sa
     JOIN obispostotales USING(url))

-- finalmente hacemos un agregado de las diócesis por las q pasan
SELECT url,
       ARRAY_AGG(diocese_name ORDER BY date_nomination) AS sucesion
FROM diocconcretas
JOIN dioceses USING(diocese_id)
GROUP BY url
ORDER BY url;

SELECT MAX(ARRAY_DIMS(sucesion)) from tv_agregadodiocesis_emd_cs_sa;

CREATE OR REPLACE TEMP VIEW tv_paresdiocesis_cnf_OPs AS

--- juntamos todas los pares
WITH juntas AS (
     SELECT url, sucesion[1] AS dioc1, sucesion[2] AS dioc2
     FROM tv_agregadodiocesis_cnf_OPs
     UNION ALL
     SELECT url, sucesion[2] AS dioc1, sucesion[3] AS dioc2
     FROM tv_agregadodiocesis_cnf_OPs
     UNION ALL
     SELECT url, sucesion[3] AS dioc1, sucesion[4] AS dioc2
     FROM tv_agregadodiocesis_cnf_OPs
     UNION ALL
     SELECT url, sucesion[4] AS dioc1, sucesion[5] AS dioc2
     FROM tv_agregadodiocesis_cnf_OPs),

--- quitamos los pares donde haya algún null
sinnulls AS (
    SELECT * FROM juntas
    WHERE dioc1 IS NOT NULL AND dioc2 IS NOT NULL)

--- hacemos el recuento
SELECT dioc1, dioc2, COUNT(*) as total
FROM sinnulls
GROUP BY dioc1, dioc2
ORDER BY COUNT(*) DESC;

DROP MATERIALIZED VIEW IF EXISTS vistas.related_dioceses_cnf_ss_sa CASCADE;

--CREATE OR REPLACE TEMP VIEW tv_related_dioceses_cnf_cs_sa
CREATE materialized VIEW vistas.related_dioceses_cnf_ss_sa
AS
WITH d_basic AS   --- CTE 1
     (SELECT a.diocese_id, d.diocese_name, p.latitude, p.longitude,
            f_related_dioceses(a.diocese_id, '"b_cnf_ss_sa"', false) AS vinculadas
     FROM (SELECT DISTINCT diocese_id FROM vistas.b_cnf_ss_sa) A
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

DROP MATERIALIZED VIEW IF EXISTS vistas.related_dioceses_cnf_cs_sa CASCADE;

--CREATE OR REPLACE TEMP VIEW tv_related_dioceses_cnf_cs_sa
CREATE materialized VIEW vistas.related_dioceses_cnf_cs_sa
AS
WITH d_basic AS   --- CTE 1
     (SELECT a.diocese_id, d.diocese_name, p.latitude, p.longitude,
            f_related_dioceses(a.diocese_id, '"b_cnf_cs_sa"', false) AS vinculadas
     FROM (SELECT DISTINCT diocese_id FROM vistas.b_cnf_cs_sa) A
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

DROP MATERIALIZED VIEW IF EXISTS vistas.related_dioceses_cnf_ops CASCADE;

CREATE materialized VIEW vistas.related_dioceses_cnf_ops
AS
WITH d_basic AS   --- CTE 1
     (SELECT a.diocese_id, d.diocese_name, p.latitude, p.longitude,
            f_related_dioceses(a.diocese_id, '"b_cnf_ss_sa"', true) AS vinculadas
     FROM (SELECT DISTINCT diocese_id
           FROM vistas.b_cnf_ss_sa
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

CREATE OR REPLACE TEMP VIEW tv_motivos_fin_cs_cnf AS

WITH sumas AS (
     SELECT reason_end, religious_order AS rel_order,
            COUNT(*) AS total
     FROM vistas.b_cnf_ss_sa
     GROUP BY reason_end, religious_order)

SELECT reason_end, rel_order, total,
       (SELECT COUNT(*)
        FROM vistas.b_cnf_ss_sa
        WHERE religious_order = rel_order) totalobispos,
       total::numeric / (SELECT COUNT(*) FROM vistas.b_cnf_ss_sa WHERE religious_order = rel_order) AS porcentaje
FROM sumas;

CREATE OR REPLACE TEMP VIEW tv_motivos_fin_ss_cnf AS

WITH sumas AS (
     SELECT reason_end, religious_order AS rel_order,
            COUNT(*) AS total
     FROM vistas.b_cnf_ss_sa
     GROUP BY reason_end, religious_order)

SELECT reason_end, rel_order, total,
       (SELECT COUNT(*)
        FROM vistas.b_cnf_ss_sa
        WHERE religious_order = rel_order) totalobispos,
       total::numeric / (SELECT COUNT(*) FROM vistas.b_cnf_ss_sa WHERE religious_order = rel_order) AS porcentaje
FROM sumas;

SELECT d.diocese_id, d.diocese_name as diocesis,
       p.longitude, p.latitude,
       count(b.bishop_all_id)
FROM vistas.b_cnf_cs_sa b
     JOIN dioceses d USING (diocese_id)
     JOIN places P USING (place_id)
WHERE b.date_nomination > '1560-01-01' and b.date_nomination < '1700-01-01'
      AND 'Europe' = ANY (macroregions)
GROUP BY d.diocese_id, diocesis, p.longitude, p.latitude;

SELECT d.diocese_id, d.diocese_name as diocesis,
       p.longitude, p.latitude,
       b.religious_order, b.religious_order_id,
       count(b.bishop_all_id)
FROM b_cnf_ss_sa b
     JOIN dioceses d USING (diocese_id)
     LEFT JOIN places P USING (place_id)
WHERE b.date_nomination > '1560-01-01' and b.date_nomination < '1700-01-01'
      AND 'Europe' = ANY (macroregions)
GROUP BY d.diocese_id, diocesis, p.longitude, p.latitude,
         b.religious_order, b.religious_order_id;

SELECT DISTINCT d.diocese_id, d.diocese_name,
       b.religious_order, b.religious_order_id,
       p.country AS pais,
       p.longitude, p.latitude,
       COALESCE(
                SUM(EXTRACT(year from b.date_end)
                                 -
                    EXTRACT(year from b.date_nomination)), 0) AS duracion
FROM  dioceses d
     JOIN b_cnf_ss_sa b ON d.diocese_id = b.diocese_id
     LEFT OUTER JOIN places p ON p.place_id = d.place_id
  WHERE b.date_nomination > '1560-01-01' and b.date_nomination < '1700-01-01'
        AND 'Europe' = ANY (macroregions)
GROUP BY d.diocese_id, diocese_name,
      b.religious_order, b.religious_order_id, pais,
      p.longitude, p.latitude
ORDER BY duracion;

CREATE OR REPLACE TEMP VIEW tv_ops_bizancio AS

WITH bizancio AS
(SELECT DISTINCT place_id, place, country, longitude, latitude
 FROM  places
 WHERE country IN ('Greece', 'Ucraine', 'Romania', 'Bulgaria', 'Albania', 'Slovenia',
       'Croacia', 'Serbia', 'Montenegro', 'Cyprus', 'Lebanon', 'Israel', 'Palestina', 'Turkey' ))
SELECT d.diocese_id, d.diocese_name as diocesis,
       bz.longitude, bz.latitude, bz.place, bz.country,
       extract(year from date_nomination) as inicio,
       extract(year from date_end) as fin,
       extract(year from date_end) - extract(year from date_nomination) as duracion
FROM vistas.b_emd_ss_sa b
     JOIN dioceses d ON d.diocese_id = b.diocese_id
     JOIN bizancio bz ON bz.place_id = d.place_id
WHERE religious_order = 'O.P.'
ORDER BY inicio;

WITH bizancio as
(SELECT DISTINCT place_id, longitude, latitude FROM places
        WHERE country IN ('Greece', 'Ucraine', 'Romania', 'Bulgaria', 'Albania', 'Slovenia',
        'Croacia', 'Serbia', 'Montenegro', 'Cyprus', 'Lebanon', 'Israel', 'Palestina', 'Turkey' )
),
dioceses_bizancio AS
(SELECT DISTINCT d.diocese_id FROM dioceses d
     JOIN bizancio ON bizancio.place_id = d.place_id),
series AS
      (SELECT generate_series(1200, 1790, 10) AS r_from),
      rangos AS (
      SELECT r_from, (r_from + 9) AS r_to FROM series)
SELECT r_from, r_to,
       (SELECT count(*) FROM vistas.b_emd_ss_sa b
               JOIN dioceses_bizancio ON dioceses_bizancio.diocese_id = b.diocese_id
               WHERE (extract(year from date_nomination) BETWEEN r_from AND r_to))  AS total
FROM rangos;

WITH obispados AS (
     SELECT DISTINCT diocese_id, place_id
     FROM vistas.b_cnf_ss_sa
     JOIN dioceses d USING(diocese_id)
     LEFT JOIN places P USING(place_id)
     WHERE religious_order = 'O.P.')

SELECT h.name AS convent,
       o.diocese_id, o.place_id,
       p.place, p.country
FROM houses h
JOIN obispados o USING(place_id)
JOIN places p USING(place_id);

SELECT d.diocese_name, order_acronym, p.longitude, p.latitude, COUNT(*) AS total
FROM vistas.b_cnf_ss_sa_ofm
JOIN dioceses d USING(diocese_id)
LEFT JOIN places P USING(place_id)
WHERE order_acronym IN ('O.P.', 'O.F.M.')
GROUP BY order_acronym, d.diocese_name, p.longitude, p.latitude;

CREATE TEMP VIEW cojones1 AS
SELECT * FROM b_cnf_cs_sa
WHERE religious_order_id =121
ORDER BY date_nomination;

CREATE TEMP VIEW cojones2 AS
SELECT * FROM b_cnf_ss_sa
WHERE religious_order_id =121
ORDER BY date_nomination;

SELECT * FROM cojones1
WHERE bishop_all_id
      NOT IN (SELECT bishop_all_id FROM cojones2);

SELECT * FROM cojones2
WHERE date_end > '1651-01-01';

DROP VIEW vistas.b_cnf_ss_sa cascade;
CREATE OR REPLACE VIEW vistas.b_cnf_ss_sa
  AS
SELECT b.*, r.order_id, r.order_acronym, r.order_name_english, r.order_type
FROM b_bishops_reducido b
LEFT JOIN religious_orders r ON b.religious_order_id = r.order_id
WHERE religious_order_id IS NOT NULL and affiliated = FALSE and date_nomination > '1560-01-01'
      AND date_nomination < '1650-01-01';



SELECT COUNT(*) FILTER (WHERE anos < 15) AS menosdequince,
       COUNT(*) FILTER (WHERE anos >= 15 AND anos < 20) AS quinces,
       COUNT(*) FILTER (WHERE anos >= 20 AND anos < 25) AS veintesuno,
       COUNT(*) FILTER (WHERE anos >= 25 AND anos < 30) AS veintesdos,
       COUNT(*) FILTER (WHERE anos >= 30 AND anos < 35) AS treintasuno,
       COUNT(*) FILTER (WHERE anos >= 35 AND anos < 40) AS treintasdos,
       COUNT(*) FILTER (WHERE anos >= 40) AS cuarentas
FROM vistas.bishops_individuals_cnf_op b;


SELECT p.country, COUNT(*) AS total
FROM bishops_individuals_cnf_op b
JOIN dioceses d USING (diocese_id)
JOIN places P USING (place_id)
WHERE anos < 2
GROUP BY p.country
ORDER BY total DESC;

SELECT url, bishop_fullname, anos, diocese_name
FROM vistas.bishops_individuals_cnf_op b
WHERE anos IS NOT null
ORDER BY anos;


WITH j AS
(SELECT b.diocese_name, p.country,
       st_transform(ST_SetSRID(ST_MakePoint(b.longitude, b.latitude),4326), 54030) AS coord,
       COUNT(*) AS total
FROM vistas.bishops_individuals_cnf_op b
JOIN dioceses d USING (diocese_id)
LEFT JOIN places P USING (place_id)
WHERE b.longitude IS NOT null
GROUP BY 1,2,3
ORDER BY COUNT(*) DESC)
SELECT * FROM j;

  DROP VIEW IF EXISTS qgis.porcentajes_ops_cnf;
  CREATE or replace VIEW qgis.porcentajes_ops_cnf AS
  WITH j AS
  (SELECT diocese_id, COUNT(*) AS totalobispos,
         COUNT(*) FILTER  (WHERE religious_order IS NOT NULL) AS nonsecular,
         COUNT(*) FILTER  (WHERE religious_order = 'O.P.') AS ops
  FROM vistas.b_cnf_cs_sa
  GROUP BY 1)
  SELECT diocese_id, diocese_name,
         st_transform(ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326), 54030) AS coord,
         totalobispos, nonsecular, ops,
         nonsecular::REAL / totalobispos::REAL AS porcentajefrailes,
         ops::REAL / totalobispos::REAL AS porcentajeopscontotales,
         ops::REAL / NULLIF(nonsecular::REAL,0) AS porcentajeopsconfrailes,
         p.country,
         ROW_NUMBER() OVER() AS myid -- esto hace falta tvz por qgis
  FROM j
  LEFT JOIN dioceses USING (diocese_id)
  LEFT JOIN places P USING (place_id);


SELECT * FROM vistas.bishops_individuals_cnf_op
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id)
where country = 'Portugal';

SELECT * FROM vistas.bishops_individuals_cnf_op
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id)
where country in ('Portugal', 'Brazil');


SELECT * FROM vistas.bishops_individuals_cnf_op
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id)
where country in ('France');


-- los q están en irlanda
SELECT diocese_id, url, bishop_fullname,
       v.diocese_name,
       v.longitude, v.latitude,
       reason_begin, reason_end, date_nomination,
       date_end, duracion, anos
FROM vistas.bishops_individuals_cnf_op v
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id) where country in ('Ireland')
ORDER BY date_nomination;


-- los q están en españa
SELECT diocese_id, url, bishop_fullname,
       v.diocese_name,
       v.longitude, v.latitude,
       reason_begin, reason_end, date_nomination,
       date_end, duracion, anos
FROM vistas.bishops_individuals_cnf_op v
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id) where country in ('Spain')
ORDER BY date_nomination;

SELECT COUNT(DISTINCT url)
FROM vistas.bishops_individuals_cnf_op
JOIN dioceses d USING (diocese_id)
JOIN places USING (place_id)
where country in ('Spain');

SELECT COUNT(DISTINCT diocese_id)
FROM vistas.bishops_individuals_cnf_op
JOIN dioceses d USING (diocese_id)
JOIN places USING (place_id)
where country in ('Spain');

SELECT *
FROM vistas.bishops_individuals_cnf_op
JOIN dioceses d USING (diocese_id)
JOIN places USING (place_id)
where country in ('Spain')
ORDER BY anos desc
LIMIT 10;

SELECT COUNT(DISTINCT url)
FROM vistas.bishops_individuals_cnf_op
JOIN dioceses d USING (diocese_id)
JOIN places USING (place_id)
where country in ('Italy');


SELECT q.diocese_id, q.diocese_name, q.total, q.porcentaje
FROM qgis.anyostotales_cnf q
JOIN dioceses d USING (diocese_id)
JOIN places p USING (place_id)
where p.country in ('Italy')
ORDER BY total desc
LIMIT 15;

SELECT b.diocese_id, b.diocese_name, COUNT(*) AS total
FROM vistas.bishops_individuals_cnf_op b
JOIN dioceses d USING (diocese_id)
JOIN places p USING (place_id)
where p.country in ('Italy')
GROUP BY 1,2
ORDER BY total;

WITH j AS
(SELECT b.diocese_id, b.diocese_name, COUNT(*) AS total
   FROM vistas.bishops_individuals_cnf_op b
   JOIN dioceses d USING (diocese_id)
   JOIN places p USING (place_id)
   where p.country in ('Italy')
   GROUP BY 1,2)
SELECT total, COUNT(*) AS agregado
FROM j
GROUP BY 1;

  WITH j AS (
  SELECT diocese_id, diocese_name,
         (other_data->'gcatholic'->>'foundation')::integer AS fundacion
  FROM dioceses
  WHERE (other_data->'gcatholic'->>'foundation')::integer > 1560
         AND (other_data->'gcatholic'->>'foundation')::integer < 1650),
  K AS (
  SELECT b.*, j.diocese_name,
         ROW_NUMBER() OVER (PARTITION BY j.diocese_id ORDER BY date_nomination) AS r
  FROM b_edm_cs_sa b
  JOIN j USING (diocese_id)),
  h AS (
  SELECT * FROM K WHERE r <= 5
  )
  SELECT diocese_id, diocese_name, COUNT(*) AS total
  FROM h
  WHERE order_id = 121
  GROUP BY 1, 2
  ORDER BY total DESC;


-- lo de bacau
SELECT * FROM vistas.bishops_individuals_cnf_op b
WHERE diocese_id = 75;

-- los q están en españa
CREATE TEMP VIEW cojones1 AS
SELECT diocese_id, url, bishop_fullname,
       v.diocese_name,
       v.longitude, v.latitude,
       reason_begin, reason_end, date_nomination,
       date_end, duracion, anos
FROM vistas.bishops_individuals_cnf_op v
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id) where country in ('Spain')
ORDER BY date_nomination;

SELECT c.diocese_name, SUM(anos), SUM(anos) / 90
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id) where country in ('Spain')
GROUP BY 1
ORDER BY SUM(anos);

SELECT c.diocese_name, count(*)
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id) where country in ('Spain')
GROUP BY 1
ORDER BY COUNT(*);

WITH j AS
(SELECT c.diocese_name, COUNT(*) AS totales
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id) where country in ('Spain')
GROUP BY 1)
SELECT totales, COUNT(*) AS frecuencia
FROM j
GROUP BY 1;

SELECT DISTINCT url, bishop_fullname
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id) where country in ('Spain');

SELECT *
FROM vistas.bishops_individuals_cnf_op c
WHERE url = '/bishop/bviec.html';


--- Italia
SELECT c.diocese_name, SUM(anos), SUM(anos) / 90
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id) where country in ('Italy')
GROUP BY 1
ORDER BY SUM(anos);

SELECT c.diocese_name, count(*)
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id) where country in ('Italy')
GROUP BY 1
ORDER BY COUNT(*);

WITH j AS
(SELECT c.diocese_name, COUNT(*) AS totales
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id) where country in ('Italy')
GROUP BY 1)
SELECT totales, COUNT(*) AS frecuencia
FROM j
GROUP BY 1;

SELECT DISTINCT url, bishop_fullname, anos
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id) where country in ('Italy')
ORDER BY anos;

--- trevico
SELECT DISTINCT url, bishop_fullname, anos, date_nomination, date_end
FROM vistas.bishops_individuals_cnf_op c WHERE diocese_id= 849;

--- como
SELECT DISTINCT url, bishop_fullname, anos, date_nomination, date_end
FROM vistas.bishops_individuals_cnf_op C
WHERE diocese_id= 181;

--- caserta
SELECT DISTINCT url, bishop_fullname, anos, date_nomination, date_end
FROM vistas.bishops_individuals_cnf_op C
WHERE diocese_id= 137;

--- otras...
SELECT DISTINCT url, bishop_fullname, anos, date_nomination, date_end
FROM vistas.bishops_individuals_cnf_op C
WHERE diocese_id=331;

--- no
SELECT DISTINCT url, bishop_fullname, COUNT(*) AS total
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id) where country in ('Italy')
GROUP BY 1, 2
ORDER BY total;

--- rtt lo anterior no es del todo real pq me limita las fechas
SELECT DISTINCT url, bishop_fullname, COUNT(*) AS total
FROM vistas.bishops_individuals_edm_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id)
JOIN  (SELECT DISTINCT url FROM vistas.bishops_individuals_cnf_op) K USING (url)
where country in ('Italy')
GROUP BY 1, 2
ORDER BY total;

WITH j AS
(SELECT b.*,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord, p.country
FROM vistas.bishops_individuals_cnf_op b
JOIN dioceses d USING (diocese_id)
JOIN places P USING (place_id)
WHERE country = ANY(array['Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey']))
SELECT diocese_id, diocese_name, coord,
       COUNT(*) AS total
FROM j
GROUP BY 1, 2, 3
ORDER BY total;

SELECT c.diocese_name, SUM(anos), SUM(anos) / 90
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id)
WHERE country = ANY(array['Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey'])
GROUP BY 1
ORDER BY SUM(anos);

--- contando italia y adyacentes
SELECT count(DISTINCT url)
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id)
WHERE country = ANY(ARRAY['Italy', 'Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro',  'Albania', 'Cyprus',
               'Serbia']);


SELECT count(DISTINCT url)
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id)
WHERE country = ANY(ARRAY['Italy']);

---- contando todos
SELECT count(DISTINCT url)
FROM vistas.bishops_individuals_cnf_op C;


--- américa
SELECT DISTINCT url, bishop_fullname, anos
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id)
WHERE country IN
       ('Argentina', 'Bolivia', 'Chile', 'Colombia',
               'Cuba', 'Ecuador', 'Dominican Republic', 'Ecuador',
               'Guatemala', 'Honduras', 'Mexico', 'Nicaragua', 'Panamá',
               'Paraguay', 'Peru', 'Philippines', 'Puerto Rico',
               'Venezuela')
ORDER BY anos;

SELECT DISTINCT url, bishop_fullname, anos
FROM vistas.bishops_individuals_cnf_op C
WHERE bishop_fullname LIKE '%Lizá%';

--- fundación dde diócesis
SELECT d.diocese_id, d.diocese_name,
       (d.other_data->'gcatholic'->>'foundation')::INT AS fundacion
FROM dioceses d
JOIN places USING (place_id)
WHERE country IN
       ('Argentina', 'Bolivia', 'Chile', 'Colombia',
               'Cuba', 'Ecuador', 'Dominican Republic', 'Ecuador',
               'Guatemala', 'Honduras', 'Mexico', 'Nicaragua', 'Panamá',
               'Paraguay', 'Peru', 'Philippines', 'Puerto Rico',
               'Venezuela')
ORDER BY fundacion;

--- con esto sacamos lo q es menos de 90 años
SELECT d.diocese_id, d.diocese_name,
       (d.other_data->'gcatholic'->>'foundation')::INT AS fundacion,
        CASE
        WHEN (d.other_data->'gcatholic'->>'foundation')::INT <= 1560 THEN 90
        WHEN (d.other_data->'gcatholic'->>'foundation')::INT > 1560 THEN
             90 - ((d.other_data->'gcatholic'->>'foundation')::INT - 1560)
        END AS datoconvertido
FROM dioceses d
JOIN places USING (place_id)
WHERE country IN
       ('Argentina', 'Bolivia', 'Chile', 'Colombia',
               'Cuba', 'Ecuador', 'Dominican Republic', 'Ecuador',
               'Guatemala', 'Honduras', 'Mexico', 'Nicaragua', 'Panamá',
               'Paraguay', 'Peru', 'Philippines', 'Puerto Rico',
               'Venezuela');


-- lo ponemos en rlc con los años
WITH j AS
(SELECT d.diocese_id, d.diocese_name,
       (d.other_data->'gcatholic'->>'foundation')::INT AS fundacion,
        CASE
        WHEN (d.other_data->'gcatholic'->>'foundation')::INT <= 1560 THEN 90
        WHEN (d.other_data->'gcatholic'->>'foundation')::INT > 1560 THEN
             90 - ((d.other_data->'gcatholic'->>'foundation')::INT - 1560)
        END AS datoconvertido
FROM dioceses d
JOIN places USING (place_id)
WHERE country IN
       ('Argentina', 'Bolivia', 'Chile', 'Colombia',
               'Cuba', 'Ecuador', 'Dominican Republic', 'Ecuador',
               'Guatemala', 'Honduras', 'Mexico', 'Nicaragua', 'Panamá',
               'Paraguay', 'Peru', 'Philippines', 'Puerto Rico',
               'Venezuela')),
K AS
  (SELECT b.diocese_id,
         b.diocese_name,
         sum(b.anos) AS total
  FROM bishops_individuals_cnf_op b
  JOIN j USING (diocese_id)
  GROUP BY 1, 2)
SELECT k.diocese_id,
       k.diocese_name,
       total,
       total / j.datoconvertido AS porcentaje
FROM K
JOIN j USING (diocese_id)
-- GROUP BY 1, 2, 3
ORDER BY total DESC;


-- en las q no están
SELECT d.diocese_id, d.diocese_name,
       (d.other_data->'gcatholic'->>'foundation')::INT AS fundacion
FROM dioceses d
JOIN places USING (place_id)
WHERE country IN
       ('Argentina', 'Bolivia', 'Chile', 'Colombia',
               'Cuba', 'Ecuador', 'Dominican Republic', 'Ecuador',
               'Guatemala', 'Honduras', 'Mexico', 'Nicaragua', 'Panamá',
               'Paraguay', 'Peru', 'Philippines', 'Puerto Rico',
               'Venezuela')
AND diocese_id NOT IN
    (SELECT DISTINCT diocese_id FROM bishops_individuals_cnf_op b);


SELECT country, COUNT(*) AS total
FROM vistas.bishops_individuals_cnf_op
JOIN dioceses d USING (diocese_id)
JOIN places USING (place_id)
WHERE country IN
       ('Indonesia', 'Philippines', 'India', 'China', 'Vietnam',
       'Cambodia', 'Malaysia')
GROUP BY 1
ORDER BY total DESC;

SELECT country, COUNT(*) AS total
FROM vistas.bishops_individuals_cnf_op
JOIN dioceses d USING (diocese_id)
JOIN places USING (place_id)
WHERE country IN
       ('Azerbaijan')
GROUP BY 1
ORDER BY total DESC;

SELECT *
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id)
WHERE country IN
       ('Azerbaijan');

---
SELECT COUNT(DISTINCT url)
FROM vistas.bishops_individuals_cnf_op
JOIN dioceses d USING (diocese_id)
JOIN places USING (place_id)
WHERE country IN
       ('Argentina', 'Bolivia', 'Chile', 'Colombia',
               'Cuba', 'Ecuador', 'Dominican Republic', 'Ecuador',
               'Guatemala', 'Honduras', 'Mexico', 'Nicaragua', 'Panamá',
               'Paraguay', 'Peru', 'Puerto Rico',
               'Venezuela', 'Spain');

---
SELECT COUNT(DISTINCT diocese_id)
FROM vistas.bishops_individuals_cnf_op
JOIN dioceses d USING (diocese_id)
JOIN places USING (place_id)
WHERE country IN
       ('Argentina', 'Bolivia', 'Chile', 'Colombia',
               'Cuba', 'Ecuador', 'Dominican Republic', 'Ecuador',
               'Guatemala', 'Honduras', 'Mexico', 'Nicaragua', 'Panamá',
               'Paraguay', 'Peru', 'Puerto Rico',
               'Venezuela', 'Spain');

----
CREATE TEMP VIEW leches1 AS
WITH j AS (
SELECT diocese_id,
       (other_data->'gcatholic'->>'foundation')::integer AS fundacion
FROM dioceses
WHERE (other_data->'gcatholic'->>'foundation')::integer >= 1560 and
      (other_data->'gcatholic'->>'foundation')::integer <= 1650),
K AS (
SELECT *,
       ROW_NUMBER() OVER (PARTITION BY j.diocese_id ORDER BY date_nomination) AS r
FROM b_cnf_cs_sa
JOIN j USING (diocese_id))
SELECT *
FROM K
WHERE r <= 5
ORDER BY k.diocese_id, k.r;

SELECT DISTINCT diocese_id, diocese_name
FROM leches1
JOIN dioceses USING (diocese_id);


CREATE TEMP VIEW leches2 AS
WITH j AS (
SELECT diocese_id,
       (other_data->'gcatholic'->>'foundation')::integer AS fundacion
FROM dioceses
WHERE (other_data->'gcatholic'->>'foundation')::integer > 1650),
K AS (
SELECT *,
       ROW_NUMBER() OVER (PARTITION BY j.diocese_id ORDER BY date_nomination) AS r
FROM b_cnf_cs_sa
JOIN j USING (diocese_id))
SELECT *
FROM K
WHERE r <= 5
ORDER BY k.diocese_id, k.r;


  SELECT COUNT(DISTINCT url)
  FROM vistas.b_cnf_cs_sa
  JOIN dioceses d USING (diocese_id)
  JOIN places USING (place_id)
  WHERE country IN
         ('Argentina', 'Bolivia', 'Chile', 'Colombia',
                 'Cuba', 'Ecuador', 'Dominican Republic', 'Ecuador',
                 'Guatemala', 'Honduras', 'Mexico', 'Nicaragua', 'Panamá',
                 'Paraguay', 'Peru', 'Puerto Rico',
                 'Venezuela');


  SELECT COUNT(DISTINCT url)
  FROM vistas.b_cnf_cs_sa
  JOIN dioceses d USING (diocese_id)
  JOIN places USING (place_id)
  WHERE country IN
         ('Argentina', 'Bolivia', 'Chile', 'Colombia',
                 'Cuba', 'Ecuador', 'Dominican Republic', 'Ecuador',
                 'Guatemala', 'Honduras', 'Mexico', 'Nicaragua', 'Panamá',
                 'Paraguay', 'Peru', 'Puerto Rico',
                 'Venezuela')
         AND religious_order_id IS NOT NULL;

----
SELECT c.diocese_name, SUM(anos), SUM(anos) / 90
FROM vistas.bishops_individuals_cnf_op c
JOIN dioceses USING (diocese_id)
JOIN places USING (place_id)
WHERE country = ANY(array['Greece'])
GROUP BY 1
ORDER BY SUM(anos);
