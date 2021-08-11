SELECT * FROM vistas.dioceses_rents;

SELECT DISTINCT b.diocese_id, b.diocese_name
FROM vistas.bishops_individuals_edm_op b
JOIN dioceses d USING (diocese_id)
JOIN places P USING (place_id)
WHERE country = ANY(array['Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey']);

DROP VIEW adriatic_total;
CREATE OR REPLACE TEMP VIEW adriatic_total AS
SELECT d.*,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord, p.country
FROM general.dioceses d
JOIN places P USING (place_id)
WHERE country = ANY(array['Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey']);

SELECT d.*,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord, p.country
FROM general.dioceses d
JOIN places P USING (place_id)
WHERE country IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey');


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
ORDER BY total desc;

SELECT d.*,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord, p.country
FROM general.dioceses d
JOIN places P USING (place_id)
WHERE country IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey')
      AND diocese_id NOT IN
      (SELECT diocese_id FROM vistas.bishops_individuals_edm_op b);

WITH j AS
(SELECT d.*,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord, p.country
FROM general.dioceses d
JOIN places P USING (place_id)
WHERE country IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey'))
SELECT j.*, j.other_data->'gcatholic'->>'foundation'
FROM j
WHERE j.diocese_id NOT IN
      (SELECT diocese_id FROM vistas.bishops_individuals_edm_op b)
ORDER BY diocese_name;

DROP VIEW rentas_adriatico;
CREATE TEMP VIEW rentas_adriatico AS
select * from bishops.dioceses_global_fl_estimado
WHERE pais IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey');

---
SELECT * FROM adriatic_total
WHERE diocese_id NOT IN
 (select diocese_id from bishops.dioceses_global_fl_estimado
WHERE pais IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey'));

SELECT *,
       CASE
       WHEN diocese_id IN (SELECT DISTINCT diocese_id FROM vistas.bishops_individuals_edm_op)
            THEN 'conops'
       ELSE 'sinops'
       END AS oppresence
FROM rentas_adriatico
LEFT JOIN (SELECT DISTINCT diocese_id FROM vistas.bishops_individuals_edm_op) j
     USING (diocese_id);


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
     WHERE pais IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey')) AND
     (
SELECT percentile_cont(1) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey'))
AND pais IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey');


--- así en genearl
SELECT percentile_cont(array[0,0.25]) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey');


--- calculamos los más altos
DROP VIEW IF EXISTS adriatic_rents_high;
CREATE TEMP VIEW adriatic_rents_high AS
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
     WHERE pais IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey')) AND
     (
SELECT percentile_cont(1) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey'))
AND pais IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey');

--- calculamos los más basjos
DROP VIEW IF EXISTS adriatic_rents_low;
CREATE TEMP VIEW adriatic_rents_low AS
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
SELECT percentile_cont(0) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey')) AND
     (
SELECT percentile_cont(0.25) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey'))
AND pais IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey');

--- las más bajas
SELECT ops, COUNT(*) AS total
FROM adriatic_rents_low
GROUP BY ops;

--- las más altas
SELECT ops, COUNT(*) AS total
FROM adriatic_rents_high
GROUP BY ops;

-- miramos clusters de las diocesis de adriatico
WITH j AS
(SELECT * FROM vistas.related_dioceses_edm_ops r
JOIN general.dioceses d ON d.diocese_id = r.dioc_id_a
JOIN places P USING (place_id)
WHERE country IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey'))
SELECT diocesis_a, COUNT(*) AS total
FROM j
GROUP BY diocesis_a
ORDER BY total DESC;

WITH j AS
(SELECT d.diocese_id, d.diocese_name, d.diocese_latin_name,
        d.archidiocese, d.sufragean_id,
        d.nowadays, d.infidelibus, d.disappeared, d.titular_see,
        d.vatican, d.url_hierarchy,
        d.other_data->'gcatholic'->>'foundation',
        d.other_data->'gcatholic'->>'parishes_number',
        ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
        p.country,
        r.*,
        CASE
         WHEN d.diocese_id IN (SELECT DISTINCT diocese_id FROM
                              vistas.bishops_individuals_edm_op b)
              THEN 'conops'
         ELSE 'sinops'
        END AS oppresence,
        (SELECT COUNT(*) FROM vistas.bishops_individuals_edm_op b
         WHERE b.diocese_id = d.diocese_id)
 FROM general.dioceses d JOIN places P USING (place_id)
 LEFT OUTER JOIN dioceses_global_fl_estimado r USING (diocese_id))
SELECT j.* FROM j;

EXPLAIN ANALYZE
WITH j AS
(SELECT d.diocese_id, d.diocese_name, d.diocese_latin_name,
        d.archidiocese, d.sufragean_id,
        d.nowadays, d.infidelibus, d.disappeared, d.titular_see,
        d.vatican, d.url_hierarchy,
        ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
        p.country,
        --- aquí empieza lo q está enla tabla de las rentas
        r.fundacion, r.parishes, r.mesa_media, r.tasa_media,
        r.essantasede, r.n_mesas, r.n_tasas, r.metropolitana, r.place,
        r.mesa_estimado, r.tasa_estimado,
        r.mesa_estimada_bool, r.tasa_estimada_bool,
        -- aquí empiezan campos calculados: 1. si hay o no OPs, 2. cuántos
        CASE
         WHEN d.diocese_id IN (SELECT DISTINCT diocese_id FROM
                              vistas.bishops_individuals_edm_op b)
              THEN 'conops'
         ELSE 'sinops'
        END AS oppresence,
        (SELECT COUNT(*) FROM vistas.bishops_individuals_edm_op b
         WHERE b.diocese_id = d.diocese_id) AS totalops
 FROM general.dioceses d JOIN places P USING (place_id)
 LEFT OUTER JOIN dioceses_global_fl_estimado r USING (diocese_id))
SELECT j.* FROM j
WHERE country IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey');



--- esta es la q dura mucho por elcount ese
EXPLAIN ANALYZE
WITH j AS
(SELECT d.diocese_id, d.diocese_name, d.diocese_latin_name,
        d.archidiocese, d.sufragean_id,
        d.nowadays, d.infidelibus, d.disappeared, d.titular_see,
        d.vatican, d.url_hierarchy,
        ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
        p.country,
        --- aquí empieza lo q está enla tabla de las rentas
        r.fundacion, r.parishes, r.mesa_media, r.tasa_media,
        r.essantasede, r.n_mesas, r.n_tasas, r.metropolitana, r.place,
        r.mesa_estimado, r.tasa_estimado,
        r.mesa_estimada_bool, r.tasa_estimada_bool,
        -- aquí empiezan campos calculados: 1. si hay o no OPs, 2. cuántos
        CASE
         WHEN d.diocese_id IN (SELECT DISTINCT diocese_id FROM
                              vistas.bishops_individuals_edm_op b)
              THEN 'conops'
         ELSE 'sinops'
        END AS oppresence,
        (SELECT COUNT(diocese_id) FROM vistas.bishops_individuals_edm_op b
         WHERE b.diocese_id = d.diocese_id) AS totalops
 FROM general.dioceses d JOIN places P USING (place_id)
 LEFT OUTER JOIN dioceses_global_fl_estimado r USING (diocese_id))
SELECT j.* FROM j;

--- quitando el count de la subquery
EXPLAIN ANALYZE
WITH K AS
        (SELECT diocese_id, COUNT(diocese_id) totalops FROM vistas.bishops_individuals_edm_op b
         GROUP BY diocese_id),
j AS
(SELECT d.diocese_id, d.diocese_name, d.diocese_latin_name,
        d.archidiocese, d.sufragean_id,
        d.nowadays, d.infidelibus, d.disappeared, d.titular_see,
        d.vatican, d.url_hierarchy,
        ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
        p.country,
        --- aquí empieza lo q está enla tabla de las rentas
        r.fundacion, r.parishes, r.mesa_media, r.tasa_media,
        r.essantasede, r.n_mesas, r.n_tasas, r.metropolitana, r.place,
        r.mesa_estimado, r.tasa_estimado,
        r.mesa_estimada_bool, r.tasa_estimada_bool,
        -- aquí empiezan campos calculados: 1. si hay o no OPs, 2. cuántos
        CASE
         WHEN d.diocese_id IN (SELECT DISTINCT diocese_id FROM
                              vistas.bishops_individuals_edm_op b)
              THEN 'conops'
         ELSE 'sinops'
        END AS oppresence,
        totalops
 FROM general.dioceses d JOIN places P USING (place_id)
 LEFT OUTER JOIN dioceses_global_fl_estimado r USING (diocese_id)
 JOIN K USING (diocese_id))
SELECT j.* FROM j;


-- sin explain
WITH K AS
        (SELECT diocese_id, COUNT(diocese_id) totalops FROM vistas.bishops_individuals_edm_op b
         GROUP BY diocese_id),
j AS
(SELECT d.diocese_id, d.diocese_name, d.diocese_latin_name,
        d.archidiocese, d.sufragean_id,
        d.nowadays, d.infidelibus, d.disappeared, d.titular_see,
        d.vatican, d.url_hierarchy,
        ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
        p.country,
        --- aquí empieza lo q está enla tabla de las rentas
        r.fundacion, r.parishes, r.mesa_media, r.tasa_media,
        r.essantasede, r.n_mesas, r.n_tasas, r.metropolitana, r.place,
        r.mesa_estimado, r.tasa_estimado,
        r.mesa_estimada_bool, r.tasa_estimada_bool,
        -- aquí empiezan campos calculados: 1. si hay o no OPs, 2. cuántos
        CASE
         WHEN d.diocese_id IN (SELECT DISTINCT diocese_id FROM
                              vistas.bishops_individuals_edm_op b)
              THEN 'conops'
         ELSE 'sinops'
        END AS oppresence,
        totalops
 FROM general.dioceses d JOIN places P USING (place_id)
 LEFT OUTER JOIN dioceses_global_fl_estimado r USING (diocese_id)
 JOIN K USING (diocese_id))
SELECT j.* FROM j;

--- hacer una tabla q saque porcentajes de
--- OPs con respecto a frailes y con respecto a total de obispos
-- EXPLAIN analyze
DROP VIEW IF EXISTS qgis.porcentajes_ops;
CREATE VIEW qgis.porcentajes_ops AS
WITH j AS
(SELECT diocese_id, COUNT(*) AS totalobispos,
       COUNT(*) FILTER  (WHERE religious_order IS NOT NULL) AS nonsecular,
       COUNT(*) FILTER  (WHERE religious_order = 'O.P.') AS ops
FROM vistas.b_edm_cs_sa
GROUP BY 1)
SELECT diocese_id, diocese_name,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
       totalobispos, nonsecular, ops,
       nonsecular::REAL / totalobispos::REAL AS porcentajefrailes,
       ops::REAL / totalobispos::REAL AS porcentajeopscontotales,
       ops::REAL / NULLIF(nonsecular::REAL,0) AS porcentajeopsconfrailes,
       p.country,
       ROW_NUMBER() OVER() AS myid -- esto hace falta tvz por qgis
FROM j
LEFT JOIN dioceses USING (diocese_id)
LEFT JOIN places P USING (place_id);


SELECT b.*, dg.tasa_media
FROM vistas.bishops_individuals_edm_op b
join bishops.dioceses_global_fl_estimado dg USING (diocese_id);

--- décadas
SELECT extract(year from decada), count(*)
FROM (
     SELECT date_nomination
         , date_trunc('decade', date_nomination)::date as decada
     FROM vistas.bishops_individuals_edm_op b
     WHERE date_nomination < '1800-01-01') as bishops_with_decades
GROUP BY decada
ORDER BY decada;


--- comprobando q funciona
SELECT *
FROM vistas.bishops_individuals_edm_op
WHERE date_nomination > '1600-01-01' AND
      date_nomination < '1609-12-31'
ORDER BY date_nomination;

--- lo mismo pero con el dato del país
SELECT extract(year from decada), country, count(*)
FROM (
     SELECT date_nomination
         , date_trunc('decade', date_nomination)::date as decada
         , country
     FROM vistas.bishops_individuals_edm_op b
     JOIN dioceses d USING (diocese_id)
     JOIN places P USING (place_id)
     WHERE date_nomination < '1800-01-01') as bishops_with_decades
GROUP BY decada, country
ORDER BY decada;


WITH series AS
      (SELECT generate_series(1500, 1800, 20) AS r_from),
      range AS (
      SELECT r_from, (r_from + 19) AS r_to FROM series)
SELECT r_from, r_to,
       (SELECT count(*) FROM vistas.bishops_individuals_edm_op b WHERE extract(year from date_nomination) BETWEEN r_from AND r_to) as total
FROM RANGE;


WITH series AS
      (SELECT generate_series(1500, 1800, 20) AS r_from),
      range AS (
      SELECT r_from, (r_from + 19) AS r_to FROM series)
SELECT r_from, r_to,
       (SELECT count(*) FROM bishops WHERE extract(year from date_nomination) >= r_from AND
       extract(year from date_nomination) <= r_to) as total
FROM RANGE;


DROP VIEW IF EXISTS cojones2;
CREATE TEMP VIEW cojones2 AS
WITH series AS
      (SELECT generate_series(1500, 1800, 10) AS r_from),
      rango AS (
      SELECT r_from, (r_from + 9) AS r_to FROM series)
SELECT r_from, r_to, p.country, COUNT(date_nomination) AS total
FROM rango
JOIN vistas.bishops_individuals_edm_op b
     ON  extract(year from date_nomination) BETWEEN r_from AND r_to
JOIN dioceses USING (diocese_id)
JOIN places P USING (place_id)
GROUP BY 1, 2, 3
ORDER BY r_from;

SELECT * FROM cojones2
WHERE country = 'Italy';


SELECT * FROM cojones2
WHERE country = 'Spain';

---esto NO funciona!!
CREATE OR REPLACE TEMP VIEW cojones3 AS
WITH series AS
      (SELECT generate_series(1500, 1800, 10) AS r_from)
SELECT s.r_from, r_to, country, COUNT(*) AS total
FROM series s
CROSS JOIN cojones2
GROUP BY 1, 2, 3
ORDER BY s.r_from;

SELECT r_from, country
     , COALESCE(t.total, 0) AS eintraege
FROM  (
   SELECT r_from, country, total
   FROM   cojones2
   ) t
RIGHT JOIN (
      SELECT generate_series(1500, 1800, 10) AS r_from
   ) decadas USING (r_from)
ORDER  BY r_from;

--- papas
SELECT p.name_pope, p.date_nomination,
       p.date_death - p.date_nomination AS duration,
       round((p.date_death - p.date_nomination)::NUMERIC / 365, 2) AS anyos,
       COUNT(*) AS total,
       COUNT(*) / ((p.date_death - p.date_nomination)::NUMERIC / 365)
FROM popes P
JOIN vistas.bishops_individuals_edm_op b
     ON  b.date_nomination > p.date_nomination
         AND b.date_nomination < p.date_death
GROUP BY 1,2,3
ORDER BY p.date_nomination ASC;

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


--- miramos is los que pasan a una 2ªestán cuánto tiempo en la primera y
--- si hay diferencias.
SELECT url, anos, 'morethanone' AS moredioceses
FROM vistas.bishops_individuals_edm_op
WHERE ordinal = 1
      AND url IN (SELECT DISTINCT url
FROM vistas.bishops_individuals_edm_op
WHERE ordinal = 2)
      AND anos IS NOT NULL
UNION ALL
SELECT url, anos, 'onlyone' AS moredioceses
FROM vistas.bishops_individuals_edm_op
WHERE ordinal = 1
      AND url NOT IN (SELECT DISTINCT url
FROM vistas.bishops_individuals_edm_op
WHERE ordinal = 2)
      AND anos IS NOT NULL;

CREATE TEMP VIEW moredioceses as
SELECT url, anos, 'morethanone' AS moredioceses
FROM vistas.bishops_individuals_edm_op
WHERE ordinal = 1
      AND url IN (SELECT DISTINCT url
FROM vistas.bishops_individuals_edm_op
WHERE ordinal = 2)
      AND anos IS NOT NULL
UNION ALL
SELECT url, anos, 'onlyone' AS moredioceses
FROM vistas.bishops_individuals_edm_op
WHERE ordinal = 1
      AND url NOT IN (SELECT DISTINCT url
FROM vistas.bishops_individuals_edm_op
WHERE ordinal = 2)
      AND anos IS NOT NULL;

SELECT COUNT(*) FILTER (WHERE anos < 2 AND moredioceses = 'onlyone')
                AS onlyone,
       COUNT(*) FILTER (WHERE anos < 2 AND moredioceses = 'morethanone')
                AS morethanone
FROM moredioceses;

--- porcentajes, etc .en américa en bruto
SELECT diocese_id, diocese_name,
       totalobispos, nonsecular, ops,
       porcentajefrailes, porcentajeopscontotales,
       porcentajeopsconfrailes
FROM qgis.porcentajes_ops
WHERE country IN
       ('Argentina', 'Bolivia', 'Chile', 'Colombia',
               'Cuba', 'Ecuador', 'Dominican Republic', 'Ecuador',
               'Guatemala', 'Honduras', 'Mexico', 'Nicaragua', 'Panamá',
               'Paraguay', 'Peru', 'Philippines', 'Puerto Rico',
               'Venezuela');

--- porcentajes, etc .en américa sumando
WITH j AS
(SELECT diocese_id, diocese_name,
       totalobispos, nonsecular, ops,
       porcentajefrailes, porcentajeopscontotales,
       porcentajeopsconfrailes
FROM qgis.porcentajes_ops
WHERE country IN
       ('Argentina', 'Bolivia', 'Chile', 'Colombia',
               'Cuba', 'Ecuador', 'Dominican Republic', 'Ecuador',
               'Guatemala', 'Honduras', 'Mexico', 'Nicaragua', 'Panamá',
               'Paraguay', 'Peru', 'Philippines', 'Puerto Rico',
               'Venezuela'))
SELECT SUM(totalobispos) AS total,
       SUM(nonsecular) AS secular,
       SUM(ops) AS ops
FROM j;


---- periplos de los OPs

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
       ARRAY_AGG(diocese_name ORDER BY date_nomination) AS sucesion,
       ARRAY_LENGTH(ARRAY_AGG(diocese_name ORDER BY date_nomination), 1) AS cantidad
FROM diocconcretas
JOIN general.dioceses USING(diocese_id)
GROUP BY url
ORDER BY ARRAY_LENGTH(ARRAY_AGG(diocese_name ORDER BY date_nomination), 1) DESC;


SELECT EXTRACT(century FROM date_nomination), COUNT(*)
FROM vistas.bishops_individuals_edm_op b
GROUP BY 1;


WITH j AS
(SELECT EXTRACT(century FROM date_nomination) AS siglo,
        COUNT(*) AS totalnombramientos
FROM vistas.bishops_individuals_edm_op b
GROUP BY 1)
SELECT EXTRACT(century FROM date_nomination) AS siglo,
       j.totalnombramientos,
       COUNT(*) AS total,
       round(COUNT(*) * 100 / j.totalnombramientos::NUMERIC,2 ) AS porcentaje
FROM vistas.bishops_individuals_edm_op b
JOIN j ON EXTRACT(century FROM date_nomination) =  j.siglo
WHERE ordinal > 1
GROUP BY 1, 2;


CREATE OR REPLACE FUNCTION harmean_accum(float8[], float8)
RETURNS float8[]
LANGUAGE sql
AS $h$
SELECT array[$1[1]+1.0/$2, $1[2]+1.0];
$h$;

CREATE OR REPLACE FUNCTION harmean_finalize(float8[])
RETURNS float8
LANGUAGE sql
AS $h$
SELECT $1[2] /  $1[1];
$h$;




cREATE AGGREGATE harmean(float8) (
    sfunc = harmean_accum,
    stype = float8[],
    finalfunc = harmean_finalize,
    INITCOND = '{0.0, 0.0}'
);




SELECT date_nomination - LAG(date_end, 1) OVER (diocese_id)
FROM vistas.bishops_individuals_edm_op;

WITH j AS
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
ORDER BY diocese_name;

CREATE OR REPLACE FUNCTION geomean_accum(float8[], float8)
RETURNS float8[]
LANGUAGE sql
AS $g$
SELECT array[$1[1]+ln($2), $1[2]+1.0];
$G$;


CREATE OR REPLACE FUNCTION geomean_finalize(float8[])
RETURNS float8
LANGUAGE sql
AS $g$
SELECT exp($1[1] /  $1[2]);
$G$;

CREATE AGGREGATE geomean(float8) (
    sfunc = geomean_accum,
    stype = float8[],
    finalfunc = geomean_finalize,
    initcond = '{0.0, 0.0}'
);


WITH j as (
SELECT 23.3 AS dato
UNION
SELECT 5.71 AS dato
UNION
SELECT 0.0849 AS dato
UNION
SELECT 0.903 AS dato)
SELECT geomean(dato) FROM j;


WITH j AS
(SELECT diocese_id, diocese_name,
        date_nomination,
        ST_SetSRID(ST_MakePoint(longitude, latitude),4326) AS coord,
        LAG(date_end, 1) OVER (PARTITION BY diocese_id  ORDER BY date_nomination) AS previa,
        date_nomination - LAG(date_end, 1) OVER (PARTITION BY diocese_id  ORDER BY date_nomination) AS diferencia
FROM vistas.bishops_individuals_edm_op),
resultado AS
(SELECT diocese_id, diocese_name,
        coord,
       -- date_nomination, previa,
       diferencia / 365.25 AS anyos,
       geomean(diferencia / 365.25) OVER (PARTITION BY diocese_id) AS geommean
FROM j
WHERE diferencia > 0
GROUP BY 1, 2, 3, 4)
SELECT DISTINCT diocese_id, diocese_name, coord, geommean,
       1 / geommean
FROM resultado
ORDER BY 2;


WITH j AS
(SELECT diocese_id, diocese_name,
        date_nomination,
        st_transform(ST_SetSRID(ST_MakePoint(longitude, latitude),4326), 54030) AS geom,
        LAG(date_end, 1) OVER (PARTITION BY diocese_id  ORDER BY date_nomination) AS previa,
        date_nomination - LAG(date_end, 1) OVER (PARTITION BY diocese_id  ORDER BY date_nomination) AS diferencia
FROM vistas.bishops_individuals_edm_op
WHERE diocese_id IN (
      SELECT diocese_id
      FROM vistas.bishops_individuals_edm_op
      GROUP BY 1
      HAVING COUNT(*) > 2
      )
),
k AS
(SELECT diocese_id, diocese_name,
        geom,
       -- date_nomination, previa,
       diferencia / 365.25 AS anyos,
       geomean(diferencia / 365.25) OVER (PARTITION BY diocese_id) AS geommean,
       1 / geomean(diferencia / 365.25) OVER (PARTITION BY diocese_id) AS geommeaninvertido
FROM j
WHERE diferencia > 0
GROUP BY 1, 2, 3, 4),
resultado AS
(SELECT DISTINCT diocese_id,
       diocese_name, geom, geommean,
       geommeaninvertido
FROM K)
SELECT *,
       ROW_NUMBER() OVER() AS myid -- esto hace falta tvz por qgis
FROM resultado
ORDER BY 2;


--- creando una view temporal
DROP VIEW IF EXISTS cojones1;
CREATE OR REPLACE TEMP VIEW cojones1 AS
WITH j AS
(SELECT diocese_id, diocese_name,
        date_nomination,
        st_transform(ST_SetSRID(ST_MakePoint(longitude, latitude),4326), 54030) AS geom,
        LAG(date_end, 1) OVER (PARTITION BY diocese_id  ORDER BY date_nomination) AS previa,
        date_nomination - LAG(date_end, 1) OVER (PARTITION BY diocese_id  ORDER BY date_nomination) AS diferencia
FROM vistas.bishops_individuals_edm_op
WHERE diocese_id IN (
      SELECT diocese_id
      FROM vistas.bishops_individuals_edm_op
      GROUP BY 1
      HAVING COUNT(*) > 2
      )
)
SELECT diocese_id, diocese_name,
        geom,
       date_nomination, previa,
       diferencia / 365.25 AS anyos,
       geomean(diferencia / 365.25) OVER (PARTITION BY diocese_id) AS geommean,
       1 / geomean(diferencia / 365.25) OVER (PARTITION BY diocese_id) AS geommeaninvertido
FROM j
WHERE diferencia > 0
GROUP BY 1, 2, 3, 4, 5, 6;

WITH j AS
(SELECT b.diocese_name, p.country, b.longitude, b.latitude,
       COUNT(*) AS total
FROM vistas.bishops_individuals_edm_op b
JOIN dioceses d USING (diocese_id)
LEFT JOIN places P USING (place_id)
WHERE b.longitude IS NOT null
GROUP BY 1,2,3,4
ORDER BY COUNT(*) DESC)
SELECT country, total, COUNT(*)
FROM j
GROUP BY 1, 2
ORDER BY 1, 2;

SELECT url, bishop_fullname, anos, diocese_name
FROM vistas.bishops_individuals_edm_op b
WHERE anos IS NOT null
ORDER BY anos;


--- una aray para hacer una tabla en la q aparecen
--- una lista con las q más
WITH j AS
(SELECT diocese_id, COUNT(*) AS total
FROM vistas.bishops_individuals_edm_op b
GROUP BY diocese_id
HAVING COUNT(*) > 3)
SELECT total, ARRAY_AGG(DISTINCT b.diocese_name || '( ' || p.country || ')')
FROM j
JOIN vistas.bishops_individuals_edm_op b USING (diocese_id)
JOIN dioceses d USING (diocese_id)
JOIN places P USING (place_id)
GROUP BY 1;

SELECT COUNT(DISTINCT diocese_id)
FROM vistas.bishops_individuals_edm_op b;

SELECT COUNT(DISTINCT url)
FROM vistas.bishops_individuals_edm_op b;


SELECT COUNT(DISTINCT url)
       FILTER (WHERE religious_order IS NULL),
       COUNT(DISTINCT url)
       FILTER (WHERE religious_order IS NOT NULL)
FROM b_edm_cs_sa;

WITH j AS
(SELECT religious_order, COUNT(*) AS total
FROM b_edm_cs_sa
WHERE religious_order IS NOT NULL
GROUP BY 1)
SELECT religious_order, total,
       total::REAL / SUM(total::REAL) OVER () AS porcentaje
FROM j
GROUP BY 1, 2
ORDER BY total DESC;

--- fundaciones de órdenes religisoas
WITH j AS (
     SELECT r.order_id,
            CASE
                WHEN r.year_foundation = '4th Century' THEN '350'
                WHEN r.year_foundation = '6th Century' THEN '550'
                WHEN r.year_foundation = '11th Century' THEN '1050'
                WHEN r.year_foundation = '14th Century' THEN '1350'
                ELSE r.year_foundation
             END AS fundacionorder
     FROM general.religious_orders r)
SELECT COUNT(*) FILTER (WHERE fundacionorder::int < 1800) AS anteriores,
       COUNT(*) FILTER (WHERE fundacionorder::int > 1800) AS posteriores,
       COUNT(*) FILTER (WHERE fundacionorder::int > 1500 AND fundacionorder::int < 1800) AS edm
FROM j;

--- las de la edm
WITH j AS (
     SELECT r.*,
            CASE
                WHEN r.year_foundation = '4th Century' THEN '350'
                WHEN r.year_foundation = '6th Century' THEN '550'
                WHEN r.year_foundation = '11th Century' THEN '1050'
                WHEN r.year_foundation = '14th Century' THEN '1350'
                ELSE r.year_foundation
             END AS fundacionorder
     FROM general.religious_orders r)
SELECT j.*
FROM j
WHERE fundacionorder::int > 1500 AND fundacionorder::int < 1800
ORDER BY fundacionorder::INT DESC;

SELECT *
FROM b_edm_cs_sa
WHERE date_nomination > '1800-01-01';

SELECT *
FROM b_edm_cs_sa
WHERE religious_order = 'S.A.';


CREATE OR REPLACE VIEW cojones3
    AS
  SELECT b.*, r.order_id, r.order_acronym, r.order_name_english, r.order_type
  FROM b_bishops_reducido b
  LEFT JOIN religious_orders r ON b.religious_order_id = r.order_id
  WHERE affiliated = FALSE and date_nomination > '1500-01-01' AND
        date_nomination < '1800-01-01';

SELECT *
FROM cojones3
WHERE date_nomination > '1800-01-01';

SELECT * FROM cojones3
WHERE bishop_all_id NOT IN
   (SELECT bishop_all_id FROM b_edm_cs_sa);



CREATE OR replace VIEW cojones4 AS
WITH concretos AS
  (SELECT url,
          bishop_surname || bishop_name AS bishop_fullname,
          diocese_id, date_nomination, date_end,
          order_id, order_acronym, order_name_english, order_type,
          reason_begin, reason_end
  FROM cojones3
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

--- otro
WITH j AS
(SELECT religious_order, COUNT(*) AS total
FROM cojones3
WHERE religious_order IS NOT NULL
GROUP BY 1)
SELECT religious_order, total,
       round((total::numeric / SUM(total::numeric) OVER ()) *100, 2) AS porcentaje
FROM j
GROUP BY 1, 2
ORDER BY total DESC;


SELECT COUNT(DISTINCT url)
       FILTER (WHERE religious_order IS NULL),
       COUNT(DISTINCT url)
       FILTER (WHERE religious_order IS NOT NULL)
FROM b_edm_cs_sa;


SELECT COUNT(DISTINCT diocese_id)
FROM b_edm_cs_sa;

SELECT COUNT(DISTINCT diocese_id)
FROM vistas.bishops_individuals_edm_op b;


SELECT CASE
       WHEN religious_order IS NOT NULL THEN 'Non-secular'
       WHEN religious_order IS NULL THEN 'Secular'
       END AS bishoptype,
       COUNT(*) AS total
FROM vistas.b_edm_cs_sa
GROUP BY bishoptype;


SELECT p.name_pope, p.date_nomination,
       p.date_death - p.date_nomination AS duration,
       round((p.date_death - p.date_nomination)::NUMERIC / 365, 2) AS anyos,
       COUNT(*) AS total,
       COUNT(*) / ((p.date_death - p.date_nomination)::NUMERIC / 365) as mediaanual
FROM popes P
JOIN vistas.bishops_individuals_edm_op b
     ON  b.date_nomination > p.date_nomination
         AND b.date_nomination < p.date_death
GROUP BY 1,2,3
ORDER BY COUNT(*) ASC;
--- mirando eso de por qué hay esa bajada
--- a finales del xvi
SELECT EXTRACT(YEAR FROM date_end) AS anyo,
       COUNT(*)
FROM vistas.bishops_individuals_edm_op b
WHERE reason_end = 'Died'
GROUP BY 1
ORDER BY anyo;


WITH series AS
      (SELECT generate_series(1500, 1800, 10) AS r_from),
      rango AS (
      SELECT r_from, (r_from + 9) AS r_to FROM series)
SELECT r_from, r_to, COUNT(date_end) AS total
FROM rango
JOIN vistas.bishops_individuals_edm_op b
     ON  extract(year from date_end) BETWEEN r_from AND r_to
WHERE b.reason_end = 'Died'
GROUP BY 1, 2
ORDER BY r_from;

-- sumar totalse por dićesis sin más
DROP VIEW IF EXISTS qgis.anyostotales;
CREATE OR REPLACE VIEW qgis.anyostotales AS
SELECT diocese_id, diocese_name,
       st_transform(ST_SetSRID(ST_MakePoint(longitude, latitude),4326), 54030) AS geom,
       SUM(anos) AS total,
       SUM(anos) / 300 AS porcentaje,
       ROW_NUMBER() OVER() AS myid -- esto hace falta tvz por qgis
FROM vistas.bishops_individuals_edm_op b
group BY 1,2,3
ORDER BY SUM(anos);

SELECT diocese_name, total, porcentaje
FROM qgis.anyostotales
ORDER BY porcentaje;

SELECT COUNT(*)
FROM vistas.bishops_individuals_edm_op b
WHERE anos < 2;
