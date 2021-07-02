
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

CREATE OR replace VIEW vistas.bishops_individuals_edm_op AS 
WITH concretos AS
  (SELECT url,
          bishop_surname || bishop_name AS bishop_fullname,
          diocese_id, date_nomination, date_end,
          order_id, order_acronym, order_name_english, order_type
  FROM vistas.b_edm_cs_sa
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

