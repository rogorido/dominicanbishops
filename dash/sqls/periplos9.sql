--- Obispos en más de una diócesis: patrones temporales por décadas

WITH obispostotales AS
  -- escogemos solo los obispos que aparecen más de una vez 
       (SELECT url, COUNT(*) AS total 
               FROM vistas.b_emd_cs_sa
               GROUP BY url
               HAVING COUNT(url) > 1),
  -- luego con lo anterior las fechas
       diocconcretas AS
       (SELECT url, diocese_ID, date_nomination, date_end, order_id, order_acronym, order_name_english, order_type,
       ROW_NUMBER() OVER (PARTITION BY url ORDER BY date_nomination)
       FROM vistas.b_emd_cs_sa
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
      (SELECT generate_series(1200, 1800, 10) AS r_from),
      range AS (
      SELECT r_from, (r_from + 9) AS r_to FROM series)

SELECT r_from || '-' || r_to AS decada,
       (SELECT count(*) FROM todojunto WHERE extract(year from date_nomination) BETWEEN r_from AND r_to) as total
FROM range;

