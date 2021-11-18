-- Cantidad de obispos por año

WITH anyos AS (
      SELECT generate_series(1500, 1799, 1) AS serie),
j AS (
  SELECT DISTINCT p.country 
  FROM dioceses
  JOIN places P USING(place_id)
)
SELECT serie, country,
       bishops_order_per_year_edm_with_country(121, serie, country) AS totalobispos
FROM anyos, j;
