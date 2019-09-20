-- Diócesis sin órdenes religiosas

WITH a AS (
     SELECT diocese_id, COUNT(ORDER_id) AS total
     FROM vistas.b_emd_cs_sa
     GROUP BY diocese_id)
SELECT DISTINCT diocese_id, d.diocese_name,
                p.longitude, p.latitude
FROM a
  JOIN dioceses d USING(diocese_id)
  JOIN places P USING (place_id)
WHERE total = 0;
