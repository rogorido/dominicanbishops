-- lista de todos los obispos 1200-1800 sin seculares
-- está en f2919d35-d45c-44f1-a6e5-d0d2dacd16d1

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
