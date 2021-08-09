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
SELECT j.diocese_id, j.diocese_name, p.country,
       -- geom, -- R no lo reconoce
       j.date_nomination, previa,
       diferencia / 365.25 AS anyos,
       geomean(diferencia / 365.25) OVER (PARTITION BY diocese_id) AS geommean,
       1 / geomean(diferencia / 365.25) OVER (PARTITION BY diocese_id) AS geommeaninvertido
FROM j
JOIN dioceses d USING (diocese_id)
JOIN places P USING (place_id)
WHERE diferencia > 0
GROUP BY 1, 2, 3, 4, 5, 6;
