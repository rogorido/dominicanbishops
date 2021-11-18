--- tenemos q usar CTE por el puñetero ogr2ogr

-- ipt tb es que lo transformamos a robinson
-- que está metido ya en postgis.

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

