--- hay que usar una CTE pq no se puede usar
--- uuna función agregada luego en el group by
WITH coords AS
(SELECT diocesis_a AS diocese_name,
       ST_SetSRID(ST_MakePoint(long_a, lat_a),4326) AS coord
FROM vistas.related_dioceses_edm_ops
)
SELECT diocese_name, coord,
       COUNT(*) AS total
FROM coords
GROUP BY 1,2
ORDER BY total DESC;
