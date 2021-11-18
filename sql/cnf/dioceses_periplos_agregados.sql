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
