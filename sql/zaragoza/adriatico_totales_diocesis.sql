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
