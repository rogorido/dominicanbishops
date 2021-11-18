WITH j AS
(SELECT d.*,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord, p.country
FROM general.dioceses d
JOIN places P USING (place_id)
WHERE country IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey'))
SELECT j.*
FROM j
WHERE j.diocese_id NOT IN
      (SELECT diocese_id FROM vistas.bishops_individuals_edm_op b)
ORDER BY diocese_name;
