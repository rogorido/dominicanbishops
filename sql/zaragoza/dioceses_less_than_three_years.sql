WITH j AS
   (SELECT b.diocese_id, COUNT(*) AS totalabsoluto
   FROM bishops_individuals_edm_op b
   GROUP BY b.diocese_id),
K AS
   (SELECT b.diocese_id, COUNT(*) AS totalpoco
   FROM bishops_individuals_edm_op b
   WHERE anos < 3
   GROUP BY b.diocese_id)
SELECT d.diocese_id, d.diocese_name,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
       totalabsoluto, totalpoco,
       totalpoco::REAL / totalabsoluto::real AS porcentaje
FROM dioceses d
JOIN j USING (diocese_id)
JOIN K USING (diocese_id)
JOIN places P USING (place_id)
ORDER BY porcentaje DESC;
