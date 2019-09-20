-- esto es emd 

SELECT religious_order, diocese_id,
       d.diocese_name, p.country,
       p.longitude, p.latitude,
       COUNT(*) AS total 
FROM vistas.b_emd_ss_sa
JOIN dioceses d USING(diocese_id)
LEFT JOIN places P USING(place_id)
GROUP BY religious_order, diocese_id, d.diocese_name,
      p.country, p.longitude, p.latitude
ORDER BY d.diocese_name;
