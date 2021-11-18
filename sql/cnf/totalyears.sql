SELECT b.diocese_name, p.country,
       b.longitude, b.latitude,
       SUM(anos) AS total
FROM vistas.bishops_individuals_cnf_op b
JOIN dioceses d USING (diocese_id)
LEFT JOIN places P USING (place_id)
GROUP BY 1,2,3,4;
