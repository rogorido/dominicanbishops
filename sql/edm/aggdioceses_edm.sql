--- contando diócesis por su número de OPs

SELECT b.diocese_name, p.country, b.longitude, b.latitude,
       COUNT(*) AS total
FROM vistas.bishops_individuals_edm_op b
JOIN dioceses d USING (diocese_id)
LEFT JOIN places P USING (place_id)
WHERE b.longitude IS NOT null
GROUP BY 1,2,3,4
ORDER BY COUNT(*) DESC;
