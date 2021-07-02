--- contando diócesis por su número de OPs

SELECT b.diocese_name, p.country, b.longitude, b.latitude, count(*)
FROM vistas.bishops_individuals_cph_op b
JOIN dioceses d USING (diocese_id)
LEFT JOIN places P USING (place_id)
WHERE b.longitude IS NOT null
GROUP BY 1,2,3,4
ORDER BY COUNT(*) DESC;
