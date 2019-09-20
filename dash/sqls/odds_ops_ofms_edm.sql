-- Creo aquí la consulta simple y luego lo del odds lo hago en R, porque
-- realmente pivoto la tabla, etc. y es más fácil en R. Esto es solo de
-- la Edad Moderna. Está en 3cda72bf-46d0-4dfa-bdcd-5115b79a1920

SELECT d.diocese_name, order_acronym, p.longitude, p.latitude, COUNT(*) AS total 
FROM vistas.b_edm_ss_sa_ofm
JOIN dioceses d USING(diocese_id)
LEFT JOIN places P USING(place_id)
WHERE order_acronym IN ('O.P.', 'O.F.M.')
GROUP BY order_acronym, d.diocese_name, p.longitude, p.latitude;
