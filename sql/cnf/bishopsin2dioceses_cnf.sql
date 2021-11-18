--- seleccionamos los q están en dos
--- y miramos done más están

SELECT b.*, p.country
FROM vistas.bishops_individuals_cnf_op b
JOIN 
 (SELECT url, COUNT(*) AS total 
  FROM vistas.bishops_individuals_cnf_op
  GROUP BY url
  HAVING COUNT(*)= 2) j USING (url)
JOIN dioceses d USING (diocese_id)
JOIN places P USING (place_id);
