--- aggreagage in dioceses 

SELECT url, bishop_fullname, COUNT(*) AS total 
FROM vistas.bishops_individuals_edm_op
GROUP BY url, bishop_fullname;
