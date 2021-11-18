--- aggreagage in dioceses 

SELECT url, bishop_fullname, COUNT(*) AS total 
FROM vistas.bishops_individuals_cnf_op
GROUP BY url, bishop_fullname;
