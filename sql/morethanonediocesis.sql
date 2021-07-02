--- aggregate from view 

SELECT url, bishop_fullname, COUNT(*) AS total 
FROM vistas.periplo_cph_op -- view 
GROUP BY url, bishop_fullname;
