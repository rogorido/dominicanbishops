-- obispos por diócesis: 
-- Lo cogemos de Todos los obispos con seculares (1200-1800)
-- sin los afiliados. los ponemos por siglos. 

WITH general AS (
     SELECT diocese_id, extract(century from DATE_nomination) as siglo, COUNT(*) AS total
     FROM vistas.b_emd_cs_sa
     GROUP BY diocese_id, siglo)
SELECT DISTINCT diocese_id, siglo, d.diocese_name, total 
FROM general
  JOIN dioceses d USING(diocese_id);
