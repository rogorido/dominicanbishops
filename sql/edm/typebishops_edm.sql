SELECT CASE
       WHEN religious_order IS NOT NULL THEN 'Non-secular'
       WHEN religious_order IS NULL THEN 'Secular'
       END AS bishoptype,
       COUNT(*) AS total 
FROM vistas.b_edm_cs_sa
GROUP BY bishoptype;
