-- Esto se reduce a la edm!
-- Se trata de b0dc901e-af8c-497b-9ee3-ae47c5796d47
-- Ie: diócesis en las que le papa X no ha intervenido.
-- Totales por papa. 

SELECT d.pope_id, p.name_pope || ' (' ||
       extract(year from p.date_nomination) || '-' || extract(year from p.date_death) || ')' AS nombre_papa,
       p.date_nomination, p.date_death,
       COUNT(diocese_id) as total 
FROM tv_diocesissinpapas_edm d
JOIN popes p USING(pope_id)
GROUP BY 1, 2, 3, 4
ORDER BY p.date_nomination ASC;
