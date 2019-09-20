-- Obispso seculares vs. no-seculares: totales y porcentajes por diócesis
-- está en 0b321b54-87aa-4b87-b489-ab189a97afcd

WITH conjunta AS (
       SELECT diocese_id, 'sinorder' AS orden, COUNT(*) AS total 
       FROM vistas.b_emd_cs_sa
       WHERE religious_order IS NULL
       GROUP BY diocese_id, religious_order
             UNION
       SELECT diocese_id, 'conorder' AS orden, COUNT(*) AS total 
       FROM vistas.b_emd_cs_sa
       WHERE religious_order IS NOT NULL
       GROUP BY diocese_id)

SELECT diocese_id, d.diocese_name, p.longitude, p.latitude,
       c.orden, total,
       total/sum(total) OVER (PARTITION BY d.diocese_name) as porcentaje
FROM dioceses d
JOIN conjunta c USING (diocese_id)	
JOIN places p ON p.place_id = d.place_id
ORDER BY d.diocese_name;
