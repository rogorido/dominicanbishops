-- Diócesis donde no ha habido regulares
-- crea una temp view q usamos en las otras 

CREATE OR REPLACE TEMP VIEW tv_diocesissinfrailes_edm AS

WITH diocconfrailes AS 
(SELECT DISTINCT 
       diocese_id, order_id
 FROM b_edm_ss_sa b), 

cruzada AS (
     SELECT r.order_id,
            CASE
                WHEN r.year_foundation = '4th Century' THEN '350'
                WHEN r.year_foundation = '6th Century' THEN '550'
                WHEN r.year_foundation = '11th Century' THEN '1050'
                WHEN r.year_foundation = '13th Century' THEN '1250'
                WHEN r.year_foundation = '14th Century' THEN '1350'
                ELSE r.year_foundation
             END AS fundacionorder,
            diocese_id, (other_data->'gcatholic'->>'foundation')::integer AS fundaciondioc 
     FROM dioceses d, religious_orders r),

diocsinfrailes AS (
     SELECT order_id, fundacionorder, diocese_id, fundaciondioc 
     FROM cruzada c
     WHERE NOT EXISTS (SELECT 1, 2 FROM diocconfrailes df
                       WHERE df.diocese_id = c.diocese_id AND df.order_id = c.order_id ))

SELECT order_id, fundacionorder, diocese_id, fundaciondioc
FROM diocsinfrailes;
--WHERE fundacionorder::integer > fundaciondioc;
