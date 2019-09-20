-- Esto se reduce a la edm!
-- Se trata de b0dc901e-af8c-497b-9ee3-ae47c5796d47
-- Ie: diócesis en las que le papa X no ha intervenido. 

CREATE OR REPLACE TEMP VIEW tv_diocesissinpapas_edm AS
  --- CTE1 
WITH papasdioc AS 
(SELECT DISTINCT 
       diocese_id, 
       p.pope_id, p.name_pope, p.date_nomination, p.date_death
 FROM b_edm_cs_sa b
 JOIN popes P ON b.date_nomination BETWEEN p.date_nomination AND p.date_death),

  --- CT2
cruzada AS (
     SELECT pope_id, pp.date_death,
            diocese_id, (other_data->'gcatholic'->>'foundation')::integer AS fundacion
     FROM dioceses d, popes pp
     WHERE pp.date_death > '1500-01-01'),

  --- CT3 
diocsinrefapapa AS (
     SELECT c.pope_id, c.date_death,
            c.diocese_id, c.fundacion
     FROM cruzada c
     WHERE NOT EXISTS (SELECT 1, 2 FROM papasdioc pd
                       WHERE pd.diocese_id = c.diocese_id AND pd.pope_id = c.pope_id ))

SELECT pope_id, date_death, diocese_id, fundacion
FROM diocsinrefapapa
WHERE fundacion < EXTRACT(YEAR FROM date_death);
