-- Diócesis que se terminan

WITH diocconfin as (
SELECT DISTINCT
       diocese_id, d.diocese_name,
       p.longitude, p.latitude, p.country,
       MAX(EXTRACT(YEAR FROM date_nomination)) OVER (PARTITION BY diocese_id) AS ultimanominacion,
       MAX(EXTRACT(YEAR FROM date_end)) OVER (PARTITION BY diocese_id) AS ultimofin
FROM b_edm_cs_sa
JOIN dioceses d USING(diocese_id)
LEFT JOIN places P USING(place_id)
ORDER BY ultimanominacion ASC)

SELECT ROW_NUMBER() OVER (PARTITION BY TRUE), diocconfin.*
FROM diocconfin;
