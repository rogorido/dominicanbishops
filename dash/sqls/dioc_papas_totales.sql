-- Todo esto está restringido ahora mismo a la Edad Moderna.
-- se trata de ec8e9fe7-6a14-4f1d-ae96-652ab6c0bef0 en analisisdatos.org

WITH conexiones AS (
SELECT bishop_all_id, 
       diocese_id, 
       p.pope_id, p.name_pope, p.date_nomination, p.date_death
FROM b_edm_cs_sa b 
JOIN popes P ON b.date_nomination BETWEEN p.date_nomination AND p.date_death)

SELECT c.pope_id, c.name_pope || ' (' ||
       extract(year from c.date_nomination) || '-' || extract(year from c.date_death) || ')' as pope_name,
       c.date_nomination, c.date_death,
       d.diocese_name, p.longitude, p.latitude, 
       COUNT(diocese_id) as total 
FROM conexiones C
JOIN dioceses d USING(diocese_id)
JOIN places P USING(place_id)
GROUP BY 1, 2, 3, 4, 5, 6, 7
ORDER BY c.date_nomination, d.diocese_name;
