-- los tomamos solo de los detalles, ie, del libro
-- pq lo otro lo he metido de forma un poco desordenada. 

SELECT d.diocese_id, d.diocese_name,
       (dd.details->'saint_siege')::BOOLEAN AS santasede,
       longitude, latitude
FROM dioceses d
LEFT OUTER JOIN dioceses_details dd USING(diocese_id)
LEFT OUTER JOIN places USING(place_id)
WHERE (dd.details->'saint_siege')::BOOLEAN = TRUE;
