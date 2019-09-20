-- Con esto podemos sacar donde son obispos y tienen monasterios:
-- está en 2dd6a614-e4fe-44eb-99ed-025af113c01b

WITH obispados AS (
     SELECT DISTINCT diocese_id, place_id
     FROM vistas.b_edm_ss_sa
     JOIN dioceses d USING(diocese_id)
     LEFT JOIN places P USING(place_id)
     WHERE religious_order = 'O.P.')

SELECT h.name AS convent,
       o.diocese_id, o.place_id,
       p.place, p.country,
       p.longitude, p.latitude
FROM houses h
JOIN obispados o USING(place_id)
JOIN places p USING(place_id);
