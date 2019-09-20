-- Realmente salen muchas que no tienen mucho interés, por eso podemos
-- seleccionar las 10 órdenes religiosas más importantes

SELECT d.order_id, r.order_acronym, diocese_id, dd.diocese_name, p.country, 
       p.longitude, p.latitude 
FROM tv_diocesissinfrailes_emd d -- esto es temp view anterior! 
JOIN religious_orders r USING(order_id)
JOIN dioceses dd USING(diocese_id)
JOIN places p USING(place_id)
WHERE d.order_id IN (SELECT DISTINCT religious_order_id
                     FROM vistas.b_emd_ss_sa_top10orders)
ORDER BY r.order_acronym;
