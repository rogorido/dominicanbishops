--- solo con órdenes religiosas
--- está en 0fa56414-33e4-4758-9a02-d16a576d74c7

SELECT d.diocese_id, d.diocese_name,
       p.longitude, p.latitude,
       b.religious_order, b.religious_order_id,
       count(b.bishop_all_id) AS total  
FROM b_edm_ss_sa b
     JOIN dioceses d USING (diocese_id)
     LEFT JOIN places P USING (place_id)
WHERE b.date_nomination > '1560-01-01' and b.date_nomination < '1700-01-01'
      AND 'Europe' = ANY (macroregions)
GROUP BY d.diocese_id, diocese_name, p.longitude, p.latitude,
         b.religious_order, b.religious_order_id;
