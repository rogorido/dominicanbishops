-- por años en las diócesis y por ordnes
-- está en f2aa1fa5-43bd-4030-8432-cc145168216d

SELECT DISTINCT d.diocese_id, d.diocese_name,
       b.religious_order, b.religious_order_id,
       p.country AS pais, 
       p.longitude, p.latitude,
       COALESCE(
                SUM(EXTRACT(year from b.date_end)
                                 -
                    EXTRACT(year from b.date_nomination)), 0) AS duracion
FROM  dioceses d
     JOIN b_edm_ss_sa b ON d.diocese_id = b.diocese_id
     LEFT OUTER JOIN places p ON p.place_id = d.place_id
  WHERE b.date_nomination > '1560-01-01' and b.date_nomination < '1700-01-01'
        AND 'Europe' = ANY (macroregions) 
GROUP BY d.diocese_id, diocese_name,
      b.religious_order, b.religious_order_id, pais,
      p.longitude, p.latitude
ORDER BY duracion;
