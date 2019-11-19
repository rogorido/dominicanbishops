-- Cantidad de obispos por año y 10 órdenes más importantes (Edad Moderna!)
-- está en 4197a23b-e4d0-4b8f-847a-008ca5beb41e

WITH anyos AS (
      SELECT generate_series('1500-01-01'::TIMESTAMP,
                             '1800-01-01'::TIMESTAMP,
                             '1 year'::interval) AS serie )

SELECT order_acronym, serie, p.country, COUNT(*) AS totalobispos
FROM anyos, vistas.b_edm_ss_sa
JOIN dioceses USING (diocese_id)
LEFT JOIN places p USING (place_id)
WHERE EXTRACT(YEAR FROM serie) BETWEEN EXTRACT(YEAR FROM date_nomination) AND EXTRACT(YEAR FROM date_end)
GROUP BY order_acronym, anyos.serie, p.country
ORDER BY anyos.serie;
