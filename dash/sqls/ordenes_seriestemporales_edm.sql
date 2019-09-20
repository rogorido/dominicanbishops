-- Cantidad de obispos por año y 10 órdenes más importantes (Edad Moderna!)
-- está en 4197a23b-e4d0-4b8f-847a-008ca5beb41e

WITH anyos AS (
      SELECT generate_series(1500, 1800, 1) AS serie )

SELECT order_acronym, serie, p.country, COUNT(*) AS totalobispos
-- FROM anyos, vistas.b_edm_ss_sa_top10orders
FROM anyos, vistas.b_edm_ss_sa
JOIN dioceses USING (diocese_id)
LEFT JOIN places p USING (place_id)
WHERE serie BETWEEN EXTRACT(YEAR FROM date_nomination) AND EXTRACT(YEAR FROM date_end)
GROUP BY order_acronym, anyos.serie, p.country
ORDER BY anyos.serie;
