WITH anyos AS (
      SELECT generate_series(1500, 1799, 1) AS serie),
d AS
      (SELECT DISTINCT b.diocese_id FROM vistas.bishops_individuals_edm_op b
       JOIN dioceses USING (diocese_id)
       JOIN places P USING (place_id)
       WHERE country= $1 ) 
SELECT serie, d.diocese_id,
       dd.diocese_name, p.longitude, p.latitude,
       bishops_order_per_year_edm_per_dioceses(serie, d.diocese_id, 121) AS total
FROM anyos, d
JOIN dioceses dd USING (diocese_id)
JOIN places P USING (place_id);
