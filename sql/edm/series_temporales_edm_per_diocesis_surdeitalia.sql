-- croe q un poco chapuza, pero para salir del paso

WITH anyos AS (
      SELECT generate_series(1500, 1799, 1) AS serie),
d AS
      (SELECT DISTINCT b.diocese_id FROM vistas.bishops_individuals_edm_op b
       JOIN italia USING (diocese_id)),
junto AS (
SELECT serie, d.diocese_id,
       bishops_order_per_year_edm_per_dioceses(serie, d.diocese_id, 121) AS total
FROM anyos, d)
SELECT serie, SUM(total) AS totalanyo
FROM junto
GROUP BY 1
ORDER BY serie;
