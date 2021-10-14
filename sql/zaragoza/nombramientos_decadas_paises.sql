WITH series AS
      (SELECT generate_series(1500, 1800, 10) AS r_from),
      rango AS (
      SELECT r_from, (r_from + 9) AS r_to FROM series)
SELECT r_from, r_to, p.country, COUNT(date_nomination) AS total
FROM rango
JOIN vistas.bishops_individuals_edm_op b
     ON  extract(year from date_nomination) BETWEEN r_from AND r_to
JOIN dioceses USING (diocese_id)
JOIN places P USING (place_id)
GROUP BY 1, 2, 3
ORDER BY r_from;
