
WITH series AS
      (SELECT generate_series(1500, 1800, 10) AS r_from),
      rango AS (
      SELECT r_from, (r_from + 9) AS r_to FROM series)
SELECT r_from, r_to, COUNT(date_end) AS total
FROM rango
JOIN vistas.bishops_individuals_edm_op b
     ON  extract(year from date_end) BETWEEN r_from AND r_to
WHERE b.reason_end = 'Died'
GROUP BY 1, 2
ORDER BY r_from;
