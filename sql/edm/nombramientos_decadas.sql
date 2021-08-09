--- décadas
SELECT extract(year from decada) AS decada,
       COUNT(*) AS total
FROM (
     SELECT date_nomination
         , date_trunc('decade', date_nomination)::date as decada
     FROM vistas.bishops_individuals_edm_op b
     WHERE date_nomination < '1800-01-01') as bishops_with_decades
GROUP BY decada
ORDER BY decada;
