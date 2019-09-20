-- periplos por diócesis.
-- está en: 891b2ec2-537f-4164-9987-aa00f7706643

CREATE OR replace temp VIEW tv_agregadodiocesis_emd_OPs AS

WITH obispostotales AS
-- escogemos solo los obispos que aparecen más de una vez 
     (SELECT url, COUNT(*) AS total 
      FROM vistas.b_emd_ss_sa
      WHERE religious_order = 'O.P.'
      GROUP BY url
      HAVING COUNT(url) > 1),

-- luego con lo anterior seleccionamos las diócesis y las fechas
     diocconcretas AS
     (SELECT url, diocese_id, date_nomination
     FROM vistas.b_emd_ss_sa
     JOIN obispostotales USING(url))

-- finalmente hacemos un agregado de las diócesis por las q pasan
SELECT url,
       ARRAY_AGG(diocese_name ORDER BY date_nomination) AS sucesion
FROM diocconcretas
JOIN dioceses USING(diocese_id)
GROUP BY url
ORDER BY url;
