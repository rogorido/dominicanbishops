--- ver explicaciones en analisisdatos.org
--- en 134fd81f-2078-44f8-8992-4c2d5637115e

SELECT dg.*,
       --- añadimos unacoluman de si hay OPs o no
       CASE WHEN
            (SELECT diocese_id
                    IN (SELECT DISTINCT diocese_id
                        FROM vistas.b_edm_ss_sa b
                        WHERE order_id = 121)) THEN 'with_ops'
       ELSE 'no_ops'
       END AS ops
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     --- seleccionamos sololos q son más q elpercentil x
WHERE dg.tasa_media >= (
   SELECT percentile_cont($1) WITHIN group (ORDER BY tasa_media)
   FROM bishops.dioceses_global_fl_estimado dg
   JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
        USING (diocese_id)
        WHERE pais = $2)
AND pais = $2;
