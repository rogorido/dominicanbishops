--- nos saca una tabla general para q veamos loas dićoesis donde están
--- y no esan para q se peuda ver rápidamente 

SELECT dg.*,
       CASE WHEN
            (SELECT diocese_id
                    IN (SELECT DISTINCT diocese_id
                        FROM vistas.b_edm_ss_sa b
                        WHERE order_id = 121)) THEN TRUE 
       ELSE FALSE 
       END AS ops
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
WHERE pais = $1
