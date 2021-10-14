--- ver explicaciones en analisisdatos.org
--- en 134fd81f-2078-44f8-8992-4c2d5637115e
--- y sto
--- 7de46ad0-9359-494c-b714-5552fa228ada

--- ax al final he tenido que hacer chapuza pq el driver
--- de rpostgresql no acepta el array[] de postgresql 

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
WHERE dg.tasa_media between (
SELECT percentile_cont($1) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais = $3) AND
     (
SELECT percentile_cont($2) WITHIN group (ORDER BY tasa_media)
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id FROM vistas.b_edm_ss_sa b) j
     USING (diocese_id)
     WHERE pais = $3)
AND pais = $3;
