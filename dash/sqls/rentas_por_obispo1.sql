-- Esto está en fa9a10b6-1d48-4143-912a-05ec59ef44f4

CREATE OR REPLACE TEMP VIEW tv_rentas_por_obispo AS 

WITH rentas AS (
     SELECT bishop_all_id, diocese_id, diocese_name,
            longitude, latitude, date_nomination,
            mesa_estimado, mesa_estimada_bool
     FROM b_emd_cs_sa
     JOIN bishops.dioceses_global_fl_estimado USING (diocese_id))

SELECT bishop_all_id, reason_begin, reason_end, destination,
       rentas.diocese_id, rentas.diocese_name,
       longitude, latitude,
       rentas.date_nomination, mesa_estimado, mesa_estimada_bool,
       COALESCE(
                SUM(EXTRACT(YEAR FROM date_end)
                                 -
                    EXTRACT(YEAR FROM rentas.date_nomination)), 0) AS duracion,
        order_id,
        order_acronym,
        order_name_english,
        order_type
FROM b_edm_cs_sa b
JOIN rentas USING (bishop_all_id)
WHERE rentas.date_nomination IS NOT NULL AND date_END IS NOT NULL
GROUP BY bishop_all_id, reason_begin, reason_end, destination,
         rentas.diocese_id, rentas.diocese_name,
         longitude, latitude,
         rentas.date_nomination, mesa_estimado, mesa_estimada_bool,
         order_id,
         order_acronym,
         order_name_english,
         order_type
ORDER BY diocese_id;

