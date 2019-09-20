-- está en 8c4a60e2-bef5-4254-aff2-00903e18160e

CREATE OR REPLACE TEMP VIEW tv_motivos_fin_ss_edm AS 

WITH sumas AS (
     SELECT reason_end, religious_order AS rel_order,
            COUNT(*) AS total
     FROM vistas.b_edm_ss_sa
     GROUP BY reason_end, religious_order)

SELECT reason_end, rel_order, total, 
       (SELECT COUNT(*)
        FROM vistas.b_edm_ss_sa
        WHERE religious_order = rel_order) totalobispos,
       total::numeric / (SELECT COUNT(*) FROM vistas.b_edm_ss_sa WHERE religious_order = rel_order) AS porcentaje 
FROM sumas;
