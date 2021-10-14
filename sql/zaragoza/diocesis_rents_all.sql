SELECT dg.*
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id
      FROM vistas.b_edm_cs_sa b) j
     USING (diocese_id);
