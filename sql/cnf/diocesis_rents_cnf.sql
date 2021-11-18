SELECT dg.*
FROM bishops.dioceses_global_fl_estimado dg
JOIN (SELECT DISTINCT diocese_id
      FROM vistas.b_edm_ss_sa b
      WHERE order_id = 121) j
     USING (diocese_id);
