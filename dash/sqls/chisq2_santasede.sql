-- en a1ae5c80-7665-42fe-afa1-9ec87f6c7e9d

SELECT bishop_all_id, diocese_id,
       CASE
          WHEN religious_order IS NULL THEN 'secular'
          ELSE 'fraile'
       END AS tiporeligioso,
       d.essantasede 
FROM vistas.b_edm_cs_sa
LEFT JOIN bishops.dioceses_global_fl d USING(diocese_id)
