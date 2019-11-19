-- está en a1ae5c80-7665-42fe-afa1-9ec87f6c7e9d

SELECT bishop_all_id, diocese_id,
       CASE
          WHEN religious_order IS NULL THEN 'secular'
          ELSE 'fraile'
       END AS tiporeligioso,
       CASE
          WHEN p.country IN 
 ('Greece', 'Ucraine', 'Romania', 'Bulgaria', 'Albania', 'Slovenia',
  'Croacia', 'Serbia', 'Montenegro', 'Cyprus', 'Lebanon', 'Israel', 'Palestina', 'Turkey' ) THEN 'bizancio'
       ELSE 'no-bizancio'
       END AS esbizancio
FROM vistas.b_emd_cs_sa
JOIN dioceses USING (diocese_id)
JOIN places p USING(place_id);
