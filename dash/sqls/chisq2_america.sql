-- en: a1ae5c80-7665-42fe-afa1-9ec87f6c7e9d

SELECT bishop_all_id, diocese_id,
       CASE
          WHEN religious_order IS NULL THEN 'secular'
          ELSE 'fraile'
       END AS tiporeligioso,
       CASE
          WHEN 'America' = ANY(p.macroregions) THEN 'America'
          ELSE 'no-America'
       END AS esamerica
FROM vistas.b_edm_cs_sa
JOIN dioceses USING (diocese_id)
JOIN places p USING(place_id);
