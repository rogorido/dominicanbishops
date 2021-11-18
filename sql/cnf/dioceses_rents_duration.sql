SELECT b.*, dg.tasa_media
FROM vistas.bishops_individuals_edm_op b
join bishops.dioceses_global_fl_estimado dg USING (diocese_id);
