-- resulta increíble pero si no lo pongo en un CTE
-- ogr2ogr me da error.

-- derivada de all_dioceses solo añadiendo el filtro.
WITH j AS
(SELECT d.diocese_id, d.diocese_name, d.diocese_latin_name,
        d.archidiocese, d.sufragean_id,
        d.nowadays, d.infidelibus, d.disappeared, d.titular_see,
        d.vatican, d.url_hierarchy,
        ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
        p.country,
        --- aquí empieza lo q está enla tabla de las rentas
        r.fundacion, r.parishes, r.mesa_media, r.tasa_media,
        r.essantasede, r.n_mesas, r.n_tasas, r.metropolitana, r.place,
        r.mesa_estimado, r.tasa_estimado,
        r.mesa_estimada_bool, r.tasa_estimada_bool,
        -- aquí empiezan campos calculados: 1. si hay o no OPs, 2. cuántos
        CASE
         WHEN d.diocese_id IN (SELECT DISTINCT diocese_id FROM
                              vistas.bishops_individuals_edm_op b)
              THEN 'conops'
         ELSE 'sinops'
        END AS oppresence,
        (SELECT COUNT(*) FROM vistas.bishops_individuals_edm_op b
         WHERE b.diocese_id = d.diocese_id) AS totalops
 FROM general.dioceses d JOIN places P USING (place_id)
 LEFT OUTER JOIN dioceses_global_fl_estimado r USING (diocese_id))
SELECT j.* FROM j
WHERE country IN ('Bosnia and Herzegovina', 'Croacia', 'Slovenia',
               'Montenegro', 'Greece', 'Albania', 'Cyprus', 'Macedonia',
               'Serbia', 'Turkey');
