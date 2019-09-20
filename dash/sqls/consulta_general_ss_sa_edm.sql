-- Esto es una talba muy general para consultar si queremos ver rápidamente
-- los obipsos de la diócesis X, o del país Y. Lo hago solo para frailes y
-- solo para la Edad Moderna y además creo urls:
-- está en c35fbc68-a99e-4311-a0ad-cdfc2ad8d8cc

CREATE OR REPLACE TEMP VIEW tv_consultageneral_ss_sa_edm AS 

SELECT 
       '<a href="http://www.catholic-hierarchy.org' || url || '" target="_blank">' || bishop_name || ' ' || bishop_surname || '</a>' AS url,
       religious_order,
       date_nomination,
       date_end,
       d.diocese_name,
       '<a href="' || url_hierarchy || '" target="_blank">' || d.diocese_name || '</a>' AS diocesis_url,
       p.country
FROM vistas.b_edm_ss_sa
JOIN dioceses d USING(diocese_id)
LEFT JOIN places P USING(place_id);
