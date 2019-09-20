-- obispos por diócesis: 
-- Lo cogemos de Todos los obispos con seculares (1200-1800)
-- sin los afiliados

-- hace falta hacer dos campos, uno con el nombre y otro con la url
-- pq si no luego no lo ordena bien DataTable. 

WITH general AS (
     SELECT diocese_id,
            COUNT(*) AS total FROM vistas.b_emd_cs_sa
     GROUP BY diocese_id)
     
SELECT DISTINCT diocese_id, d.diocese_name,
       '<a href="'  || url_hierarchy || '" target="_blank">' || d.diocese_name || '</a>' AS url,
       p.longitude, p.latitude, total 
FROM general
  JOIN dioceses d USING(diocese_id)
  JOIN places P USING (place_id);
