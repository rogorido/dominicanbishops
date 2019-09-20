-- Esto está en fa9a10b6-1d48-4143-912a-05ec59ef44f4

CREATE OR replace TEMP VIEW tv_rentas_por_obispo_orrg AS 

SELECT diocese_name, longitude, latitude,
       order_acronym,
       mesa_estimada_bool,
       round(mesa_estimado, 1), SUM(duracion),
       round(mesa_estimado, 1) * SUM(duracion) as total_rentas
FROM tv_rentas_por_obispo
WHERE order_acronym IS NOT NULL
GROUP by diocese_name,
         longitude, latitude, order_acronym,
         mesa_estimado, mesa_estimada_bool
ORDER by diocese_name;
