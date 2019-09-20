-- Está en d627605d-b8cf-478c-a2d7-2f3e4748a21b

WITH general AS (
     SELECT diocese_id,
            avg(date_end - date_nomination) as media,
            stddev(date_end - DATE_nomination) AS sd
     FROM vistas.b_edm_cs_sa
     GROUP BY diocese_id)

SELECT DISTINCT diocese_id, 
                d.diocese_name,
                p.longitude, p.latitude,
                round(media/365, 1) AS mediaanos,
                round(sd/365, 1) AS sdanos
FROM general
  JOIN dioceses d USING(diocese_id)
  JOIN places P USING (place_id);
