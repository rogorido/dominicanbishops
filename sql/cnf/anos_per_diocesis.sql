SELECT d.diocese_name AS diocesis,
       p.country AS pais,
       ST_SetSRID(ST_MakePoint(P.longitude, P.latitude),4326) AS coord,
       COALESCE(
                (SUM(date_end - date_nomination) / 365.0), 0) AS duracion
FROM vistas.b_edm_ss_sa b
     JOIN dioceses d ON d.diocese_id = b.diocese_id
     join places p on d.place_id = p.place_id
WHERE (date_nomination is not null and date_end is not NULL) AND order_id = 121
group by 1, 2, 3
ORDER BY duracion DESC
