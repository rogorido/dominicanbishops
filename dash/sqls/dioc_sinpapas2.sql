-- Esto se reduce a la edm!
-- Se trata de b0dc901e-af8c-497b-9ee3-ae47c5796d47
-- Ie: diócesis en las que le papa X no ha intervenido. Con datos de
-- lat/long para mostrarlo en mapa 

SELECT d.pope_id, p.name_pope || ' (' ||
       extract(year from p.date_nomination) || '-' || extract(year from p.date_death) || ')' AS pope_name,
       p.date_nomination, p.date_death,
       dc.diocese_name,
       pl.longitude, pl.latitude
FROM tv_diocesissinpapas_edm d
JOIN popes p USING(pope_id)
JOIN dioceses dc USING(diocese_id)
JOIN places pl USING(place_id)
ORDER BY p.date_nomination ASC;
