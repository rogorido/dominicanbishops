-- lo tenemos en 4b595131-06fb-4b9e-93bf-ddb5f6eac005

CREATE OR REPLACE TEMP VIEW tv_ops_bizancio AS

WITH bizancio AS
(SELECT DISTINCT place_id, place, country, longitude, latitude
 FROM  places
 WHERE country IN ('Greece', 'Ucraine', 'Romania', 'Bulgaria', 'Albania', 'Slovenia',
       'Croacia', 'Serbia', 'Montenegro', 'Cyprus', 'Lebanon', 'Israel', 'Palestina', 'Turkey' ))
SELECT d.diocese_id, d.diocese_name as diocesis,
       bz.longitude, bz.latitude, bz.place, bz.country,
       extract(year from date_nomination) as inicio,
       extract(year from date_end) as fin,
       extract(year from date_end) - extract(year from date_nomination) as duracion
FROM vistas.b_emd_ss_sa b
     JOIN dioceses d ON d.diocese_id = b.diocese_id
     JOIN bizancio bz ON bz.place_id = d.place_id
WHERE religious_order = 'O.P.'
ORDER BY inicio;
