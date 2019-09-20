WITH bizancio as
(SELECT DISTINCT place_id, longitude, latitude FROM places
        WHERE country IN ('Greece', 'Ucraine', 'Romania', 'Bulgaria', 'Albania', 'Slovenia',
        'Croacia', 'Serbia', 'Montenegro', 'Cyprus', 'Lebanon', 'Israel', 'Palestina', 'Turkey' )
),
dioceses_bizancio AS 
(SELECT DISTINCT d.diocese_id FROM dioceses d
     JOIN bizancio ON bizancio.place_id = d.place_id),
series AS
      (SELECT generate_series(1200, 1790, 10) AS r_from),
      rangos AS (
      SELECT r_from, (r_from + 9) AS r_to FROM series)
SELECT r_from, r_to,
       (SELECT count(*) FROM vistas.b_emd_ss_sa b
               JOIN dioceses_bizancio ON dioceses_bizancio.diocese_id = b.diocese_id 
               WHERE (extract(year from date_nomination) BETWEEN r_from AND r_to))  AS total
FROM rangos;
