-- esto son las duraciones de los obispados 

SELECT DISTINCT bishop_id,
                EXTRACT(year from date_nomination) as inicio,
                EXTRACT(year from date_end) as fin,
                COALESCE(
                        SUM(EXTRACT(year from date_end)
                        -
                        EXTRACT(year from date_nomination)), 0) AS duracion
FROM bishops
WHERE date_nomination is not null and  date_end is not null
group by bishop_id;
