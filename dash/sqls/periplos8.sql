-- agregamos lo de periplos7.sql 

WITH totales AS (
     SELECT url, COUNT(*) AS total 
     FROM vt_periplo_edm_ss_sa
     GROUP BY url),

reducido AS (
SELECT DISTINCT url, bishop_surname, bishop_name
FROM  bishops_all
)

SELECT '<a href="http://www.catholic-hierarchy.org' || url || '" target="_blank">' || url || '</a>' AS url,
       b.bishop_surname, b.bishop_name, total
FROM totales
LEFT JOIN reducido b USING(url);
