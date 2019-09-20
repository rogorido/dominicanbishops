-- periplos por diócesis.
-- está en: 891b2ec2-537f-4164-9987-aa00f7706643

--- juntamos todas los pares 
WITH juntas AS (
     SELECT url, sucesion[1] AS dioc1, sucesion[2] AS dioc2
     FROM tv_agregadodiocesis_emd_OPs
     UNION ALL 
     SELECT url, sucesion[2] AS dioc1, sucesion[3] AS dioc2
     FROM tv_agregadodiocesis_emd_OPs
     UNION ALL 
     SELECT url, sucesion[3] AS dioc1, sucesion[4] AS dioc2
     FROM tv_agregadodiocesis_emd_OPs
     UNION ALL 
     SELECT url, sucesion[4] AS dioc1, sucesion[5] AS dioc2
     FROM tv_agregadodiocesis_emd_OPs),

--- quitamos los pares donde haya algún null 
sinnulls AS (
    SELECT * FROM juntas
    WHERE dioc1 IS NOT NULL AND dioc2 IS NOT NULL)

--- hacemos el recuento 
SELECT dioc1, dioc2, COUNT(*) as total 
FROM sinnulls
GROUP BY dioc1, dioc2
ORDER BY COUNT(*) DESC; 
