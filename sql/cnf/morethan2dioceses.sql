-- bstt seleccionamos los q están en 1ª diocesis
-- cuando aparecen tb en una segunda

SELECT url, anos, 'morethanone' AS moredioceses
FROM vistas.bishops_individuals_edm_op
WHERE ordinal = 1
      AND url IN (SELECT DISTINCT url
FROM vistas.bishops_individuals_edm_op
WHERE ordinal = 2)
      AND anos IS NOT NULL
UNION ALL
SELECT url, anos, 'onlyone' AS moredioceses
FROM vistas.bishops_individuals_edm_op
WHERE ordinal = 1
      AND url NOT IN (SELECT DISTINCT url
FROM vistas.bishops_individuals_edm_op
WHERE ordinal = 2)
      AND anos IS NOT NULL;
