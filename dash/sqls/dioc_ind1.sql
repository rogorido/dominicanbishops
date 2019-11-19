-- esto hay que añadirle un elemento que lo hacemos con paste0
-- en el propio código de R

SELECT bishop_all_id,
       (case
                when order_acronym IS null THEN 'Secular'
                else order_acronym
              end)
FROM vistas.b_emd_cs_sa
WHERE diocese_id =
