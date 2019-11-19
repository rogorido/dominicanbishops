-- Lo uso para tener una visión general de los nombres y sus abreviaturas,
-- porque luego nunca me aclaro. El único misterio que tiene es que en
-- lugar de coger todas las que hay (que tengo unas 160) cojo solo las que
-- aparecen realmente en mis datos.
-- en 8541ca21-e9c1-48b3-8291-ad0c03e5b734

SELECT DISTINCT r.order_acronym,
       r.order_name_english AS order_name,
       r.order_type AS type_order,
       r.year_foundation,
       COUNT(bishop_all_id)
FROM religious_orders r
JOIN vistas.b_emd_ss_sa USING(order_id)
GROUP BY r.order_acronym,
      order_name,
      type_order,
      r.year_foundation
ORDER BY order_acronym;
