-- Esto está en fa9a10b6-1d48-4143-912a-05ec59ef44f4

SELECT t.*
FROM tv_rentas_por_obispo_orrg t -- esto usa la temp view anterior!
WHERE t.order_acronym IN
      (SELECT DISTINCT order_acronym
       FROM b_edm_ss_sa_top10orders);
