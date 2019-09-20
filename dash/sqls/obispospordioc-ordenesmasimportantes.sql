-- Obispos por diócesis con las órdenes más importantes 

WITH a AS 
       (SELECT d.diocese_name, religious_order, COUNT(*) AS cuenta 
               FROM vistas.b_emd_ss_sa_top10orders b
               JOIN dioceses d USING (diocese_id)
               GROUP BY d.diocese_name, religious_order)

SELECT a.diocese_name, religious_order, cuenta, cuenta/SUM(cuenta)
       OVER (PARTITION BY a.diocese_name)
FROM a
ORDER BY a.diocese_name;
