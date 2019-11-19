message("Cargando los datos generales")

total_diocesis <- dbGetQuery(con, "SELECT COUNT(*) FROM dioceses;")

total_obispos <- dbGetQuery(con, "SELECT COUNT(DISTINCT url) FROM vistas.b_all_cs_sa;")
total_obispos_edm <-
    dbGetQuery(con, "SELECT COUNT(DISTINCT url) FROM vistas.b_edm_cs_sa;")

total_obispos_op <-
    dbGetQuery(con, "SELECT COUNT(DISTINCT url) FROM vistas.b_all_ss_sa WHERE religious_order = 'O.P.';")

total_obispos_op_edm <-
    dbGetQuery(con, "SELECT COUNT(DISTINCT url) FROM vistas.b_edm_ss_sa WHERE religious_order = 'O.P.';")

total_obispos_frailes <-
    dbGetQuery(con, "SELECT COUNT(DISTINCT url) FROM vistas.b_all_ss_sa WHERE religious_order IS NOT NULL;")

total_obispos_frailes_edm <-
    dbGetQuery(con, "SELECT COUNT(DISTINCT url) FROM vistas.b_all_ss_sa WHERE religious_order IS NOT NULL;")

# cargamos una serie de vectores con nombres de países q
# luego usamos para filtrar en algunos sitios

ejecutar <- getSQL("./sqls/listas_paises.sql")
dbSendQuery(con, ejecutar) # creamos una temp view

paises_todos <- dbGetQuery(con, "SELECT * FROM tv_paises_todos")
paises_todos <- as.vector(paises_todos$country)
lista_paises_todos <- as.list(paises_todos)

paises_europa <- dbGetQuery(con, "SELECT * FROM tv_paises_europa") 
paises_europa <- as.vector(paises_europa$country)

paises_noeuropa <- dbGetQuery(con, "SELECT * FROM tv_paises_noeuropa") 
paises_noeuropa <- as.vector(paises_noeuropa$country)

paises_bizancio <- dbGetQuery(con, "SELECT * FROM tv_paises_bizancio") 
paises_bizancio <- as.vector(paises_bizancio$country)

# aquí habría q cargar tb la lista orrg_lista que es realmente es una
# lista que creamos para usarla en general en comoboxes para seleccionar
# dióceses. PERO: la cargamos en load_orrgs.R al crear ahí una temp view. 

dioceses <- dbGetQuery(con, "SELECT DISTINCT diocese_id, diocese_name from dioceses ORDER BY diocese_name")

dioceses_lista_ids <- dioceses$diocese_id
dioceses_lista <- dioceses$diocese_name
