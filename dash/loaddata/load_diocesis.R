message("Cargando los datos de diócesis")

#######################################
# 1. Todos los obispos con seculares (1200-1800)
# pero sin los afiliados
#######################################

datossql <- getSQL("./sqls/obispospordioc.sql")
bis_per_dioc <- dbGetQuery(con, datossql)

# por siglos
datossql <- getSQL("./sqls/obispospordioc_porsiglos.sql")
bis_per_dioc_por_siglo <- dbGetQuery(con, datossql)


#######################################
# 2. Diócesis q se terminan 
#######################################

datossql <- getSQL("./sqls/diocesis_con_fin.sql")
dioc_con_fin <- dbGetQuery(con, datossql)

#######################################
# 3. Diócesis sin órdenes religiosas überhaupt
# solo mostramos mapa 
#######################################

datossql <- getSQL("./sqls/dioc_sin_orrg.sql")
dioc_sin_orrg <- dbGetQuery(con, datossql)

#######################################
# 3. Órdenes religiosasy dónde no están 
#######################################

# 3.1 primero en el periodo 1200-1800
ejecutar <- getSQL("./sqls/dioc_sin_frailes1.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
datossql <- getSQL("./sqls/dioc_sin_frailes2.sql")
orrg_sin_dioc_emd <- dbGetQuery(con, datossql)
orrg_sin_dioc_emd$order_acronym <- factor(orrg_sin_dioc_emd$order_acronym)
orrg_sin_dioc_emd_lista <- as.list(levels(orrg_sin_dioc_emd$order_acronym))

orrg_sin_dioc_emd$country <- factor(orrg_sin_dioc_emd$country)

# aquí lo tenemos agregados
ejecutar <- getSQL("./sqls/dioc_sin_frailes3.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
orrg_sin_dioc_emd_agregado <- dbGetQuery(con,
                                         "SELECT * from tv_diocesissinfrailes_porcentajes_emd;")

# 3.2 para la edm 
ejecutar <- getSQL("./sqls/dioc_sin_frailes4.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
datossql <- getSQL("./sqls/dioc_sin_frailes5.sql")
orrg_sin_dioc_edm <- dbGetQuery(con, datossql)
orrg_sin_dioc_edm$order_acronym <- factor(orrg_sin_dioc_edm$order_acronym)
orrg_sin_dioc_edm_lista <- as.list(levels(orrg_sin_dioc_edm$order_acronym))

orrg_sin_dioc_edm$country <- factor(orrg_sin_dioc_edm$country)

# aquí lo tenemos agregados
ejecutar <- getSQL("./sqls/dioc_sin_frailes6.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
orrg_sin_dioc_edm_agregado <- dbGetQuery(con,
                                     "SELECT * from tv_diocesissinfrailes_porcentajes_edm;")


#######################################
# 4. Diócesis de la santa sede 
#######################################

datossql <- getSQL("./sqls/dioc_santasede.sql")
dioc_santasede <- dbGetQuery(con, datossql)

#######################################
# 5. Papas y nombramientos por diócesis 
#######################################

datossql <- getSQL("./sqls/dioc_papas_totales.sql")
dioc_papas_totales <- dbGetQuery(con, datossql)

dioc_papas_totales$pope_name <- factor(dioc_papas_totales$pope_name)
dioc_papas_lista <- as.list(levels(dioc_papas_totales$pope_name))

# todos los obispos no-seculares 
sql <- "SELECT * FROM b_emd_ss_sa"

#######################################
# 6. Papas que no nombran en diócesis 
#######################################

ejecutar <- getSQL("./sqls/dioc_sinpapas1.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
datossql <- getSQL("./sqls/dioc_sinpapas2.sql")
dioc_sinpapas_congis <- dbGetQuery(con, datossql)

dioc_sinpapas_congis$pope_name <- factor(dioc_sinpapas_congis$pope_name)
dioc_sinpapas_congis_lista <- as.list(levels(dioc_sinpapas_congis$pope_name))

datossql <- getSQL("./sqls/dioc_sinpapas3.sql")
dioc_sinpapas_agregados <- dbGetQuery(con, datossql)

#######################################
# 7. Rentas 
#######################################

dioc_rentas <- dbGetQuery(con, "SELECT * FROM bishops.dioceses_global_fl_estimado;")

ejecutar <- getSQL("./sqls/rentas_por_obispo1.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
ejecutar <- getSQL("./sqls/rentas_por_obispo2.sql")
dbSendQuery(con, ejecutar) # creamos una temp view

# con esto rtt cogemos solo los datos de las 10 orrg
# más ipt de la edm
datossql <- getSQL("./sqls/rentas_por_obispo3.sql")
rentas_por_obispo_orrg <- dbGetQuery(con, datossql)
rentas_por_obispo_orrg$order_acronym <- factor(rentas_por_obispo_orrg$order_acronym)
rentas_por_obispo_orrg$diocese_name <- factor(rentas_por_obispo_orrg$diocese_name)

#######################################
# 8. Duraciones  
#######################################
datossql <- getSQL("./sqls/dioc_duraciones.sql")
dioc_duraciones <- dbGetQuery(con, datossql)

dioc_duraciones_min <- min(dioc_duraciones$mediaanos)
dioc_duraciones_max <- max(dioc_duraciones$mediaanos)

# y luego huecos


#######################################
# 8. Periplos 
#######################################
ejecutar <- getSQL("./sqls/periplos1.sql")
dbSendQuery(con, ejecutar) # creamos una temp view

# aquí cogemos todos los de 1200-1800 sumados. No sé para qué me sirve
# esta tabla...
datossql <- getSQL("./sqls/periplos2.sql")
periplos_emd_cs_sa_totales <- dbGetQuery(con, datossql)

# aquí cogemos todos los de 1200-1800 sumados. No sé para qué me sirve
# esta tabla...
datossql <- getSQL("./sqls/periplos2.sql")
periplos_emd_cs_sa_totales <- dbGetQuery(con, datossql)

# para la edm
ejecutar <- getSQL("./sqls/periplos3.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
# aquí cogemos todos los de 1200-1800 sumados. No sé para qué me sirve
# esta tabla...
datossql <- getSQL("./sqls/periplos4.sql")
periplos_edm_cs_sa_totales <- dbGetQuery(con, datossql)

# frailes, 1200-1800
ejecutar <- getSQL("./sqls/periplos5.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
datossql <- getSQL("./sqls/periplos6.sql")
periplos_emd_ss_sa_totales <- dbGetQuery(con, datossql)

# frailes, edm 
ejecutar <- getSQL("./sqls/periplos7.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
datossql <- getSQL("./sqls/periplos8.sql")
periplos_edm_ss_sa_totales <- dbGetQuery(con, datossql)

# por décadas (1200-1800)
datossql <- getSQL("./sqls/periplos9.sql")
periplos_emd_cs_sa_decadas <- dbGetQuery(con, datossql)

### pares de destinos
ejecutar <- getSQL("./sqls/periplos10.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
datossql <- getSQL("./sqls/periplos11.sql")
periplospares_emd_cs_sa <- dbGetQuery(con, datossql)

### pares de destinos (edm)
ejecutar <- getSQL("./sqls/periplos12.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
datossql <- getSQL("./sqls/periplos13.sql")
periplospares_edm_cs_sa <- dbGetQuery(con, datossql)

### pares de destinos: frailes, 1200-1800
ejecutar <- getSQL("./sqls/periplos14.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
datossql <- getSQL("./sqls/periplos15.sql")
periplospares_emd_ss_sa <- dbGetQuery(con, datossql)

### pares de destinos: frailes, edm 
ejecutar <- getSQL("./sqls/periplos16.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
datossql <- getSQL("./sqls/periplos17.sql")
periplospares_edm_ss_sa <- dbGetQuery(con, datossql)

#######################################
# 9. Seculares/vs no seculares
#######################################

datossql <- getSQL("./sqls/dioc_sec_vs_nosec1.sql")
dioc_sec_vs_nosec_emd <- dbGetQuery(con, datossql)

datossql <- getSQL("./sqls/dioc_sec_vs_nosec2.sql")
dioc_sec_vs_nosec_edm <- dbGetQuery(con, datossql)

