message("Cargando los datos de confesionalización")

#######################################
# 1. Por órdenes (solo europa)
#######################################

datossql <- getSQL("./sqls/confe_ordenes.sql")
cnf_ordenes <- dbGetQuery(con, datossql)

cnf_ordenes$religious_order <- factor(cnf_ordenes$religious_order)
cnf_ordenes_lista <- as.list(levels(cnf_ordenes$religious_order))

#######################################
# Totales del tiempo q están 
#######################################

datossql <- getSQL("./sqls/confe_ordenes_sumaanos.sql")
cnf_ordenes_sumaanos <- dbGetQuery(con, datossql)

cnf_ordenes_sumaanos$religious_order <- factor(cnf_ordenes_sumaanos$religious_order)
cnf_ordenes_sumaanos_lista <- as.list(levels(cnf_ordenes_sumaanos$religious_order))


