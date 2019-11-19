
output$bis_per_dioc <- renderDataTable({
    mostrar <- bis_per_dioc %>%
        select(diocese_name, url, total) %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 50))
})

output$bis_per_dioc_mapa <-
    renderLeaflet({
        leaflet(bis_per_dioc) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             label = ~htmlEscape(paste0(diocese_name, " (", total, " )")),
                             radius = ~total,
                             clusterOptions = markerClusterOptions())})

output$bis_per_dioc_por_siglos <-
    renderDataTable({

        # lo pasamos a wide
        mostrar <- spread(bis_per_dioc_por_siglo, siglo, total)
        
        mostrar <- mostrar %>%
            arrange(diocese_name)

        DT::datatable(mostrar, filter = 'top',
                      options = list(pageLength = 50))
})

output$dioc_santasede <-
    renderLeaflet({
        leaflet(dioc_santasede, height = 1000) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             label = ~htmlEscape(diocese_name))})

output$dioc_sin_orrg_emd <-
    renderLeaflet({
        leaflet(dioc_sin_orrg) %>%
            addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             label = ~htmlEscape(diocese_name))})


output$orrg_sin_dioc_emd <-
    renderDataTable({
        mostrar <- orrg_sin_dioc_emd %>%
            select(order_acronym, diocese_name, country) %>%
            filter(order_acronym == input$seleccionar_orrg_sin_dioc_emd)

        DT::datatable(mostrar, filter = 'top', extensions = 'Buttons',
                      options = list(pageLength = 10, dom = 'Bfrtip',
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    })

output$orrg_sin_dioc_emd_agregados <-
    renderDataTable({
        mostrar <- orrg_sin_dioc_emd_agregado %>%
            select(orden, pais, totalpais, faltan, percentage) %>%
            filter(orden == input$seleccionar_orrg_sin_dioc_emd)

        DT::datatable(mostrar, filter = 'top', extensions = 'Buttons',
                      options = list(pageLength = 10, dom = 'Bfrtip',
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    })

output$orrg_sin_dioc_emd_mapa <-
    renderLeaflet({
        mostrar <- orrg_sin_dioc_emd %>%
            filter(order_acronym == input$seleccionar_orrg_sin_dioc_emd)

        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             label = ~htmlEscape(diocese_name))})

output$orrg_sin_dioc_edm <-
    renderDataTable({
        mostrar <- orrg_sin_dioc_edm %>%
            select(order_acronym, diocese_name, country) %>%
            filter(order_acronym == input$seleccionar_orrg_sin_dioc_edm)

        DT::datatable(mostrar, filter = 'top', extensions = 'Buttons',
                      options = list(pageLength = 10, dom = 'Bfrtip',
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    })


output$orrg_sin_dioc_edm_agregados <-
    renderDataTable({
        mostrar <- orrg_sin_dioc_edm_agregado %>%
            select(orden, pais, totalpais, faltan, percentage) %>%
            filter(orden == input$seleccionar_orrg_sin_dioc_edm)

        DT::datatable(mostrar, filter = 'top', extensions = 'Buttons',
                      options = list(pageLength = 10, dom = 'Bfrtip',
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    })

output$orrg_sin_dioc_edm_mapa <-
    renderLeaflet({
        mostrar <- orrg_sin_dioc_edm %>%
            filter(order_acronym == input$seleccionar_orrg_sin_dioc_edm)

        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             label = ~htmlEscape(diocese_name))})

output$dioc_papas <- renderDataTable({
    mostrar <- dioc_papas_totales %>%
        select(pope_name, diocese_name, total) %>%
        filter(pope_name == input$seleccionar_dioc_papas) %>%
        arrange(total)

    DT::datatable(mostrar, filter = 'top',
                      options = list(pageLength = 10))
})

output$dioc_papas_mapa <-
    renderLeaflet({
        mostrar <- dioc_papas_totales %>%
            filter(pope_name == input$seleccionar_dioc_papas)

        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             #radius = ~total, # esto no funciona con estas otras opciones?
                             label = ~htmlEscape(paste0(diocese_name, "(", total, ")")),
                             clusterOptions = markerClusterOptions())})

output$dioc_sinpapas <- renderDataTable({
    mostrar <- dioc_sinpapas_agregados %>%
        select(nombre_papa, total) %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                      options = list(pageLength = 15))
})

output$dioc_sinpapas_mapa <-
    renderLeaflet({
        mostrar <- dioc_sinpapas_congis %>%
            filter(pope_name == input$seleccionar_dioc_sinpapas)

        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             #radius = ~total, # esto no funciona con estas otras opciones?
                             clusterOptions = markerClusterOptions())})

output$dioc_rentas <- renderDataTable({
    mostrar <- dioc_rentas %>%
        select(diocese_name, mesa_media, tasa_media, indice, 
               essantasede, mesa_estimado, tasa_estimado)

    DT::datatable(mostrar, filter = 'top',
                      options = list(pageLength = 15))
})


output$dioc_rentas_mapa <-
    renderLeaflet({
        mostrar <- dioc_rentas
        
        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             label = ~htmlEscape(paste0(diocese_name, "(", indice, ")")),
                             radius = ~indice)})

output$rentas_por_obispo_orrg_top10 <- renderDataTable({

    # hacemos un wide format que ciertamente pierde algunos datos, pero da igual 
    mostrar <- rentas_por_obispo_orrg %>%
        select(diocese_name, order_acronym, total_rentas)

    mostrar <- spread(mostrar, key = order_acronym, value = total_rentas)

    DT::datatable(mostrar, filter = 'top',
                      options = list(pageLength = 50))
})


output$dioc_duraciones <- renderDataTable({
    mostrar <- dioc_duraciones %>%
        select(diocese_name, mediaanos, sdanos) %>%
        filter(mediaanos >= input$seleccionar_dioc_duraciones[1],
               mediaanos <= input$seleccionar_dioc_duraciones[2])

    DT::datatable(mostrar, filter = 'top',
                      options = list(pageLength = 15))
})

output$dioc_duraciones_mapa <-
    renderLeaflet({
        mostrar <- dioc_duraciones %>%
            filter(mediaanos >= input$seleccionar_dioc_duraciones[1],
                   mediaanos <= input$seleccionar_dioc_duraciones[2])
        
        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             label = ~htmlEscape(paste0(diocese_name, "(", mediaanos, ")")),
                             radius = ~mediaanos)})

output$periplos_emd_cs_sa_totales <- renderDataTable({
    mostrar <- periplos_emd_cs_sa_totales %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 15))
})

output$periplos_edm_cs_sa_totales <- renderDataTable({
    mostrar <- periplos_edm_cs_sa_totales %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 15))
})

output$periplos_emd_ss_sa_totales <- renderDataTable({
    mostrar <- periplos_emd_ss_sa_totales %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 15))
})

output$periplos_edm_ss_sa_totales <- renderDataTable({
    mostrar <- periplos_edm_ss_sa_totales %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 15))
})

output$periplos_emd_cs_sa_decadas <- renderDataTable({
    mostrar <- periplos_emd_cs_sa_decadas

    DT::datatable(mostrar, filter = 'top',
                  options = list(pageLength = 100))
})


output$periplospares_emd_cs_sa <- renderDataTable({
    mostrar <- periplospares_emd_cs_sa %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 50))
})

output$periplospares_edm_cs_sa <- renderDataTable({
    mostrar <- periplospares_edm_cs_sa %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 50))
})

output$periplospares_emd_ss_sa <- renderDataTable({
    mostrar <- periplospares_emd_ss_sa %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 50))
})

output$periplospares_edm_ss_sa <- renderDataTable({
    mostrar <- periplospares_edm_ss_sa %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 50))
})


output$dioc_con_fin <- renderDataTable({
    mostrar <- dioc_con_fin %>%
        select(diocese_name, country, ultimofin) %>%
        arrange(ultimofin)

    DT::datatable(mostrar, filter = 'top',
                      options = list(pageLength = 50))
})

output$dioc_con_fin_mapa <-
    renderLeaflet({
        anos <- input$slider_dioc_con_fin

        mostrar <- dioc_con_fin %>%
            dplyr::filter(ultimanominacion >= anos[1], ultimofin <= anos[2])

        colores <- colorFactor(topo.colors(10), mostrar$country)
        
        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             label = ~htmlEscape(diocese_name),
                             color = ~colores(country))})

output$dioc_sec_vs_nosec_emd <- renderDataTable({
    mostrar <- dioc_sec_vs_nosec_emd %>%
        mutate(porcentaje = round(porcentaje, 2)) %>%
        select(diocese_id, diocese_name, orden, porcentaje) %>%
        spread(orden, porcentaje) %>%
        mutate(odds = round(sinorder / (1 - sinorder), 2)) %>%
        select(-diocese_id)

    DT::datatable(mostrar, filter = 'top',
                      options = list(pageLength = 50))
})

output$dioc_sec_vs_nosec_emd_mapa <-
    renderLeaflet({
        mostrar <- dioc_sec_vs_nosec_emd %>%
            mutate(porcentaje = round(porcentaje, 2)) %>%
            #select(diocese_id, diocese_name, orden, porcentaje) %>%
            spread(orden, porcentaje) %>%
            select(-diocese_id)

        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             radius = ~conorder,
                             label = ~htmlEscape(paste0(diocese_name, "( ", conorder, "% )")))})

output$dioc_sec_vs_nosec_edm <- renderDataTable({
    mostrar <- dioc_sec_vs_nosec_edm %>%
        mutate(porcentaje = round(porcentaje, 2)) %>%
        select(diocese_id, diocese_name, orden, porcentaje) %>%
        spread(orden, porcentaje) %>%
        mutate(odds = round(sinorder / (1 - sinorder), 2)) %>%
        select(-diocese_id)

    DT::datatable(mostrar, filter = 'top',
                      options = list(pageLength = 50))
})

output$dioc_sec_vs_nosec_edm_mapa <-
    renderLeaflet({
        mostrar <- dioc_sec_vs_nosec_edm %>%
            mutate(porcentaje = round(porcentaje, 2)) %>%
            #select(diocese_id, diocese_name, orden, porcentaje) %>%
            spread(orden, porcentaje) %>%
            select(-diocese_id)

        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             radius = ~conorder,
                             label = ~htmlEscape(paste0(diocese_name, "( ", conorder, "% )")))})

### Para el tab individuales
# cogemos algunos datos de otros df q ya están cargados para no tener
# q hacer abragen a la base de datos

# ATENCIÓN: hay q tener en cuenta que el pickerInput seleccionar_diocs
# realmente tiene los ids aunque gracias a choicesOpt muestra los
# nombres. 

output$nombrediocesis_url  <- renderText({

    d_id <- input$seleccionar_diocs
        
    j <- bis_per_dioc %>%
        filter(diocese_id == d_id)

    fundacion <- ifelse(j$foundation == 0, "--", as.character(j$foundation))

    paste0(j$diocese_name, ", ", j$country, " (", j$url, ")",
           ". Fundada: ",
           fundacion)
})

output$ind_total_obispos_emd  <- renderValueBox({
    d_id <- input$seleccionar_diocs
    j <- bis_per_dioc %>%
        filter(diocese_id == d_id)

    valueBox(
        j$total,
        "Total obispos (1200-1800)", width = 3,
        icon = icon("users", lib = "font-awesome"))
})

output$ind_total_obispos_edm  <- renderValueBox({

    ## atneción: esto no está completado! esto lo mismo que lo anterior
    d_id <- input$seleccionar_diocs
    j <- bis_per_dioc %>%
        filter(diocese_id == d_id)

    valueBox(
        j$total,
        "Total obispos (1200-1800)", width = 3,
        icon = icon("users", lib = "font-awesome"))
})

output$ind_tipos_obispos_emd  <- renderDataTable({

    d_id <- input$seleccionar_diocs

    sql <- paste0(getSQL("./sqls/dioc_ind1.sql"), d_id)
    rs <- dbGetQuery(con, sql)

    rs$order_acronym <- factor(rs$order_acronym)

    mostrar <- fct_count(rs$order_acronym, sort = TRUE, prop = TRUE)
    mostrar$p <- round(mostrar$p * 100, 2)
    colnames(mostrar) <- c("Tipo", "Total", "%")
    
    DT::datatable(mostrar,
                  options = list(searching = FALSE, paging = FALSE))
    
})

output$ind_tipos_obispos_edm  <- renderDataTable({

    d_id <- input$seleccionar_diocs

    sql <- paste0(getSQL("./sqls/dioc_ind2.sql"), d_id)
    rs <- dbGetQuery(con, sql)

    rs$order_acronym <- factor(rs$order_acronym)

    mostrar <- fct_count(rs$order_acronym, sort = TRUE, prop = TRUE)
    mostrar$p <- round(mostrar$p * 100, 2)
    colnames(mostrar) <- c("Tipo", "Total", "%")
    
    DT::datatable(mostrar,
                  options = list(searching = FALSE, paging = FALSE))

})

output$ind_dioc_mapa  <- renderLeaflet({

    ## atneción: esto no está completado! esto lo mismo que lo anterior
    d_id <- input$seleccionar_diocs
    j <- bis_per_dioc %>%
        filter(diocese_id == d_id)

    # no sé por qué coño con el setview no me funciona lo de ~longitude, etc.
    # y por tanto tengo que poner los parámetros con j$longitude, etc. 
    leaflet(j) %>%
        setView(j$longitude, j$latitude, zoom = 6) %>% 
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addCircleMarkers(lat = ~latitude, lng= ~longitude,
                         label = ~htmlEscape(paste0(j$diocese_name, " (", ~total, " )")))
})
