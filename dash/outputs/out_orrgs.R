
output$ordenes_listageneral <- renderDataTable({
    mostrar <- ordenes_listageneral

    DT::datatable(mostrar, filter = 'top',
                  options = list(pageLength = 50))
})

output$orrg_consultageneral_edm <- renderDataTable({
    mostrar <- orrg_consultageneral_edm

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 50))
})


output$orrg_presencia_emd <- renderDataTable({

    # no me queda claro cómo se hace con switch, lo hago con ifelse
    if (input$seleccionar_tipo_diocesis == "Todos") {
        mostrar_previo <- orrg_presencia_emd
    }
    else if (input$seleccionar_tipo_diocesis == "Bizancio") {
        mostrar_previo <- orrg_presencia_emd %>%
            filter(country %in% paises_bizancio)
    }
    else if (input$seleccionar_tipo_diocesis == "Extraeuropeas") {
        mostrar_previo <- orrg_presencia_emd %>%
            filter(country %in% paises_noeuropa)
    }
    
    mostrar <- mostrar_previo %>%
        filter(religious_order == input$seleccionar_orrg_presencia) %>%
        select(religious_order, diocese_name, country, total) %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 15))
})

output$orrg_presencia_emd_agregados <- renderDataTable({
    # no me queda claro cómo se hace con switch, lo hago con ifelse
    if (input$seleccionar_tipo_diocesis == "Todos") {
        mostrar_previo <- orrg_presencia_emd
    }
    else if (input$seleccionar_tipo_diocesis == "Bizancio") {
        mostrar_previo <- orrg_presencia_emd %>%
            filter(country %in% paises_bizancio)
    }
    else if (input$seleccionar_tipo_diocesis == "Extraeuropeas") {
        mostrar_previo <- orrg_presencia_emd %>%
            filter(country %in% paises_noeuropa)
    }

    mostrar <- mostrar_previo %>%
        filter(religious_order == input$seleccionar_orrg_presencia) %>%
        select(religious_order, country, total) %>%
        group_by(religious_order, country) %>%
        summarise(totalpais = sum(total)) %>%
        arrange(-totalpais)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 15))
})

output$orrg_presencia_emd_mapa <-
    renderLeaflet({

        # no me queda claro cómo se hace con switch, lo hago con ifelse
        if (input$seleccionar_tipo_diocesis == "Todos") {
            mostrar_previo <- orrg_presencia_emd
        }
        else if (input$seleccionar_tipo_diocesis == "Bizancio") {
            mostrar_previo <- orrg_presencia_emd %>%
                filter(country %in% paises_bizancio)
        }
        else if (input$seleccionar_tipo_diocesis == "Extraeuropeas") {
            mostrar_previo <- orrg_presencia_emd %>%
                filter(country %in% paises_noeuropa)
        }

        leaflet(mostrar_previo) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             label = ~htmlEscape(paste0(diocese_name, " (", total, " )")),
                             radius = ~total,
                             clusterOptions = markerClusterOptions())})

output$orrg_presencia_edm <- renderDataTable({

    # no me queda claro cómo se hace con switch, lo hago con ifelse
    if (input$seleccionar_tipo_diocesis == "Todos") {
        mostrar_previo <- orrg_presencia_edm
    }
    else if (input$seleccionar_tipo_diocesis == "Bizancio") {
        mostrar_previo <- orrg_presencia_edm %>%
            filter(country %in% paises_bizancio)
    }
    else if (input$seleccionar_tipo_diocesis == "Extraeuropeas") {
        mostrar_previo <- orrg_presencia_edm %>%
            filter(country %in% paises_noeuropa)
    }

    mostrar <- mostrar_previo %>%
        filter(religious_order == input$seleccionar_orrg_presencia) %>%
        select(religious_order, diocese_name, country, total) %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 15))
})

output$orrg_presencia_edm_agregados <- renderDataTable({

    # no me queda claro cómo se hace con switch, lo hago con ifelse
    if (input$seleccionar_tipo_diocesis == "Todos") {
        mostrar_previo <- orrg_presencia_edm
    }
    else if (input$seleccionar_tipo_diocesis == "Bizancio") {
        mostrar_previo <- orrg_presencia_edm %>%
            filter(country %in% paises_bizancio)
    }
    else if (input$seleccionar_tipo_diocesis == "Extraeuropeas") {
        mostrar_previo <- orrg_presencia_edm %>%
            filter(country %in% paises_noeuropa)
    }

    mostrar <- mostrar_previo %>%
        filter(religious_order == input$seleccionar_orrg_presencia) %>%
        select(religious_order, country, total) %>%
        group_by(religious_order, country) %>%
        summarise(totalpais = sum(total)) %>%
        arrange(-totalpais)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 15))
})

output$orrg_presencia_edm_mapa <-
    renderLeaflet({

        # no me queda claro cómo se hace con switch, lo hago con ifelse
        if (input$seleccionar_tipo_diocesis == "Todos") {
            mostrar_previo <- orrg_presencia_edm
        }
        else if (input$seleccionar_tipo_diocesis == "Bizancio") {
            mostrar_previo <- orrg_presencia_edm %>%
                filter(country %in% paises_bizancio)
        }
        else if (input$seleccionar_tipo_diocesis == "Extraeuropeas") {
            mostrar_previo <- orrg_presencia_edm %>%
                filter(country %in% paises_noeuropa)
        }

        mostrar <- mostrar_previo %>%
            filter(religious_order == input$seleccionar_orrg_presencia)
            
        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             label = ~htmlEscape(paste0(diocese_name, " (", total, " )")),
                             radius = ~total,
                             clusterOptions = markerClusterOptions())})


output$ops_periplos_emd <- renderDataTable({
    mostrar <- ops_periplos_emd

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 50))
})

output$ops_periplos_edm <- renderDataTable({
    mostrar <- ops_periplos_edm

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 50))
})

output$periplos_clusters_edm <- renderDataTable({
    mostrar <- periplos_clusters_edm %>%
        filter(dioc_id_a == input$seleccionar_diocs_periplos_clusters)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 50))
})

output$periplos_clusters_edm_mapa <-
    renderLeaflet({
        mostrar <- periplos_clusters_edm %>%
            filter(dioc_id_a == input$seleccionar_diocs_periplos_clusters)
        
        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addMarkers(lat = ~lat_a, lng= ~long_a,
                       label = ~htmlEscape(diocesis_a)) %>%
            addCircleMarkers(lat = ~lat_b, lng= ~long_b,
                             label = ~htmlEscape(diocesis_b))})
output$periplos_clusters_edm_ops_totales <- renderDataTable({
    mostrar <- periplos_clusters_edm_ops %>%
        group_by(diocesis_a) %>%
        summarise(total = n()) %>%
        arrange(-total)
    
    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 10))
})

output$periplos_clusters_edm_ops <- renderDataTable({
    mostrar <- periplos_clusters_edm_ops %>%
        filter(dioc_id_a == input$seleccionar_diocs_periplos_clusters_ops)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 50))
})

output$periplos_clusters_edm_ops_mapa <-
    renderLeaflet({
        mostrar <- periplos_clusters_edm_ops %>%
            filter(dioc_id_a == input$seleccionar_diocs_periplos_clusters_ops)
        
        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addMarkers(lat = ~lat_a, lng= ~long_a,
                       label = ~htmlEscape(diocesis_a)) %>%
            addCircleMarkers(lat = ~lat_b, lng= ~long_b,
                             label = ~htmlEscape(diocesis_b))})

output$orrg_series_general <- renderPlot({

    # lo primero que hay q decidir es si es con los OFMs juntos o separados
    # luego seleccionamos las fechas. Esto dtf está cutre pq repetimos el código!
    if (input$seleccionar_series_ofms == "Separados") {
        if (input$seleccionar_series_rango == "1200-1800") {
        mostrar_previo <- orrg_series_emd_general
        fechainicio = "1200-01-01"
        fechafin = "1800-01-01"
        }
        else {
            mostrar_previo <- orrg_series_edm_general
            fechainicio = "1500-01-01"
            fechafin = "1800-01-01"
        }
    }
    else {
        if (input$seleccionar_series_rango == "1200-1800") {
        mostrar_previo <- orrg_series_emd_unido
        fechainicio = "1200-01-01"
        fechafin = "1800-01-01"
        }
        else {
            mostrar_previo <- orrg_series_edm_unido 
            fechainicio = "1500-01-01"
            fechafin = "1800-01-01"
        }
    }

    
    # solo lo cambiamos en el caso de q sean varios países...
    if (is.null(input$seleccionar_orrg_series_paises)) {
        mostrar_previo <- mostrar_previo 
    }
    else  {
        mostrar_previo <- mostrar_previo %>%
            filter(country %in% input$seleccionar_orrg_series_paises)
    }

    # y ahora con esto tenemos q completar los años porque realmente no lo están!
    dffinal = NULL
    for (i in input$seleccionar_orrg_series) {

        dftemp <- mostrar_previo %>%
            filter(order_acronym == i)

        dftemp  <- dftemp %>%
            mutate(ano = as.Date(serie)) %>%
            complete(ano = seq.Date(as.Date(fechainicio), as.Date(fechafin), by="year"),
                     order_acronym = i)

        dffinal = rbind(dffinal, dftemp)
        }

    # esto es sin países por lo q hay q colapsar 
    mostrar <- dffinal %>%
        group_by(order_acronym, ano) %>%
        summarise(total = sum(totalobispos))

    ggplot(mostrar, aes( x = ano, y = total, color = order_acronym)) +
        geom_line(size = 1.2) +
        #xlim(fechainicio, fechafin) + 
        theme_light()
})

output$orrg_series_general_porpaises <- renderPlot({

    # lo primero que hay q decidir es si es con los OFMs juntos o
    # separados luego seleccionamos las fechas. Esto dtf está cutre pq
    # repetimos el código dentro del ifelse! Además se repite el código
    # en este renderPlot y en el anteiror!
        if (input$seleccionar_series_ofms == "Separados") {
        if (input$seleccionar_series_rango == "1200-1800") {
        mostrar_previo <- orrg_series_emd_general
        fechainicio = "1200-01-01"
        fechafin = "1800-01-01"
        }
        else {
            mostrar_previo <- orrg_series_edm_general
            fechainicio = "1500-01-01"
            fechafin = "1800-01-01"
        }
    }
    else {
        if (input$seleccionar_series_rango == "1200-1800") {
        mostrar_previo <- orrg_series_emd_unido
        fechainicio = "1200-01-01"
        fechafin = "1800-01-01"
        }
        else {
            mostrar_previo <- orrg_series_edm_unido 
            fechainicio = "1500-01-01"
            fechafin = "1800-01-01"
        }
    }

    # y ahora con esto tenemos q completar los años porque realmente no lo están!
    dffinal = NULL
    for (i in input$seleccionar_orrg_series) {

        dftemp <- mostrar_previo %>%
            filter(order_acronym == i)

        dftemp  <- dftemp %>%
            mutate(ano = as.Date(serie)) %>%
            complete(ano = seq.Date(as.Date(fechainicio), as.Date(fechafin), by="year"),
                     order_acronym = i)

        dffinal = rbind(dffinal, dftemp)
    }

    ggplot(dffinal, aes( x = ano, y = totalobispos, color = order_acronym)) +
        geom_line(size = 1.2) +
        facet_wrap(~country) +
        #xlim(fechainicio, fechafin) + 
        theme_light()
})


output$orrg_motivosfin_edm  <- renderDataTable({
    mostrar <- orrg_motivosfin_edm %>%
        filter(rel_order == input$seleccionar_orrg_motivosfin_edm) %>%
        mutate(porcentaje = round(porcentaje, 2)) %>% 
        arrange(-porcentaje)

    DT::datatable(mostrar)
})

output$ops_monasterioobispado_edm <- renderDataTable({
    mostrar <- ops_mon_obispado_edm %>%
        select(convent, place, country) %>%
        arrange(country)

    DT::datatable(mostrar, filter = 'top',
                  options = list(pageLength = 20))
})

output$ops_monasterioobispado_edm_mapa <-
    renderLeaflet({
        mostrar <- ops_mon_obispado_edm
            
        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             label = ~htmlEscape(paste0(convent, " (", place, " )")))})

output$ops_bizancio <- renderDataTable({
    mostrar <- ops_bizancio %>%
        select(diocesis, country, inicio, fin, duracion)

    DT::datatable(mostrar, filter = 'top',
                  options = list(pageLength = 20))
})

output$ops_bizancio_decadas <- renderPlot({
    mostrar <- ops_bizancio_decadas

    ggplot(mostrar, aes( x = r_from, y = total)) +
        geom_bar(stat = "identity")
})

output$odds_ops_ofms_edm <- renderDataTable({
    mostrar <- odds_ops_ofms_edm %>%
        select(-longitude, -latitude) %>%
        arrange(-odds_op)

    DT::datatable(mostrar, filter = 'top',
                  options = list(pageLength = 20))
})

output$odds_ops_ofms_edm_mapa <-
    renderLeaflet({
        mostrar <- odds_ops_ofms_edm
            
        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             radius = ~odds_op,
                             label = ~htmlEscape(paste0(diocese_name, " (", odds_op, " )")))})

