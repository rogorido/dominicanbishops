
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

output$orrg_series_edm_general <- renderPlot({
    # esto es sin países por lo q hay q colapsar 
    mostrar <- orrg_series_edm_general %>%
        filter(order_acronym %in% input$orrg_series_edm_general) %>%
        group_by(order_acronym, serie) %>%
        summarise(total = sum(totalobispos))

    ggplot(mostrar, aes( x = serie, y = total, color = order_acronym)) +
        geom_line(size = 1.2) +
        theme_light()
})

output$orrg_series_edm_general_porpaises <- renderPlot({
    mostrar <- orrg_series_edm_general %>%
        filter(order_acronym %in% input$orrg_series_edm_general)

    ggplot(mostrar, aes( x = serie, y = totalobispos, color = order_acronym)) +
        geom_line(size = 1.2) +
        facet_wrap(~country)
        
})

output$orrg_series_emd_general <- renderPlot({
    # esto es sin países por lo q hay q colapsar 
    mostrar <- orrg_series_emd_general %>%
        filter(order_acronym == input$orrg_series_emd_general) %>%
        group_by(order_acronym, serie) %>%
        summarise(total = sum(totalobispos))

    ggplot(mostrar, aes( x = serie, y = total)) +
        geom_line(size = 1.2)
})

output$orrg_series_emd_general_porpaises <- renderPlot({
    mostrar <- orrg_series_emd_general %>%
        filter(order_acronym == input$orrg_series_emd_general)

    ggplot(mostrar, aes( x = serie, y = totalobispos)) +
        geom_line(size = 1.2) +
        facet_wrap(~country)
        
})

output$orrg_series_emd_unido <- renderPlot({
    # esto es sin países por lo q hay q colapsar 
    mostrar <- orrg_series_emd_unido %>%
        filter(order_acronym == input$orrg_series_emd_unido) %>%
        group_by(order_acronym, serie) %>%
        summarise(total = sum(totalobispos))

    ggplot(mostrar, aes( x = serie, y = total)) +
        geom_line(size = 1.2)
})

output$orrg_series_emd_unido_porpaises <- renderPlot({
    mostrar <- orrg_series_emd_unido %>%
        filter(order_acronym == input$orrg_series_emd_unido)

    ggplot(mostrar, aes( x = serie, y = totalobispos)) +
        geom_line(size = 1.2) +
        facet_wrap(~country)
        
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

