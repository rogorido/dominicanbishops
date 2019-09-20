

output$cnf_ordenes <- renderDataTable({
    mostrar <- cnf_ordenes %>%
        select(diocese_name, religious_order, total) %>%
        filter(religious_order == input$seleccionar_orrg_confe) %>%
        arrange(-total)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 15))
})

output$cnf_ordenes_mapa <-
    renderLeaflet({
        mostrar <- cnf_ordenes %>%
            filter(religious_order == input$seleccionar_orrg_confe)

        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             radius = ~total,
                             label = ~htmlEscape(paste0(diocese_name, "(", total, ")")))
    })

output$cnf_ordenes_sumaanos <- renderDataTable({
    mostrar <- cnf_ordenes_sumaanos %>%
        select(diocese_name, religious_order, duracion) %>%
        filter(religious_order == input$seleccionar_orrg_confe_sumaanos) %>%
        arrange(-duracion)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 15))
})

output$cnf_ordenes_sumaanos_paises <- renderDataTable({
    mostrar <- cnf_ordenes_sumaanos %>%
        filter(religious_order == input$seleccionar_orrg_confe_sumaanos) %>%
        group_by(religious_order, pais) %>%
        summarise(total_duracion = sum(duracion)) %>%
        select(pais, total_duracion) %>% # no sé por qué esto no funciona bien...
        arrange(-total_duracion)

    DT::datatable(mostrar, filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 15))
})


output$cnf_ordenes_sumaanos_mapa <-
    renderLeaflet({
        mostrar <- cnf_ordenes_sumaanos %>%
            filter(religious_order == input$seleccionar_orrg_confe_sumaanos)

        leaflet(mostrar) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude,
                             radius = ~duracion,
                             label = ~htmlEscape(paste0(diocese_name, "(", duracion, ")")))
    })

