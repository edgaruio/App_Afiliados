shinyServer(function(input, output){
  
  # output$picture <- renderImage({
  #   return(list(src = "/srv/samba/share/site layouts//Alignment.PNG",contentType = "image/png",alt = "Alignment"))
  # }, deleteFile = FALSE) #where the src is wherever you have the picture
  
  ### ========= PUNTO --------------
  bd_mapa_punto<-eventReactive(input$go7,{
    aux1 <- data.frame(CX = as.numeric(input$CX), CY = as.numeric(input$CY))
    return(aux1)
  })

  bd_persona4 <- eventReactive(input$go7,{
    aux1 <- persona
    aux1$Dist_V <- round(distHaversine(aux1[,c("cx_persona","cy_persona")], bd_mapa_punto()[,c("CX","CY")])/1000,1)
    aux1$Dist_T <- round(distHaversine(aux1[,c("cx_empresa","cy_empresa")], bd_mapa_punto()[,c("CX","CY")])/1000,1)
    aux1 <- aux1 %>% filter(Dist_V<=input$Distancia3 | Dist_T<=input$Distancia3) %>% 
      data.frame()
    return(aux1)
  })
  
  bd_persona4_viven <- reactive({
    aux1 <- bd_persona4()
    aux1 <- aux1 %>% filter(Dist_V<=input$Distancia3) %>% 
      data.frame()
    return(aux1)
  })
  
  bd_persona4_trabajan <- reactive({
    aux1 <- bd_persona4()
    aux1 <- aux1 %>% filter(Dist_T<=input$Distancia3) %>% 
      data.frame()
    return(aux1)
  })

  bd_empresa4 <- eventReactive(input$go7,{
    aux1 <- empresa
    aux1$Dist_V <- round(distHaversine(aux1[,c("cx_empresa","cy_empresa")], bd_mapa_punto()[,c("CX","CY")])/1000,1)
    aux1 <- aux1 %>% filter(Dist_V<input$Distancia3)
  })

  output$Mapapunto <- renderLeaflet({
    aux_punto <- bd_mapa_punto()
    aux_personas <- bd_persona4()

    map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 6, maxZoom = 25))
    map %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addMarkers(data = aux_punto,lng =~ CX, lat =~ CY) %>%
      addCircles(data = aux_punto, lng = ~CX, lat = ~CY, color = "steelblue", radius = input$Distancia3*1000) %>%
      addPolygons(data=localidad, fill = F, stroke = T, color = "navy", group = "Poligonos",weight = 1) %>%
      addPolygons(data=cundi, fill = F, stroke = T, color = "red", group = "Poligonos",weight = 1) %>%
      # addWebGLGPXHeatmap(aux_personas, lng = cx_persona, lat = cy_persona, size = 2000, opacity = 0.9) %>%
      addHeatmap(data = aux_personas, lng = ~cx_persona, lat = ~cy_persona, blur = 5, max = 0.3, radius = 10, intensity = 0.01, 
                 group = "Densidad") %>%
      addLayersControl(
        baseGroups  = c("Poligonos","Densidad"),
        overlayGroups =  c("Agencia de Empleo","Centros de Servicio","Educacion","Supermercados","Medicamentos",
                           "Recreacion y Turismo","Salud","Vivienda"),
        options = layersControlOptions(collapsed = TRUE), position = "bottomleft") %>%
      addMarkers(data=AGENCIA, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsAG, group = "Agencia de Empleo") %>%
      addMarkers(data=CSERVICIOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsCS, group = "Centros de Servicio") %>%
      addMarkers(data=EDUCACION, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsED, group = "Educacion") %>%
      addMarkers(data=SUPERMERCADOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsSP, group = "Supermercados") %>%
      addMarkers(data=MEDICAMENTOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsDR, group = "Medicamentos") %>%
      addMarkers(data=RYT, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsRYT, group = "Recreacion y Turismo") %>%
      addMarkers(data=SALUD, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsSL, group = "Salud") %>%
      addMarkers(data=VIVIENDA, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsVV, group = "Vivienda") %>%
      hideGroup(c("Centros de Servicio","Educacion","Supermercados","Medicamentos","Recreacion y Turismo","Salud","Vivienda")) %>%
      setView(input$CX, input$CY, zoom = 15) %>% 
      addMiniMap(tiles = providers$CartoDB.Positron,toggleDisplay = TRUE)
    # %>% addTitles()
  })

  output$downloadData <- downloadHandler(
    filename = function(){
      paste("Afiliados", Sys.time(), ".csv", sep = "_")
    },
    content = function(file){
      fwrite(bd_persona4() %>%
               select(id_persona,Genero,Edad) %>%
               distinct(),
             file, row.names = F, sep = ";", dec = ",")
    }
  )
  
  output$downloadData_viven <- downloadHandler(
    filename = function(){
      paste("Afiliados_viven", Sys.time(), ".csv", sep = "_")
    },
    content = function(file){
      fwrite(bd_persona4_viven() %>%
               select(id_persona) %>%
               distinct(),
             file, row.names = F, sep = ";", dec = ",")
    }
  )
  
  output$downloadData_trabajan <- downloadHandler(
    filename = function(){
      paste("Afiliados_trabajan", Sys.time(), ".csv", sep = "_")
    },
    content = function(file){
      fwrite(bd_persona4_trabajan() %>%
               select(id_persona) %>%
               distinct(),
             file, row.names = F, sep = ";", dec = ",")
    }
  )
  
  output$info_emp1 <- renderValueBox({
    data_f1<-bd_persona4()
    valueBox(
      value = formatC(length(unique(data_f1$id_persona)),digits = 0, format = "d", big.mark=","),
      subtitle = "Total Afiliados",
      icon = icon("user"),
      color = "blue"
    )
  })
  
  output$info_emp2 <- renderValueBox({
    data_f1<-bd_persona4()
    valueBox(
      value = formatC(sum(data_f1$MunicipioPersona == "BOGOTA D.C.", na.rm = T),digits = 0, format = "d", big.mark=","),
      subtitle = "Empleados viven en Bogotá",
      icon = icon("user"),
      color = "blue"
    )
  })

  output$info_emp3 <- renderValueBox({
    data_f1<-bd_persona4()
    valueBox(
      value = formatC(sum(data_f1$Genero == "F", na.rm = T),digits = 0, format = "d", big.mark=","),
      subtitle = "Mujeres",
      icon = icon("user"),
      color = "blue"
    )
  })

  output$info_emp4 <- renderValueBox({
    data_f1<-bd_persona4()
    valueBox(
      value = formatC(sum(data_f1$Genero == "M", na.rm = T),digits = 0, format = "d", big.mark=","),
      subtitle = "Hombres",
      icon = icon("user"),
      color = "blue"
    )
  })

  output$resumen_punto_afiliado <- DT::renderDataTable({
    resu_afil <- bd_persona4() %>%
      select(Categoria,RangoEdad2) %>%
      group_by(Categoria,RangoEdad2) %>%
      summarise(Freq = n()) %>%
      spread(key = Categoria, value = Freq, fill = 0)
    datatable(resu_afil,options = list(dom="lt", pageLength = 10, lengthChange = FALSE),rownames = F)
  })

  output$resumen_punto_afiliado4 <- DT::renderDataTable({
    resu_afil <- bd_persona4() %>%
      select(Genero,Segmento_poblacional) %>%
      group_by(Genero,Segmento_poblacional) %>%
      summarise(Freq = n()) %>%
      spread(key = Segmento_poblacional, value = Freq, fill = 0)
    datatable(resu_afil,options = list(dom="lt", pageLength = 10, lengthChange = FALSE),rownames = F)
  })

  output$resumen_punto_afiliado2 <- DT::renderDataTable({
    resu_afil <- bd_persona4() %>%
      select(Segmento_poblacional,RangoEdad2) %>%
      group_by(Segmento_poblacional,RangoEdad2) %>%
      summarise(Freq = n()) %>%
      spread(key = Segmento_poblacional, value = Freq, fill = 0)
    datatable(resu_afil,options = list(dom="lt", pageLength = 10, lengthChange = FALSE),rownames = F)
  })

  output$resumen_punto_afiliado3 <- DT::renderDataTable({
    resu_afil <- bd_persona4() %>%
      select(SectorCIIU,Segmento_poblacional) %>%
      group_by(SectorCIIU,Segmento_poblacional) %>%
      summarise(Freq = n()) %>%
      spread(key = Segmento_poblacional, value = Freq, fill = 0)
    datatable(resu_afil,options = list(dom="lt", pageLength = 10, lengthChange = FALSE),rownames = F)
  })

  # output$resumen_afiliado_municipio <- DT::renderDataTable({
  #   resu_afil_loca <- bd_persona4() %>%
  #     select(Categoria,MunicipioPersona) %>%
  #     mutate(Barrio = ifelse(is.na(MunicipioPersona),"SIN INFORMACION",MunicipioPersona)) %>%
  #     group_by(Categoria,MunicipioPersona) %>%
  #     summarise(Freq = n()) %>%
  #     spread(key = Categoria, value = Freq, fill = 0)
  #   datatable(resu_afil_loca,options = list(dom="lt", pageLength = 10, lengthChange = FALSE),rownames = F)
  # })

  
  output$resumen_empresa_actividad <- DT::renderDataTable({
    resu_emp_actividad <- bd_empresa4() %>%
      select(Piramide1,ActividadCIIU) %>%
      group_by(Piramide1,ActividadCIIU) %>%
      summarise(Freq = n()) %>%
      spread(key = Piramide1, value = Freq, fill = 0)
    datatable(resu_emp_actividad,options = list(dom="lt", pageLength = 10, lengthChange = FALSE),rownames = F)
  })

  ### ========= PUNTOS --------------

  data<-reactive({
    req(input$plus)
    data <- fread(input$plus$datapath, encoding = "UTF-8", na.strings = c(""))
  })

  # bd_calificada <- eventReactive(input$go2,{
  #   puntos <- data()
  #   n_puntos <- nrow(puntos)
  #   cedulas <- data.frame(id_persona = character())
  # 
  #   for (i in 1:n_puntos){
  #     consulta <- persona %>%
  #       mutate(Dis_v = distHaversine(cbind(cx_persona,cy_persona), puntos[i,c('CX','CY')])/1000,
  #              Dis_t = distHaversine(cbind(cx_empresa,cy_empresa), puntos[i,c('CX','CY')])/1000) %>%
  #       filter(Dis_v <= input$Distancia2 | Dis_t <= input$Distancia2) %>%
  #       select(id_persona) %>%
  #       distinct()
  # 
  #     print(paste("Punto", i, "de", n_puntos))
  # 
  #     cedulas <- bind_rows(cedulas,consulta)
  #   }
  #   cedulas <- cedulas %>% distinct()
  # }, ignoreNULL = F)
  
  bd_calificada <- eventReactive(input$go2,{
    n_puntos <- nrow(data())
    puntos <- data() %>% 
      mutate(puntos = as.character(1:n_puntos))
    cedulas <- data.frame(id_persona = c(),
                          Dis_v = c(),
                          Dis_t = c())

    for (i in 1:n_puntos){
      consulta <- persona %>%
        mutate(Dis_v = distHaversine(cbind(cx_persona,cy_persona), puntos[i,c('CX','CY')])/1000,
               Dis_t = distHaversine(cbind(cx_empresa,cy_empresa), puntos[i,c('CX','CY')])/1000) %>%
        filter(Dis_v <= input$Distancia2 | Dis_t <= input$Distancia2) %>%
        select(id_persona, Dis_v, Dis_t) %>%
        mutate(Punto = as.character(i)) %>% 
        left_join(puntos %>% dplyr::select(NOMBRE, puntos), by = c("Punto"="puntos"))

      print(paste("Punto", i, "de", n_puntos))

      cedulas <- bind_rows(cedulas,consulta)
    }
    cedulas <- cedulas %>% distinct()
  })
  
  output$Mapapunto2 <- renderLeaflet({
    
    aux_puntos <- data()
    
    map <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 8, maxZoom = 16))
    map %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addMarkers(data = aux_puntos, lng =~ CX, lat =~ CY) %>%
      addCircles(data = aux_puntos, lng = ~CX, lat = ~CY, color = "steelblue", radius = input$Distancia2*1000) %>%
      addPolygons(data=localidad, fill = F, stroke = T, color = "navy", group = "Poligonos",weight = 1) %>%
      addPolygons(data=cundi, fill = F, stroke = T, color = "red", group = "Poligonos",weight = 1) %>%
      addLayersControl(
        # baseGroups  = c("Poligonos", "Densidad"),
        overlayGroups =  c("Agencia de Empleo","Centros de Servicio","Educacion","Supermercados","Medicamentos",
                           "Recreacion y Turismo","Salud","Vivienda"),
        options = layersControlOptions(collapsed = TRUE), position = "bottomleft") %>%
      addMarkers(data=AGENCIA, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsAG, group = "Agencia de Empleo") %>%
      addMarkers(data=CSERVICIOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsCS, group = "Centros de Servicio") %>%
      addMarkers(data=EDUCACION, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsED, group = "Educacion") %>%
      addMarkers(data=SUPERMERCADOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsSP, group = "Supermercados") %>%
      addMarkers(data=MEDICAMENTOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsDR, group = "Medicamentos") %>%
      addMarkers(data=RYT, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsRYT, group = "Recreacion y Turismo") %>%
      addMarkers(data=SALUD, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsSL, group = "Salud") %>%
      addMarkers(data=VIVIENDA, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsVV, group = "Vivienda") %>%
      hideGroup(c("Centros de Servicio","Educacion","Supermercados","Medicamentos","Recreacion y Turismo","Salud","Vivienda")) %>% 
      setView(-74.10858316, 4.62808999, zoom = 13) %>% 
      addMiniMap(tiles = providers$CartoDB.Positron,toggleDisplay = TRUE)
  })
  
  output$Preview <- renderDataTable({
    datatable(data(), options=list(dom="t",searching=F, scrollX = TRUE), rownames=F)
  })

  calif<-reactive({
    x=bd_calificada()
    step1 <- x %>%
      left_join(persona %>%
                select(id_persona,Edad,Genero,RangoEdad2,Segmento_poblacional,Categoria),
                by = "id_persona")
    return(step1)
  })
  
  output$downloadData2 <- downloadHandler(
    filename = function(){
      paste("Afiliados_Obj", Sys.time(), ".csv", sep = "_")
    },
    content = function(file){
      fwrite(calif(), file, row.names = F, sep = ";", dec = ",")
    }
  )

  output$Calificada <- renderDataTable({
    datatable(calif(),
              filter = 'top',
              options = list(dom="lt", pageLength = 10, lengthMenu = c(10, 50, 100, 200, 500)),
              rownames = F)
  })
  
  calif_viven_2 <-reactive({
    x=bd_calificada() %>% 
      select(-Dis_t) %>% 
      arrange(id_persona, Dis_v) %>%
      group_by(id_persona) %>%
      filter(row_number()==1) %>%
      data.frame() %>% 
      filter(Dis_v <= input$Distancia2)
    step1 <- x %>%
      left_join(persona %>%
                  select(id_persona,Edad,Genero,RangoEdad2,Segmento_poblacional,Categoria),
                by = "id_persona")
    return(step1)
  })
  
  output$downloadData_viven2 <- downloadHandler(
    filename = function(){
      paste("Afiliados_viven", Sys.time(), ".csv", sep = "_")
    },
    content = function(file){
      fwrite(calif_viven_2(), file, row.names = F, sep = ";", dec = ",")
    }
  )
  
  calif_trabajan_2 <-reactive({
    x=bd_calificada() %>% 
      select(-Dis_v) %>% 
      arrange(id_persona, Dis_t) %>%
      group_by(id_persona) %>%
      filter(row_number()==1) %>%
      data.frame() %>% 
      filter(Dis_t <= input$Distancia2)
    step1 <- x %>%
      left_join(persona %>%
                  select(id_persona,Edad,Genero,RangoEdad2,Segmento_poblacional,Categoria),
                by = "id_persona")
    return(step1)
  })
  
  output$downloadData_trabajan2 <- downloadHandler(
    filename = function(){
      paste("Afiliados_trabajan", Sys.time(), ".csv", sep = "_")
    },
    content = function(file){
      fwrite(calif_trabajan_2(), file, row.names = F, sep = ";", dec = ",")
    }
  )
  
  output$info_emp1_p <- renderValueBox({
    data_f1<-bd_calificada()
    valueBox(
      value = formatC(nrow(data_f1),digits = 0, format = "d", big.mark=","),
      subtitle = "Total Afiliados",
      icon = icon("user"),
      color = "blue"
    )
  })
  
  output$info_emp2_p <- renderValueBox({
    data_f1<-calif_viven_2()
    valueBox(
      value = formatC(length(unique(data_f1$id_persona)),digits = 0, format = "d", big.mark=","),
      subtitle = "Afiliados que viven",
      icon = icon("user"),
      color = "blue"
    )
  })
  
  output$info_emp3_p <- renderValueBox({
    data_f1<-calif_trabajan_2()
    valueBox(
      value = formatC(length(unique(data_f1$id_persona)),digits = 0, format = "d", big.mark=","),
      subtitle = "Afiliados que trabajan",
      icon = icon("user"),
      color = "blue"
    )
  })
  
  ### ========= EMPRESA --------------

  data_f_emp <- eventReactive(input$go,{
    id2 = as.character(paste0(input$tipodoc,input$xnit_empresa))
    data_f_ <- persona %>%
      filter(id_empresa == id2)
    return(data_f_)
  })

  output$Mapaafiliados <- renderLeaflet({

    aux_info <- data_f_emp() %>%
      filter(!is.na(cx_persona))

    map2 <- leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom = 4, maxZoom = 20))

    map2 %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addCircleMarkers(data=aux_info, lng =~cx_persona, lat =~cy_persona, fillOpacity = 0.5,radius = 2, stroke=FALSE, 
                       color = "darkgreen") %>%
      addPolygons(data=localidad, fill = F, stroke = T, color = "navy", group = "Poligonos",weight = 1) %>%
      addPolygons(data=cundi, fill = F, stroke = T, color = "red", group = "Poligonos",weight = 1) %>%
      addHeatmap(data = aux_info, lng = ~cx_persona, lat = ~cy_persona, blur = 5, max = 0.3, radius = 10, intensity = 0.01, 
                 group = "Densidad") %>%
      addLayersControl(
        baseGroups  = c("Poligonos", "Densidad"),
        overlayGroups =  c("Agencia de Empleo","Centros de Servicio","Educacion","Supermercados","Medicamentos",
                           "Recreacion y Turismo","Salud","Vivienda"),
        options = layersControlOptions(collapsed = TRUE), position = "bottomleft") %>%
      addMarkers(data=AGENCIA, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsAG, group = "Agencia de Empleo") %>%
      addMarkers(data=CSERVICIOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsCS, group = "Centros de Servicio") %>%
      addMarkers(data=EDUCACION, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsED, group = "Educacion") %>%
      addMarkers(data=SUPERMERCADOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsSP, group = "Supermercados") %>%
      addMarkers(data=MEDICAMENTOS, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsDR, group = "Medicamentos") %>%
      addMarkers(data=RYT, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsRYT, group = "Recreacion y Turismo") %>%
      addMarkers(data=SALUD, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsSL, group = "Salud") %>%
      addMarkers(data=VIVIENDA, lng =~CX, lat =~CY, popup = ~as.character(NOMBRE),label = ~as.character(paste(NOMBRE)),
                 icon = leafIconsVV, group = "Vivienda") %>%
      hideGroup(c("Centros de Servicio","Educacion","Supermercados","Medicamentos","Recreacion y Turismo","Salud","Vivienda")) %>% 
      setView(-74.078773, 4.64144452, zoom = 11) %>% 
      addMiniMap(tiles = providers$CartoDB.Positron,toggleDisplay = TRUE)
    })


  
  
})
