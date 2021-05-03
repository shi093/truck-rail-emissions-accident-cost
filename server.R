server <- shinyServer(
  
  function(input, output, session) {
    zone.rg <- readOGR(dsn="faf4_zone2.shp",layer="faf4_zone2", encoding = "UTF-8")
    
    od_mode_vol <- read.csv(file = 'od_mode_vol.csv')
    
    centroid <- read.csv(file = 'centroid.csv')
    
    selected_zone <- reactive({
      p <- input$Zone_shape_click
      subset(centroid, id==p$id )
    })
    
    click_count <- 0
    type <- 0
    origin <- ""
    dest <- ""
    origin_id <- 0
    dest_id <- 0
    
    selected_od <- reactive({
      p <- input$Zone_shape_click
      
      selected <- subset(centroid, id==p$id )
      od_pair <- data.frame()
      if (type ==0 ){
        origin <<- selected$name
        dest <<- ""
        origin_id <<- selected$id
        dest_id <<- 0
      }
      
      if (type == 1){
        dest_id <<- selected$id
        dest <<- selected$name
        od_pair <- data.frame(origin, origin_id, dest, dest_id)
        colnames(od_pair)<- c("origin", "origin_id", "dest", "dest_id")
      }
      od_pair
      
    })
    
    output$Zone <- renderLeaflet({
      zone_labels <- sprintf(
        "<strong>%s</strong><br/>",
        paste(zone.rg$id, "--", zone.rg$name, sep='')
      ) %>% lapply(htmltools::HTML)
      
      m<-leaflet() %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Default Maptile",
                         options = providerTileOptions(noWrap = TRUE))%>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Maptile") %>%
        setView(lng = -95.0491410487803, lat = 38.8977674296551, zoom = 4)%>%
        addLayersControl(
          baseGroups = c("Default Maptile", "Satellite Maptile"),
          options = layersControlOptions(collapsed = TRUE)
        )%>%
        addPolygons(data=zone.rg, col="black", weight = 1, layerId = ~id, label = zone_labels, 
                    highlight = highlightOptions(color = "blue",weight = 2, bringToFront = F, opacity = 0.7))
      
    })
    
    observe({
      p <- input$Zone_shape_click
      if (is.null(p))
        return()
      
      m2<-leafletProxy("Zone", session = session)
      
      zone_labels <- sprintf(
        "<strong>%s</strong><br/>",
        paste(centroid$id, "--", centroid$name, sep='')
      ) %>% lapply(htmltools::HTML)
      
      selected <- selected_zone()
      selected_zone_labels <- sprintf(
        "<strong>%s</strong><br/>",
        paste(selected$id, "--", selected$name, sep='')
      ) %>% lapply(htmltools::HTML)
      
      type <<- click_count%%2
      if (type ==0 ){
        m2 %>% clearMarkers()%>%
          addCircleMarkers(data=selected, radius=6, color="green", lng =~x, lat =~y, stroke=FALSE, label = selected_zone_labels,
                           fillOpacity=1, layerId = ~id) 
      }
      
      if (type == 1){
        m2 %>% 
          addCircleMarkers(data=selected, radius=6, color="red", lng =~x, lat =~y, stroke=FALSE, label = selected_zone_labels,
                           fillOpacity=1, layerId = ~id) 
      }
      click_count <<- click_count+1
    })
    
    output$od_info <- renderText({ 
      p <- input$Zone_shape_click
      
      selected <- subset(centroid, id==p$id )
      
      if (type ==0 ){
        origin <<- selected$name
        dest <<- ""
        origin_id <<- selected$id
        dest_id <<- 0
      }
      
      if (type == 1){
        dest_id <<- selected$id
        dest <<- selected$name
      }
      
      paste(
        "<strong> <span style = \'font-weight: 700;\'> Origin:            </span> </strong> 
        <strong> <span style = \'font-weight: 500;\'> ",origin, "</span> </strong>
          <br>",
        "<strong> <span style = \'font-weight: 700;\'> Destination:       </span> </strong> 
        <strong> <span style = \'font-weight: 500;\'> ",dest, "</span> </strong> 
          <br>"
        ,sep = '')
    })
    
    output$od_vol <- DT::renderDataTable(server = FALSE,{
      vol<-data.frame()
      selected <- selected_od()
      
      if (length(selected)){
        vol <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id & mode %in% c('Truck', 'Rail'))%>%
          select(mode, tons, tmiles, dist)
        
        vol$tons <- format(round(vol$tons, 4), big.mark=",")
        vol$tmiles <- format(round(vol$tmiles, 4), big.mark=",")
        vol$dist <- format(round(vol$dist, 2), big.mark=",")
        colnames(vol) <- c('Mode', 'Tons (000s)', 'Ton-Miles (Millions)', 'Average Distance (Miles)')
      }
      vol
    }, 
    rownames = FALSE,  class="compact", width="80%", 
    options = list(paging = FALSE, searching = FALSE, ordering=F, dom='t',columnDefs = list(list(className = 'dt-left', targets = 0:3)))
    )
    
    output$od_total <- renderText({ 
      total_tons <- 0
      total_ton_miles <- 0
      total_value <- 0
      
      selected <- selected_od()
      
      if (length(selected)){
        df <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id)%>%
          select(tons, tmiles, value) %>%
          summarize(tons=sum(tons), tmiles = sum(tmiles), value = sum(value))
        total_tons <- format(round(df$tons, 2), big.mark=",")
        total_ton_miles <- format(round(df$tmiles, 2), big.mark=",")
        total_value <- format(round(df$value, 2), big.mark=",")
      }
      
      paste(
        "<strong> <span style = \'font-weight: 700;\'> By all modes for the selected OD </span> </strong> 
          <br><br>",
        "<strong> <span style = \'font-weight: 500;\'> Total Tons (000s):            ",total_tons, "</span> </strong> 
          <br>",
        "<strong> <span style = \'font-weight: 500;\'> Total Ton-Miles (Millions):       ",total_ton_miles, "</span> </strong> 
          <br>",
        "<strong> <span style = \'font-weight: 500;\'> Total Value (Millions):       ",total_value, "</span> </strong> 
          <br>"
        ,sep = '')
      
    })
    
    output$od_pie <- renderPlotly({
      
      m <- list(l = 3, r = 10, b = 30, t = 80, pad = 4)
      selected <- selected_od()
      
      if (length(selected)){
        df_sub <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id) %>%
          select(mode, tons)
        if(nrow(df_sub)){
          colnames(df_sub) <- c("Mode", "Tons") 
          setDT(df_sub, keep.rownames = TRUE)[]
          p <- df_sub%>% 
            plot_ly(labels = ~Mode, values = ~round(Tons, 2),
                    width = 380, height = 350) %>%
            add_pie(hole = 0.4)%>%
            layout(title = "Weight by Mode", 
                   font = list(family='Arial', size = 11), margin = m,
                   showlegend = T, autosize = F, 
                   legend = list(orientation = 'h', x=0, font = list( family = 'Arial', size = 10)),
                   paper_bgcolor='transparent',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        }
      }
    })
    
    get_total <- reactive({
      
      selected <- selected_od()
      
      if (length(selected)){
        
        df_mode <- subset(od_mode_vol, dms_orig== selected$origin_id & dms_dest == selected$dest_id) %>% select(mode, tons, tmiles, dist)
        df_truck <- subset(df_mode, mode == 'Truck')
        df_rail <- subset(df_mode, mode == 'Rail')
        
        total_truck_tons <- 0
        truck_ton_miles <- 0
        truck_miles <- 0
        total_rail_tons <- 0
        rail_ton_miles <- 0
        rail_miles <- 0
        
        if (nrow(df_truck)){
          total_truck_tons <- df_truck$tons*1000
          truck_ton_miles <- df_truck$tmiles*1000000
          truck_miles <- df_truck$dist
        }
        if (nrow(df_rail)){
          total_rail_tons <- df_rail$tons*1000
          rail_ton_miles <- df_rail$tmiles*1000000
          rail_miles <- df_rail$dist
        }
        
        tt <- data.frame(total_truck_tons, total_rail_tons, truck_ton_miles, rail_ton_miles, truck_miles, rail_miles)
        colnames(tt)<- c("total_truck_tons", "total_rail_tons", "truck_ton_miles", "rail_ton_miles", "truck_miles", "rail_miles")
        tt
      }
    })
    
    get_emission <- reactive ({
      selected <- selected_od()
      if (length(selected)){
        df_tt <- get_total()
        total_truck_tons <- df_tt$total_truck_tons 
        total_rail_tons <- df_tt$total_rail_tons
        rail_ton_miles <- df_tt$rail_ton_miles
        truck_ton_miles <- df_tt$truck_ton_miles
        
        rail_co2 <- rail_ton_miles * input$co2 * input$co2c/907185
        rail_nox <- rail_ton_miles * input$nox * input$noxc/907185
        rail_pm25 <- rail_ton_miles * input$pm25 * input$pm25c/907185
        rail_voc <- rail_ton_miles * input$voc * input$vocc/907185
        rail_so2 <- rail_ton_miles * input$so2 * input$so2c/907185
        Emissions <- c('CO2', 'Nox', 'PM2.5', 'VOC', 'SO2')
        Rail <- c(rail_co2, rail_nox, rail_pm25, rail_voc, rail_so2)
        
        truck_co2 <- truck_ton_miles * input$co2t * input$co2c/907185
        truck_nox <- truck_ton_miles * input$noxt * input$noxc/907185
        truck_pm25 <- truck_ton_miles * input$pm25t * input$pm25c/907185
        truck_voc <- truck_ton_miles * input$voct * input$vocc/907185
        truck_so2 <- truck_ton_miles * input$so2t * input$so2c/907185
        Truck <- c(truck_co2, truck_nox, truck_pm25, truck_voc, truck_so2)
        df <- data.frame(Emissions, Rail, Truck)
      }
    })
    
    get_safety <- reactive({
      selected <- selected_od()
      if (length(selected)){
        df_tt <- get_total()
        total_truck_tons <- df_tt$total_truck_tons 
        total_rail_tons <- df_tt$total_rail_tons
        rail_ton_miles <- df_tt$rail_ton_miles
        truck_ton_miles <- df_tt$truck_ton_miles
        
        rail_fatality <- rail_ton_miles/1000 * input$rail_fatality * input$cost_fatality/1000000
        rail_injury <- rail_ton_miles/1000 * input$rail_injury * input$cost_injury/1000000
        Events <- c('Fatality', 'Injury')
        Rail <- c(rail_fatality, rail_injury)
        
        truck_fatality <- truck_ton_miles/1000 * input$truck_fatality * input$cost_fatality/1000000
        truck_injury <- truck_ton_miles/1000 * input$truck_injury * input$cost_injury/1000000
        Truck <- c(truck_fatality, truck_injury)
        df <- data.frame(Events, Rail, Truck)
      }
    })
    
    output$safety_chart <- renderPlotly({
      df <- get_safety()
      if (length(df)){
        fig <- plot_ly(df, x = ~Events, y = ~Rail, type = 'bar', name = 'Rail Accident Costs')
        fig <- fig %>% add_trace(y = ~Truck, name = 'Truck Accident Costs')
        fig <- fig %>% layout(yaxis = list(title = 'Costs ($)'), barmode = 'group')
        
        fig
      }
    }) 
    
    output$safety_info <- DT::renderDataTable(server = FALSE,{
      df <- get_safety()
      if(length(df)){
        rail <- sum(df$Rail)
        truck <- sum(df$Truck)
        
        rail <- format(round(rail, 0), big.mark=",")
        truck <- format(round(truck, 0), big.mark=",")
        Accident_Cost <- c('Annual Rail Accident Costs', 'Annual Truck Accident Costs')
        Value_Amount <- c(rail, truck)
        df <- data.frame(Accident_Cost, Value_Amount)
      }
    }, 
    rownames = FALSE,  class="compact", width="80%", 
    options = list(paging = FALSE, searching = FALSE, ordering=F, dom='t',columnDefs = list(list(className = 'dt-left', targets = 0:1)))
    )
    
    output$emission_chart <- renderPlotly({
      df <- get_emission()
      if(length(df)){
        fig <- plot_ly(df, x = ~Emissions, y = ~Rail, type = 'bar', name = 'Rail Emissions Cost')
        fig <- fig %>% add_trace(y = ~Truck, name = 'Truck Emissions Cost')
        fig <- fig %>% layout(yaxis = list(title = 'Costs ($)'), barmode = 'group')
        
        fig
      }
    }) 
    
    output$emission_info <- DT::renderDataTable(server = FALSE,{
      df <- get_emission()
      if (length(df)){
        erail <- sum(df$Rail)
        etruck <- sum(df$Truck)
        
        erail <- format(round(erail, 0), big.mark=",")
        etruck <- format(round(etruck, 0), big.mark=",")
        Emission_Cost <- c('Annual Rail Emissions Costs', 'Annual Truck Emissions Costs')
        Value_Amount <- c(erail, etruck)
        df_emission <- data.frame(Emission_Cost, Value_Amount)
      }
    }, 
    rownames = FALSE,  class="compact", width="80%", 
    options = list(paging = FALSE, searching = FALSE, ordering=F, dom='t',columnDefs = list(list(className = 'dt-left', targets = 0:1)))
    )
    
    get_efficiency <- reactive({
      df_tt <- get_total()
      if(length(df_tt)){
        rail_ton_miles <- df_tt$rail_ton_miles
        truck_ton_miles <- df_tt$truck_ton_miles
        
        rail <- rail_ton_miles * input$rail_tm
        truck <- truck_ton_miles * input$truck_tm
        
        df <- data.frame(rail, truck, rail_ton_miles, truck_ton_miles)
      }
    })
    
    output$efficiency_info <- DT::renderDataTable(server = FALSE,{
      df <- get_efficiency()
      if(length(df)){
        rail_tm <- format(round(df$rail_ton_miles/1000, 0), big.mark=",")
        truck_tm <- format(round(df$truck_ton_miles/1000, 0), big.mark=",")
        netval <- df$truck - df$rail
        
        rail <- format(round(df$rail, 0), big.mark=",")
        truck <- format(round(df$truck, 0), big.mark=",")
        netval <- format(round(netval, 0), big.mark=",")
        Value_Name <- c('Annual Rail Ton-Miles (000s)', 'Annual Truck Ton-Miles (000s)', 'Annual Rail Efficiency Benefits ($)', 'Annual Truck Efficiency Benefits ($)', 
                        'Annual Net Transportation Efficiency Benefits ($)')
        Value_Amount <- c(rail_tm, truck_tm, rail, truck, netval)
        df <- data.frame(Value_Name, Value_Amount)
      }
    }, 
    rownames = FALSE,  class="compact", width="80%", 
    options = list(paging = FALSE, searching = FALSE, ordering=F, dom='t',columnDefs = list(list(className = 'dt-left', targets = 0:1)))
    )
    
    output$pavement_info <- DT::renderDataTable(server = FALSE,{
      
      df_tt <- get_total()
      if(length(df_tt)){
        shipments <- df_tt$total_truck_tons/input$carload_ton*input$truck_per_carload
        damage <- df_tt$truck_miles * shipments * input$pavement_cost
        damage_cost <- format(round(damage, 0), big.mark=",")
        truck_shipments <- format(round(shipments, 0), big.mark=",")
        tons <- format(round(df_tt$total_truck_tons/1000, 0), big.mark=",")
        miles <- format(round(df_tt$truck_miles, 0), big.mark=",")
        
        Value_Name <- c('Annual Total Tons (000s)', 'Truck Miles','Annual Truck Shipments', 'Annual Pavement Damage Cost ($)')
        Value_Amount <- c(tons, miles, truck_shipments, damage_cost)
        df <- data.frame(Value_Name, Value_Amount)
      }
    }, 
    rownames = FALSE,  class="compact", width="80%", 
    options = list(paging = FALSE, searching = FALSE, ordering=F, dom='t',columnDefs = list(list(className = 'dt-left', targets = 0:1)))
    )
    
    observeEvent(input$resetEmission, {
      reset("co2")
      reset("nox")
      reset("pm25")
      reset("voc")
      reset("so2")
      reset("co2t")
      reset("noxt")
      reset("pm25t")
      reset("voct")
      reset("so2t")
      reset("co2c")
      reset("noxc")
      reset("pm25c")
      reset("vocc")
      reset("so2c")
    })
    
    observeEvent(input$resetSafety, {
      reset("rail_fatality")
      reset("rail_injury")
      reset("truck_fatality")
      reset("truck_injury")
      reset("cost_fatality")
      reset("cost_injury")
    })
    
    observeEvent(input$resetEfficiency, {
      reset("rail_tm")
      reset("truck_tm")
    })
    
    observeEvent(input$resetPavement, {
      reset("pavement_cost")
      reset("carload_ton")
      reset("truck_per_carload")
    })
    
    
  })