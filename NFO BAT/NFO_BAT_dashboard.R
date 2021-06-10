library(shiny)
library(shinydashboard)
library(rsconnect)
library(leaflet)
library(sf)
library(tidyverse)

ui <- dashboardPage(skin="blue",
    dashboardHeader(title ="NFO Block Action Team Dashboard", titleWidth = 350),
    dashboardSidebar(
      div(class="widget",selectizeInput('maplayer', label = "MapLayer:", choices= list("Neighborhoods", 
                                                                                      "Blocks"))),
      # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
      # tags$head(tags$style(HTML("
      #               .sidebar { height: 90vh; overflow-y: auto; }" ))),
      uiOutput("text")
        # 
        #     div(class="sidetext",
        #     p("Assessment of NFO BAT blocks."))

            # div(class="widget",
            # sliderInput("pedestrian_pre_weight", "Pedestrian Traffic Pre-COVID",
            #             min = 0, max = 1, value = 0.3
            #     ),
            # sliderInput("thermal_weight", "Thermal Data",
            #             min = 0, max = 1, value = 0.15
            #     ),
            # sliderInput("thermal_buffer_weight", "Thermal with buffer",
            #             min = 0, max = 1, value = 0.15
            #     ),
            # sliderInput("ndvi_weight", "NDVI",
            #             min = 0, max = 1, value = 0.15
            #     ),
            # sliderInput("ndvi_buffer_weight", "NDVI with buffer",
            #             min = 0, max = 1, value = 0.10
            #     ),
            # sliderInput("plantable_weight", "Potential Plantable Area",
            #             min = 0, max = 1, value = 0.15
            #     )
            #           ), # div
            # div(class="sidetext",
            # p(strong('Pedestrain'), 'stands for foot traffic a parcel recevies in a month.',
            #   p(strong('NDVI'), ' stands for Normalized Difference Vegetation Index.'),
            # p(strong('Plantable Area'), '  is an estimated amount of front yard plantable area in square feet with a 10 ft buffer from buildings or sidewalk.'),
            # p(strong('Thermal'), ' is the surface temperature of parcels at a 30m resolution.')
            #               ),
            #           )
                    ), #sidebar
                    
    dashboardBody(
      tags$style(type = "text/css", "#map {height: calc(100vh - 150px) !important;}"),
        fluidRow(column(12,
                        box(width=NULL, leafletOutput("map")
                        # box(width=NULL, leafletOutput("map", height = 350) #should we change the height?
                            )
                        )
                ) #,
        # fluidRow(column(12,
        #                box(width=NULL, DT::dataTableOutput("table") #should we remove the table?
        #                   )
        #                )
        #         )
    )
)

server <- function(input, output, session) {
  
  googlesheets4::gs4_auth_configure(api_key="AIzaSyDkJJ2brMHagmb2mmOGNXQgkT71QUwRLgU")
  googlesheets4::gs4_deauth()
  
  data <- googlesheets4::range_speedread("1gYXR9YfThGipVZTnJQ806_t_QK66co6YZfudiYsVdLg", range="Main Sheet!A2:Z4975")
  
  data <-
    data %>%
    mutate(Neighborhood = as.character(Neighborhood))
  
  nfo_bat_parcels_updated <- readRDS("./data/nfo_bat_parcels_updated.rds")
  
  nfo_bat_parcels_updated <-  nfo_bat_parcels_updated %>%
    mutate(Neighborhood = str_remove(nfo_bat_parcels_updated$Neighborhood, "Block ")
    )
  
  
  nfo_neighborhood_boundary <- st_read("./data/nfo_boundary.kml") %>%
    select(Name, geometry) %>%
    st_zm(drop = T, what = "ZM") %>% 
    st_transform(4326) %>% 
    st_as_sf()
  
  nfo_neighborhood_boundary <-
    nfo_neighborhood_boundary %>%
    mutate(Name = str_remove(nfo_neighborhood_boundary$Name, "Block ")
           )
  
  nfo_street_blocks_boundary <- readRDS("./data/nfo_street_blocks.rds") %>%
    filter(!GEOID10 %in% c("060816106023009", "060816102031012", "060816115002006", 
                           "060816115002001", "060816106023006"))
  
  neighborhood_vals <-
    data %>%
    group_by(
      Neighborhood
    ) %>% 
    summarise(
      count_ppl = sum(!is.na(Name))
    )

  street_block_vals <-
    data %>%
    group_by(
      Block
    ) %>% 
    summarise(
      count_ppl = sum(!is.na(Name))
    )
  
  nfo_neighborhood_blocks <- nfo_neighborhood_boundary %>% 
    rename(
      Neighborhood = "Name" 
    ) %>% 
    left_join(
      neighborhood_vals,
      by = "Neighborhood"
    )
  
  nfo_street_blocks <- nfo_street_blocks_boundary %>% 
    left_join(
      street_block_vals,
      by = c("GEOID10" = "Block")
    )
  
  community_pal <- colorFactor(
    "Greens", 
    domain = nfo_neighborhood_blocks$count_ppl
  )  
  
  street_block_pal <- colorFactor(
    "Blues", 
    domain = nfo_street_blocks$count_ppl
  )  
  
  # epa_parcels_data_r <- reactiveValues(data = epa_parcels_data %>% 
  #                                        dplyr::select(
  #                                          APN, 
  #                                          Address = full_address,
  #                                          Score,
  #                                          DAC_low_income_multiplier,
  #                                          parcel_multiplier,
  #                                          Pedestrian,
  #                                          Thermal,
  #                                          Thermal_buffer,
  #                                          NDVI,
  #                                          NDVI_buffer,
  #                                          Potential_plantable_area,
  #                                          "Ped. Traffic Pre-COVID" = raw_pedestrian,
  #                                          "Raw Thermal" = raw_thermal,
  #                                          "Raw NDVI" = raw_NDVI,
  #                                          "Plantable Area" = raw_potential_plantable_area, 
  #                                           DAC, 
  #                                          "Low Income" = low_income, 
  #                                          "Property Type" = property_type,
  #                                          "Exemption Code" = exemption_code
  #                                        )
  # )
  
  # observeEvent({
  #   input$pedestrian_pre_weight
  #   input$thermal_weight
  #   input$thermal_buffer_weight
  #   input$ndvi_weight
  #   input$ndvi_buffer_weight
  #   input$plantable_weight
  # }, {
  #   epa_parcels_data_r$data$Score <- 
  #     (epa_parcels_data$DAC_low_income_multiplier * epa_parcels_data$parcel_multiplier * 
  #        (epa_parcels_data$Pedestrian * input$pedestrian_pre_weight + 
  #           epa_parcels_data$Thermal * input$thermal_weight + 
  #           epa_parcels_data$Thermal_buffer * input$thermal_buffer_weight + 
  #           epa_parcels_data$NDVI * input$ndvi_weight + 
  #           epa_parcels_data$NDVI_buffer * input$ndvi_buffer_weight + 
  #           epa_parcels_data$Potential_plantable_area * input$plantable_weight)
  #     ) %>% signif(4) 
  # })
  # 
  # output$table <- DT::renderDT(
  #   epa_parcels_data_r$data %>% 
  #     select(!c(DAC_low_income_multiplier, parcel_multiplier, 
  #               Pedestrian, Thermal, Thermal_buffer, NDVI, NDVI_buffer, 
  #               Potential_plantable_area)) %>%
  #     st_set_geometry(NULL),
  #   rownames = FALSE,
  #   selection = 'single',
  #   filter = 'top', 
  #   extensions = c('Buttons', 'Scroller'),
  #   options = list(select = TRUE,
  #                  # scrollY = 650,
  #                  # scrollX = 400,
  #                  scrollX = TRUE,
  #                  deferRender = TRUE,
  #                  # scroller = TRUE,
  #                  buttons = list(
  #                    list(extend = "excel", exportOptions = list(modifier = list(selected = TRUE))),
  #                    list(extend = "csv", exportOptions = list(modifier = list(selected = TRUE))),
  #                    list(extend = "pdf", exportOptions = list(modifier = list(selected = TRUE))),
  #                    list(extend = 'colvis', targets = 0, visible = FALSE) 
  #                  ),
  #                  dom = '<"domB"B>lrtip',
  #                  autoWidth = TRUE)
  # ) 
  # 
  # score_pal <- reactive({
  #   colorNumeric(palette = "Greens", domain = epa_parcels_data_r$data$Score)
  # })
  
  output$text <- renderUI({
    
    HTML(paste(
      h1("General Info"),
      "More text here.",
      sep = "<br>"
    ))
    
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -122.202, lat = 37.4755, zoom = 14) %>% 
      addControl(actionButton("zoomer","Reset"),position="topright") %>%
      addPolygons(
        data = nfo_neighborhood_blocks,
        fillColor = ~community_pal(count_ppl),
        color = "black",
        weight = 1,
        opacity = 0.2,
        fillOpacity = 0.5,
        label = ~lapply(paste0(count_ppl, " signed up in neighborhood ", Neighborhood),htmltools::HTML),
        layerId = ~Neighborhood,
        group = "Neighborhood",
        highlightOptions =
          highlightOptions(
            weight = 2.25,
            opacity = 1
          )
      ) %>%
      addLabelOnlyMarkers(
        data = nfo_neighborhood_blocks %>% st_centroid(),
        group = "Neighborhood",
        label = ~Neighborhood,
        labelOptions = labelOptions(noHide = T, direction = "center", textOnly = T, textsize = "15px")
      ) %>%
      # addLegend("bottomright", data= nfo_neighborhood_blocks, group = "Neighborhood", pal = community_pal, values = ~count_ppl,
      #           title = "# of Participants",
      #           opacity = 0.9
      # ) %>%
      addPolygons(
        data = nfo_street_blocks,
        fillColor = ~street_block_pal(count_ppl),
        color = "black",
        weight = 1,
        opacity = 0.2,
        fillOpacity = 0.5,
        label = ~lapply(paste0(count_ppl, " signed up in block ", GEOID10),htmltools::HTML),
        layerId = ~GEOID10,
        group = "Block",
        highlightOptions =
          highlightOptions(
            weight = 2.25,
            opacity = 1
          )
      ) #%>%
      # addLegend("bottomright", data= nfo_street_blocks, group = "Block",  pal = street_block_pal, values = ~count_ppl,
      #           title = "# of Participants",
      #           opacity = 0.9
      # ) 
  
  })
  
  observeEvent({
    input$zoomer},{
      leafletProxy('map') %>% 
        removeShape(layerId = "select") %>% 
        flyTo(lng = -122.202, lat = 37.4755, zoom = 14)
    })
  
  observeEvent({
    input$maplayer
  }, {
    if (input$maplayer == "Neighborhoods"){
      leafletProxy("map") %>%
        # clearControls() %>%  
        hideGroup("Neighborhood") %>%
        hideGroup("Block") %>%
        showGroup("Neighborhood") %>% 
        addLegend("bottomright", data= nfo_neighborhood_blocks, layerId = "legend", pal = community_pal, values = ~count_ppl,
                  title = "# of Participants",
                  opacity = 0.9
        )
    }
    if (input$maplayer == "Blocks"){
      leafletProxy("map") %>%
        #clearControls() %>% 
        hideGroup("Neighborhood") %>%
        hideGroup("Block") %>%
        showGroup("Block") %>% 
        addLegend("bottomright", data= nfo_street_blocks, layerId = "legend",  pal = street_block_pal, values = ~count_ppl,
                  title = "# of Participants",
                  opacity = 0.9
        ) 
        
    }
  
  })
    
              
  observeEvent(input$map_click,{
    if(!is.null(input$map_click$lng)){
      leafletProxy('map') %>%
        removeShape(layerId = "select")
    } 
    click <- input$map_click
    if (input$map_shape_click$id %in% nfo_street_blocks$GEOID10)
    { 
      selected <-
        st_point(c(click$lng,click$lat)) %>%
        st_sfc() %>% 
        st_sf() %>% 
        st_set_crs(st_crs(nfo_street_blocks)) %>% 
        nfo_street_blocks[., ]
    } else {
      selected <-
        st_point(c(click$lng,click$lat)) %>%
        st_sfc() %>% 
        st_sf() %>% 
        st_set_crs(st_crs(nfo_neighborhood_blocks)) %>% 
        nfo_neighborhood_blocks[., ]
    }
    if (nrow(selected) == 0) {
      output$text <- renderUI({
        HTML(paste(
          h1("General Info"),
          "More text here.",
          sep = "<br>"
        ))
      })
    } else {
      output$text <- renderUI({
        if (input$map_shape_click$id %in% nfo_street_blocks$GEOID10)
        {
          HTML(paste(
            "Block ",
            h4(selected$GEOID10),
            "Block Captain: ",
            "More text here",
            sep = "<br>"
          ))
        } else
        {
          HTML(paste(
            "Neighborhood ",
            h4(selected$Neighborhood),
            "Neighborhood Captain: ",
            "More text here",
            sep = "<br>"
          ))
        }
      })
    }
  })
  
  # 
  # observeEvent({
  #   input$zoomer},{
  #     leafletProxy('map') %>% 
  #       removeShape(layerId = "select") %>% 
  #       flyTo(lng = -122.202, lat = 37.4755, zoom = 14)
  #   })
  # 
  # observeEvent({
  #   input$maplayer
  # }, {
  #   # if (input$maplayer == "Parcels"){
  #   #     leafletProxy("map") %>%
  #   #         addPolygons(
  #   #             data = epa_parcels,
  #   #             fillColor = "blue",
  #   #             color = "black",
  #   #             weight = 0.2,
  #   #             opacity = 0.2,
  #   #             fillOpacity = 0.3,
  #   #             label = ~paste0("APN: ", APN),
  #   #             layerId = weight_layer,
  #   #             highlightOptions =
  #   #                 highlightOptions(
  #   #                     weight = 2.25,
  #   #                     opacity = 1
  #   #                 )
  #   #         )
  #   # }
  #   
  # 
  #   if (input$maplayer == "Building Footprint"){
  #     leafletProxy("map") %>%
  #       removeControl(layerId = 'legend') %>%  
  #       addPolygons(
  #         data = epa_bldgs,
  #         fillColor = "blue",
  #         color = "black",
  #         weight = 0.2,
  #         opacity = 0.2,
  #         fillOpacity = 0.8,
  #         label = ~paste0("Building ID: ",osm_id),
  #         layerId = weight_layer,
  #         highlightOptions =
  #           highlightOptions(
  #             weight = 2.25,
  #             opacity = 1
  #           )
  #       )
  #   }
  #   
  #   if (input$maplayer == "Thermal"){
  #     leafletProxy("map") %>%
  #       addPolygons(
  #         data = epa_parcels_data,
  #         fillColor = ~temp_pal(raw_thermal),
  #         color = "black",
  #         weight = 0.2,
  #         opacity = 0.2,
  #         fillOpacity = 0.5,
  #         label = ~lapply(paste0("Thermal Score: ", raw_thermal, '</br>', "Address: ", full_address),htmltools::HTML),
  #         layerId = ~APN,
  #         highlightOptions =
  #           highlightOptions(
  #             weight = 2.25,
  #             opacity = 1
  #           )
  #       ) %>% 
  #       addLegend("bottomright", data= epa_parcels_data, pal = temp_pal, values = ~raw_thermal,
  #                 title = "Thermal",
  #                 opacity = 0.9, layerId = 'legend'
  #       )
  #   }
  #   
  #   if (input$maplayer == "NDVI"){
  #     leafletProxy("map") %>%
  #       addPolygons(
  #         data = epa_parcels_data,
  #         fillColor = ~ndvi_pal(raw_NDVI),
  #         color = "black",
  #         weight = 0.2,
  #         opacity = 0.2,
  #         fillOpacity = 0.6,
  #         label = ~lapply(paste0("NDVI: ", raw_NDVI, '</br>', "Address: ", full_address),htmltools::HTML),
  #         layerId = ~APN,
  #         highlightOptions =
  #           highlightOptions(
  #             weight = 2.25,
  #             opacity = 1
  #           )
  #       ) %>% 
  #       addLegend("bottomright", data= epa_parcels_data, pal = ndvi_pal, values = ~raw_NDVI,
  #                 title = "NDVI",
  #                 opacity = 0.9, layerId = 'legend'
  #       )
  #   }
  #   
  #   if (input$maplayer == "Pedestrian Traffic"){
  #     leafletProxy("map") %>%
  #       addPolygons(
  #         data = epa_parcels_data,
  #         fillColor = ~pedestrian_pal(raw_pedestrian),
  #         color = "black",
  #         weight = 0.2,
  #         opacity = 0.2,
  #         fillOpacity = 0.6,
  #         label = ~lapply(paste0("Pedestrian Traffic: ", raw_pedestrian, '</br>', "Address: ", full_address),htmltools::HTML),
  #         layerId = ~APN,
  #         highlightOptions =
  #           highlightOptions(
  #             weight = 2.25,
  #             opacity = 1
  #           )
  #       ) %>% 
  #       addLegend("bottomright", data= epa_parcels_data, pal = pedestrian_pal, values = ~raw_pedestrian,
  #                 title = "Foot traffic",
  #                 opacity = 0.9, layerId = 'legend'
  #       )
  #   }
  #   
  #   if (input$maplayer == "Plantable Area"){
  #     leafletProxy("map") %>%
  #       addPolygons(
  #         data = epa_parcels_data,
  #         fillColor = ~plantable_pal(raw_potential_plantable_area),
  #         color = "black",
  #         weight = 0.2,
  #         opacity = 0.2,
  #         fillOpacity = 0.8,
  #         label = ~lapply(paste0("Potential Plantable Area: ", raw_potential_plantable_area, '</br>', "Address: ", full_address),htmltools::HTML),
  #         layerId = ~APN,
  #         highlightOptions =
  #           highlightOptions(
  #             weight = 2.25,
  #             opacity = 1
  #           )
  #       ) %>% 
  #       addLegend("bottomright", data= epa_parcels_data, pal = plantable_pal, values = ~raw_potential_plantable_area,
  #                 title = "Plantable area (sqft)",
  #                 opacity = 0.9, layerId = 'legend'
  #       )
  #   }
  #   
  #   observeEvent(score_pal(), {
  #     if (input$maplayer == "Overall Score"){
  #       pal <- score_pal()
  #       
  #       leafletProxy("map") %>%
  #         addPolygons(
  #           data = epa_parcels_data_r$data,
  #           group = "Score",
  #           fillColor = ~pal(Score),
  #           color = "black",
  #           weight = 0.2,
  #           opacity = 0.2,
  #           fillOpacity = 0.8,
  #           label = ~paste0("Overall Score: ", Score, ' ', "Address: ", Address)
  #         )  %>% 
  #         addLegend("bottomright", data=epa_parcels_data_r$data, pal = pal, values = ~epa_parcels_data_r$data$Score,
  #                   title = "Score",
  #                   opacity = 0.9, layerId = 'legend'
  #         ) 
  #     } #if statement
  #   }) #observe pal
  # }) #observe map layer
  # 
  # #pan to parcel from table click    
  # observeEvent(input$table_rows_selected,{    
  #   
  #   leafletProxy('map') %>% 
  #     removeShape(layerId = "select")
  #   
  #   row <- input$table_rows_selected
  #   
  #   if(length(row)){
  #     leafletProxy('map') %>% 
  #       addPolygons(
  #         data = epa_parcels_data_r$data[row,],
  #         fill = F,
  #         color = "black",
  #         weight = 2,
  #         opacity = 1,
  #         label = ~lapply(paste0("Overall Score: ", epa_parcels_data_r$data[row,]$Score, '</br>', "Address: ", epa_parcels_data_r$data[row,]$Address), htmltools::HTML),
  #         layerId = "select",
  #         highlightOptions = 
  #           highlightOptions(
  #             weight = 2.25,
  #             opacity = 1,
  #             bringToFront = TRUE
  #           )
  #       ) %>%
  #       flyTo(
  #         lng = epa_parcels_data_r$data[row,] %>% st_centroid() %>% st_coordinates() %>% .[1],
  #         lat = epa_parcels_data_r$data[row,] %>% st_centroid() %>% st_coordinates() %>% .[2],
  #         zoom = 17
  #       )
  #   } #if
  # }, ignoreInit = T, ignoreNULL = F)
  # 
  # # go to row on table from map clicking
  # 
  # 
  # observeEvent(input$map_click, {
  #   
  #   proxy <- DT::dataTableProxy("table")
  #   
  #   leafletProxy('map') %>%
  #     removeShape(layerId = "select")
  #   
  #   click<- input$map_click
  #   
  #   selected_APN <-
  #     st_point(c(click$lng,click$lat)) %>%
  #     st_sfc() %>%
  #     st_sf() %>%
  #     st_set_crs(st_crs(epa_parcels)) %>%
  #     epa_parcels[., ]
  #   
  #   if(nrow(selected_APN) == 0)
  #     return()
  #   else {
  #     selected_parcel <- epa_parcels_data_r$data %>%
  #       filter(APN == selected_APN[1, ]$APN)
  #     print(selected_parcel$Address)
  #     leafletProxy('map') %>%
  #       addPolygons(
  #         data = selected_parcel,
  #         fillOpacity = 0,
  #         color = "black",
  #         weight = 2,
  #         opacity = 1,
  #         label = selected_APN$Address,
  #         layerId = "select",
  #         highlightOptions = 
  #           highlightOptions(
  #             weight = 2.25,
  #             opacity = 1,
  #             bringToFront = TRUE
  #           )
  #       ) %>%
  #       flyTo(
  #         lng = click$lng,
  #         lat = click$lat,
  #         zoom = 17
  #       )
  #     
  #     a <- which(epa_parcels_data_r$data$APN == selected_parcel$APN)
  #     
  #     proxy %>%
  #       DT::selectRows(a) %>%
  #       DT::selectPage(a %/% 10 + 1)
  #   }
  # })
  
  
  
} 

shinyApp(ui = ui, server = server)
