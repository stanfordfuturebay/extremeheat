library(shiny)
library(shinydashboard)
library(rsconnect)
library(leaflet)
library(sf)
library(tidyverse)



ui <- dashboardPage(skin="blue",
                    dashboardHeader(title ="NFO Tree Dashboard"),
                    dashboardSidebar(
                        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "tree_dash.css")),
                        tags$head(tags$style(HTML("
                      .sidebar { height: 90vh; overflow-y: auto; }" ))),
                        div(class="widget",selectizeInput('maplayer', label = "MapLayer:", choices= list("Overall Score", "Building Footprint", "Community",
                                                                                                         "Thermal","Pedestrian Traffic", "NDVI", "Plantable Area")
                        )
                        ),
                        
                        div(class="sidetext",
                            p("Assessment of North Fair Oaks for tree planting for a score from 0 to 1. Higher score indicates an area with higher need of trees (more foot traffic, fewer trees, higher temperatures, etc.)
"),
                            p("See more detailed documentation", a(href="https://docs.google.com/spreadsheets/d/1P2pYE3zuRFTIYsg9EvJwvnquuiSxUHNJDm45hCU4RK0/edit#gid=0", " here.", style="color:blue")),
                            p("Adjust the weight of the sliders below to view overall score. Note: slider weights should total to 1.")),
                        
                        div(class="widget",
                            sliderInput("pedestrian_pre_weight", "Pedestrian Traffic Pre-COVID",
                                        min = 0, max = 1, value = 0.10
                            ),
                            sliderInput("pedestrian_post_weight", "Pedestrian Traffic Post-COVID",
                                        min = 0, max = 1, value = 0
                            ),
                            sliderInput("thermal_weight", "Thermal Data",
                                        min = 0, max = 1, value = 0.10
                            ),
                            sliderInput("thermal_buffer_weight", "Thermal with buffer",
                                        min = 0, max = 1, value = 0.10
                            ),
                            sliderInput("ndvi_weight", "NDVI",
                                        min = 0, max = 1, value = 0.10
                            ),
                            sliderInput("ndvi_buffer_weight", "NDVI with buffer",
                                        min = 0, max = 1, value = 0.10
                            ),
                            sliderInput("plantable_weight", "Potential Plantable Area",
                                        min = 0, max = 1, value = 0.20
                            ),
                            sliderInput("dac_low_income_weight","Disadvantaged Community",
                                        min = 0, max = 1, value = 0.15
                            ),
                            sliderInput("bus_stops_weight", "Bus Stops",
                                        min = 0, max = 1, value = 0.15
                            )
                        ), # div
                        div(class="sidetext",
                            p(strong('Pedestrain'), 'stands for foot traffic a parcel recevies in a month.', 
                              p(strong('NDVI'), ' stands for Normalized Difference Vegetation Index.'),
                              p(strong('Plantable Area'), '  is an estimated amount of front yard plantable area in square feet with a 10 ft buffer from buildings or sidewalk.'),
                              p(strong('Thermal'), ' is the surface temperature of parcels at a 30m resolution.')
                            ),
                        )
                    ), #sidebar
                    
                    dashboardBody(
                        fluidRow(column(12,
                                        box(width=NULL, leafletOutput("map", height = 350)
                                        ),
                        )
                        ),
                        fluidRow(column(12,
                                        box(width=NULL, DT::dataTableOutput("table")
                                        )
                        )
                        )
                    ) #body
) #ui


server <- function(input, output, session) {
    
    googlesheets4::gs4_auth_configure(api_key="AIzaSyDkJJ2brMHagmb2mmOGNXQgkT71QUwRLgU")
    googlesheets4::gs4_deauth()
    data <- googlesheets4::range_speedread("1P2pYE3zuRFTIYsg9EvJwvnquuiSxUHNJDm45hCU4RK0", range="Parcel-Level Outputs [New Version]!B2:AG3128")
    
    nfo_parcels <- readRDS("./data/nfo_parcels_community.rds")
    nfo_bldgs <- readRDS("./data/nfo_bldgs.rds")
    
    data$apn <- as.character(data$apn) 
    
    nfo_parcels_data <-
        data %>% 
        left_join(nfo_parcels %>% dplyr::select(apn = APN, Neighbourhood)) %>%
        sf::st_as_sf() %>% 
        mutate(
            raw_potential_plantable_area = raw_potential_plantable_area %>% as.numeric()
        )
    
    temp_pal <- colorNumeric(
        palette = "Reds",
        domain = nfo_parcels_data$raw_lst_nobuffer
    )
    
    community_pal <- colorFactor(
        "RdYlBu", 
        domain = nfo_parcels_data$Neighbourhood
    )
    
    pedestrian_pal <- colorNumeric(
        palette = "Blues",
        domain = nfo_parcels_data$raw_pedestrian_precovid
    )
    
    ndvi_pal <- colorNumeric(
        palette = "Oranges",
        domain = nfo_parcels_data$raw_ndvi_nobuffer
    )
    
    plantable_pal <- colorNumeric(
        palette = "YlGn",
        domain = nfo_parcels_data$raw_potential_plantable_area
    )
    
    weight_layer <- nfo_parcels %>% 
        dplyr::pull(APN) %>%
        as.character() %>%
        unique()
    
    
    nfo_parcels_data_r <- reactiveValues(data = nfo_parcels_data %>% 
                                             dplyr::select(
                                                 APN = apn, 
                                                 Address = full_address, 
                                                 Neighborhood = Neighbourhood,
                                                 Score = overall_score,
                                                 "Ped. Traffic Pre-COVID" = raw_pedestrian_precovid,
                                                 "Thermal" = raw_lst_nobuffer, 
                                                 "NDVI" = raw_ndvi_nobuffer,
                                                 "Plantable Area" = raw_potential_plantable_area, 
                                                 "DAC & Low Income" = dac_and_lowincome,
                                                 "Low Income" = lowincome, 
                                                 "Near Bus Stop" = bus_stop_proximity,
                                                 "Property Type" = property_type,
                                                 "Exemption Code" = exemption_code 
                                             )
    )
    
    observeEvent({
        input$pedestrian_pre_weight
        input$pedestrian_post_weight
        input$thermal_weight
        input$thermal_buffer_weight
        input$ndvi_weight
        input$ndvi_buffer_weight
        input$plantable_weight
        input$dac_low_income_weight
        input$bus_stops_weight
    }, {
        nfo_parcels_data_r$data$Score <- 
            (nfo_parcels_data$parcel_multiplier * 
                 (nfo_parcels_data$pedestrian_precovid * input$pedestrian_pre_weight + 
                      nfo_parcels_data$pedestrian_postcovid * input$pedestrian_post_weight + 
                      nfo_parcels_data$lst_nobuffer * input$thermal_weight + 
                      nfo_parcels_data$lst_withbuffer * input$thermal_buffer_weight + 
                      nfo_parcels_data$ndvi_nobuffer * input$ndvi_weight + 
                      nfo_parcels_data$ndvi_withbuffer * input$ndvi_buffer_weight + 
                      nfo_parcels_data$potential_plantable_area * input$plantable_weight +
                      nfo_parcels_data$dac_low_income * input$dac_low_income_weight +
                      nfo_parcels_data$proximity_bus_stop * input$bus_stops_weight)
            ) %>% signif(4) * 100
    })
    
    output$table <- DT::renderDT(
        nfo_parcels_data_r$data %>% 
            st_set_geometry(NULL),
        rownames = FALSE,
        selection = 'single',
        filter = 'top', 
        extensions = c('Buttons', 'Scroller'),
        options = list(select = TRUE,
                       # scrollY = 650,
                       # scrollX = 400,
                       scrollX = TRUE,
                       deferRender = TRUE,
                       # scroller = TRUE,
                       buttons = list(
                           list(extend = "excel", exportOptions = list(modifier = list(selected = TRUE))),
                           list(extend = "csv", exportOptions = list(modifier = list(selected = TRUE))),
                           list(extend = "pdf", exportOptions = list(modifier = list(selected = TRUE))),
                           list(extend = 'colvis', targets = 0, visible = FALSE) 
                       ),
                       dom = '<"domB"B>lrtip',
                       autoWidth = TRUE)
    ) 
    
    score_pal <- reactive({
        colorNumeric(palette = "Greens", domain = nfo_parcels_data_r$data$Score)
    })
    
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -122.202, lat = 37.4755, zoom = 14) %>% 
            addControl(actionButton("zoomer","Reset"),position="topright")
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
        # if (input$maplayer == "Parcels"){
        #     leafletProxy("map") %>%
        #         addPolygons(
        #             data = nfo_parcels,
        #             fillColor = "blue",
        #             color = "black",
        #             weight = 0.2,
        #             opacity = 0.2,
        #             fillOpacity = 0.3,
        #             label = ~paste0("APN: ", APN),
        #             layerId = weight_layer,
        #             highlightOptions =
        #                 highlightOptions(
        #                     weight = 2.25,
        #                     opacity = 1
        #                 )
        #         )
        # }
        
        if (input$maplayer == "Community") {
            leafletProxy("map") %>%
                addPolygons(
                    data = nfo_parcels_data,
                    fillColor = ~community_pal(Neighbourhood),
                    color = "black",
                    weight = 0.2,
                    opacity = 0.2,
                    fillOpacity = 0.5,
                    label = ~lapply(paste0("Community: ", Neighbourhood, '</br>', "Address: ", full_address),htmltools::HTML),
                    layerId = weight_layer,
                    highlightOptions =
                        highlightOptions(
                            weight = 2.25,
                            opacity = 1
                        )
                ) %>% 
                addLegend("bottomright", data= nfo_parcels_data, pal = community_pal, values = ~Neighbourhood,
                          title = "Community",
                          opacity = 0.9, layerId = 'legend'
                )
        }
        
        if (input$maplayer == "Building Footprint"){
            leafletProxy("map") %>%
                removeControl(layerId = 'legend') %>%  
                addPolygons(
                    data = nfo_bldgs,
                    fillColor = "green",
                    color = "black",
                    weight = 0.2,
                    opacity = 0.2,
                    fillOpacity = 0.8,
                    label = ~paste0("Building ID: ",osm_id),
                    layerId = weight_layer,
                    highlightOptions =
                        highlightOptions(
                            weight = 2.25,
                            opacity = 1
                        )
                )
        }
        
        if (input$maplayer == "Thermal"){
            leafletProxy("map") %>%
                addPolygons(
                    data = nfo_parcels_data,
                    fillColor = ~temp_pal(raw_lst_nobuffer),
                    color = "black",
                    weight = 0.2,
                    opacity = 0.2,
                    fillOpacity = 0.5,
                    label = ~lapply(paste0("Thermal Score: ", raw_lst_nobuffer, '</br>', "Address: ", full_address),htmltools::HTML),
                    layerId = weight_layer,
                    highlightOptions =
                        highlightOptions(
                            weight = 2.25,
                            opacity = 1
                        )
                ) %>% 
                addLegend("bottomright", data= nfo_parcels_data, pal = temp_pal, values = ~raw_lst_nobuffer,
                          title = "Thermal",
                          opacity = 0.9, layerId = 'legend'
                )
        }
        
        if (input$maplayer == "NDVI"){
            leafletProxy("map") %>%
                addPolygons(
                    data = nfo_parcels_data,
                    fillColor = ~ndvi_pal(raw_ndvi_nobuffer),
                    color = "black",
                    weight = 0.2,
                    opacity = 0.2,
                    fillOpacity = 0.6,
                    label = ~lapply(paste0("NDVI: ", raw_ndvi_nobuffer, '</br>', "Address: ", full_address),htmltools::HTML),
                    layerId = weight_layer,
                    highlightOptions =
                        highlightOptions(
                            weight = 2.25,
                            opacity = 1
                        )
                ) %>% 
                addLegend("bottomright", data= nfo_parcels_data, pal = ndvi_pal, values = ~raw_ndvi_nobuffer,
                          title = "NDVI",
                          opacity = 0.9, layerId = 'legend'
                )
        }
        
        if (input$maplayer == "Pedestrian Traffic"){
            leafletProxy("map") %>%
                addPolygons(
                    data = nfo_parcels_data,
                    fillColor = ~pedestrian_pal(raw_pedestrian_precovid),
                    color = "black",
                    weight = 0.2,
                    opacity = 0.2,
                    fillOpacity = 0.6,
                    label = ~lapply(paste0("Pedestrian Traffic: ", raw_pedestrian_precovid, '</br>', "Address: ", full_address),htmltools::HTML),
                    layerId = weight_layer,
                    highlightOptions =
                        highlightOptions(
                            weight = 2.25,
                            opacity = 1
                        )
                ) %>% 
                addLegend("bottomright", data= nfo_parcels_data, pal = pedestrian_pal, values = ~raw_pedestrian_precovid,
                          title = "Foot traffic",
                          opacity = 0.9, layerId = 'legend'
                )
        }
        
        if (input$maplayer == "Plantable Area"){
            leafletProxy("map") %>%
                addPolygons(
                    data = nfo_parcels_data,
                    fillColor = ~plantable_pal(raw_potential_plantable_area),
                    color = "black",
                    weight = 0.2,
                    opacity = 0.2,
                    fillOpacity = 0.8,
                    label = ~lapply(paste0("Potential Plantable Area: ", raw_potential_plantable_area, '</br>', "Address: ", full_address),htmltools::HTML),
                    layerId = weight_layer,
                    highlightOptions =
                        highlightOptions(
                            weight = 2.25,
                            opacity = 1
                        )
                ) %>% 
                addLegend("bottomright", data= nfo_parcels_data, pal = plantable_pal, values = ~raw_potential_plantable_area,
                          title = "Plantable area (sqft)",
                          opacity = 0.9, layerId = 'legend'
                )
        }
        
        observeEvent(score_pal(), {
            if (input$maplayer == "Overall Score"){
                pal <- score_pal()
                
                leafletProxy("map") %>%
                    addPolygons(
                        data = nfo_parcels_data_r$data,
                        layerId = weight_layer,
                        fillColor = ~pal(Score),
                        color = "black",
                        weight = 0.2,
                        opacity = 0.2,
                        fillOpacity = 0.8,
                        label = ~lapply(paste0("Overall Score: ", Score, '</br>', "Address: ", Address),htmltools::HTML)
                    )  %>% 
                    addLegend("bottomright", data=nfo_parcels_data_r$data, pal = pal, values = ~nfo_parcels_data_r$data$Score,
                              title = "Score",
                              opacity = 0.9, layerId = 'legend'
                    )
            } #if statement
        }) #observe pal
    }) #observe map layer
    
    #pan to parcel from table click    
    observeEvent(input$table_rows_selected,{    
        
        leafletProxy('map') %>% 
            removeShape(layerId = "select")
        
        row <- input$table_rows_selected
        
        if(length(row)){
            leafletProxy('map') %>% 
                addPolygons(
                    data = nfo_parcels_data_r$data[row,],
                    fill = F,
                    color = "black",
                    weight = 2,
                    opacity = 1,
                    label = ~lapply(paste0("Overall Score: ", nfo_parcels_data_r$data[row,]$Score, '</br>', "Address: ", nfo_parcels_data_r$data[row,]$Address), htmltools::HTML),
                    layerId = "select",
                    highlightOptions = 
                        highlightOptions(
                            weight = 2.25,
                            opacity = 1,
                            bringToFront = TRUE
                        )
                ) %>%
                flyTo(
                    lng = nfo_parcels_data_r$data[row,] %>% st_centroid() %>% st_coordinates() %>% .[1],
                    lat = nfo_parcels_data_r$data[row,] %>% st_centroid() %>% st_coordinates() %>% .[2],
                    zoom = 17
                )
        } #if
    }, ignoreInit = T, ignoreNULL = F)
    
    # go to row on table from map clicking
    
    
    observeEvent(input$map_click, {
        
        proxy <- DT::dataTableProxy("table")
        
        leafletProxy('map') %>%
            removeShape(layerId = "select")
        
        click<- input$map_click
        
        selected_APN <-
            st_point(c(click$lng,click$lat)) %>%
            st_sfc() %>%
            st_sf() %>%
            st_set_crs(st_crs(nfo_parcels)) %>%
            nfo_parcels[., ]
        
        if(nrow(selected_APN) == 0)
            return()
        else {
            selected_parcel <- nfo_parcels_data_r$data %>%
                filter(APN == selected_APN[1, ]$APN)
            print(selected_parcel$Address)
            leafletProxy('map') %>%
                addPolygons(
                    data = selected_parcel,
                    fillOpacity = 0,
                    color = "black",
                    weight = 2,
                    opacity = 1,
                    label = selected_APN$Address,
                    layerId = "select",
                    highlightOptions = 
                        highlightOptions(
                            weight = 2.25,
                            opacity = 1,
                            bringToFront = TRUE
                        )
                ) %>%
                flyTo(
                    lng = click$lng,
                    lat = click$lat,
                    zoom = 17
                )
            
            a <- which(nfo_parcels_data_r$data$APN == selected_parcel$APN)
            
            proxy %>%
                DT::selectRows(a) %>%
                DT::selectPage(a %/% 10 + 1)
        }
    })
    
    
    
} #server.R

shinyApp(ui = ui, server = server)
