server <- shinyServer(function(input, output, session){
  
  ampList <- reactive({
    popTH_Amp %>% 
      subset(ProvinceThai == input$provinces) %>%
      select(DistrictThaiShort) %>%
      unlist() %>%
      as.vector() %>%
      sort()
  })
  
  #update amphoe from selected province
  observe({
    updateSelectInput(session, "amphoes", choices = ampList())
  })
  
  #select population table
  rawtblpop <- reactive({
    if(input$levels == "Country"){
      tbl <- popTH_Coun
    }else if(input$levels == "Province"){
      tbl <- popTH_Prov
    }else if(input$levels == "Amphoe"){
      tbl <- popTH_Amp
    }
    return(tbl)
  })
  
  # select shapefile level
  THsp <- reactive({
    if(input$levels == "Country"){
      sp <- THmap
    }else if(input$levels == "Province"){
      sp <- THmapAmp
    }
    else{
      sp <- THmapTam
    }
    return(sp)
  })
  
  # fliter only selected population table
  poptable <- eventReactive(input$go, {
    filtertbl(rawtblpop(), input$levels, input$provinces, input$amphoes)
  })
  
  # fliter only selected shapefile
  map <- eventReactive(input$go, {
    filterSp(THsp(), input$levels, poptable())
  })
  
  #join shapefile with population table
  joindf <- eventReactive(input$go,{
    joinTbl(map(), poptable(), input$levels)
  })
  
  area_text <- reactive({
    if(input$levels == "Country"){
      area <- "Province"
    }
    else if(input$levels == "Province"){
      area <- "DistrictThaiShort"
    }
    else{
      area <- "TambonThaiShort"
    }
    return(area)
  })
  
  fac <- reactive({
    if(input$types == "Total"){
      gender <- "ทั้งหมด"
    }
    else if(input$types == "Male"){
      gender <- "เพศชาย"
    }
    else if(input$types == "Female"){
      gender <- "เพศหญิง"
    }
    return(gender)
  })
  
  output$chmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.VoyagerLabelsUnder) %>%
      setView(100.99, 12.87, zoom = 5)
  })
  
  
  observeEvent(input$go,{
    qpal <- colorNumeric(palette="YlOrRd",
                         domain = as.numeric(joindf()@data[[input$types]]),
                         na.color="#adadad")
    
    selarea <- sym(area_text())
    
    text <- paste(
      "<b>", as.vector(t(joindf()@data[area_text()])),"</b></br>",
      "จำนวนประชากร",
      fac(), ": ", as.numeric(joindf()@data[[input$types]]), " คน</br>", sep = "") %>%
      lapply(htmltools::HTML)
    
    leafletProxy("chmap", data = joindf()) %>% 
      fitBounds(joindf()@bbox[1], joindf()@bbox[2],joindf()@bbox[3], joindf()@bbox[4]) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(fillColor = qpal(as.numeric(joindf()@data[[input$types]])),
                  stroke = TRUE,
                  fillOpacity = 0.7,
                  color = "black",
                  weight = 0.3,
                  smoothFactor = 0.2,
                  label = text,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto")) %>%
      addLegend(pal = qpal,
                title = paste(input$types, "Population"),
                values = joindf()@data[[input$types]],
                opacity = 0.9,
                position = "bottomleft")
  })
  
  
  
  
  observeEvent(input$go, {
    
    output$popTbl <- DT::renderDT(
      DT::datatable(poptable(),
                    extensions = "Buttons",
                    options = list(
                      dom = "Bfrtipl",
                      buttons = list("copy", "print", list(
                        extend = "collection",
                        buttons = c("csv", "excel","pdf"),
                        text = "Export As..."
                      )
                      )
                    )
      )
    )
  })
  
})