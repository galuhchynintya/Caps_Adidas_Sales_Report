shinyServer(function(input, output) {
  #TABITEM OVERVIEW
  #PLOT1
  output$plot_1 <-  renderPlotly({
    sales_region <- 
      sales %>% 
      filter(season %in% input$input_cat1,    # ganti musim disini
             year %in% input$input_cat2) %>%  # ganti tahun disini
      group_by(Retailer, Region) %>% 
      summarise(Total.Sales = sum(Total.Sales), .groups = 'drop') %>% 
      ungroup() %>% 
      #arrange(-Total.Sales) %>% 
      mutate(label = glue("Retailer: {Retailer}
                      {Region} Sales: {comma(Total.Sales)}"))
    urutan <- sales_region %>% 
      aggregate(Total.Sales ~ Retailer, FUN = sum) %>% 
      arrange(Total.Sales) %>%  
      mutate(Retailer = as.character(Retailer)) %>% 
      select(Retailer) %>% 
      as.list()
    # Membuat visual dengan menambahkan scale_x/y_discreate
    # Pembuatan Visual Statis
    plot1 <- 
      ggplot(data = sales_region, 
             mapping = aes(x=Total.Sales, 
                           y=reorder(Retailer, Total.Sales),
                           text=label)) +
      geom_col(mapping = aes(fill=Region), position = "stack") +
      scale_fill_brewer(palette = "Dark2")+
      
      # menambahkan custom urutan
      scale_y_discrete(limits = urutan$Retailer)+
      scale_x_continuous(labels = unit_format(scale = 1e-6, suffix = "M"),
                         breaks = seq(from = 0,
                                      to = 250e6,
                                      by = 50e6))+
      labs(title = "Total Sales Based on Season",
           x = "Total Sales (in million dollar)",
           y = "Retailer",
           fill="Region")+
      theme_classic()+
      theme(plot.title = element_text(face = "bold", size = 14, hjust = 6),
            panel.background = element_rect(fill = "#ffffff"),
            legend.position = "bottom")
    
    # Mengubah Visual Statis menjadi Interaktif
    ggplotly(plot1, tooltip = 'text') 
  })
  
  
  #PLOT2
  output$plot_2 <- renderPlotly({
    sales_unitsold <- 
      sales%>% 
      filter(season %in% input$input_cat1,
             year %in% input$input_cat2) %>% 
      group_by(Retailer, Region) %>% 
      summarise(Units.Sold = sum(Units.Sold), .groups = 'drop') %>% 
      ungroup() %>%
      mutate(label = glue("Retailer: {Retailer}
                      {Region} Units Sold: {comma(Units.Sold)}"))
    #Membuat agregasi urutan peringkat
    urutan2 <- sales_unitsold %>% 
      aggregate(Units.Sold ~ Retailer, FUN = sum) %>% 
      arrange(Units.Sold) %>%  
      mutate(Retailer = as.character(Retailer)) %>% 
      select(Retailer) %>% 
      as.list()
    # Pembuatan Visual Statis
    plot2 <- 
      ggplot(data = sales_unitsold, 
             mapping = aes(x=Units.Sold, y=reorder(Retailer, Units.Sold), text=label)) +
      geom_col(mapping = aes(fill=Region), position = "stack") +
      scale_fill_brewer(palette = "Dark2")+
      # menambahkan custom urutan
      scale_y_discrete(limits = urutan2$Retailer)+
      scale_x_continuous(labels = unit_format(scale = 1e-3, suffix = "K"),
                         breaks = seq(from = 0,
                                      to = 600e3,
                                      by = 100e3))+
      labs(title = "Total Unit Sold Based on Season",
           x = "Total Unit Sold (in thousand)",
           y = "Retailer",
           fill="Region") +
      theme_classic()+
      theme(plot.title = element_text(face = "bold", size = 14, hjust = 6))
            #axis.text.x = element_text(angle = 20, hjust = 1))
    # Mengubah Visual Statis menjadi Interaktif
    ggplotly(plot2, tooltip = 'text')
  })
  
  #PLOT3
  output$plot_3 <- renderPlotly({
    sales_opmargin <- 
      sales %>% 
      filter(season %in% input$input_cat1,
             year %in% input$input_cat2) %>% 
      group_by(Retailer, Region) %>% 
      summarise(Operating.Margin = mean(Operating.Margin)) %>% 
      arrange(-Operating.Margin) %>% 
      mutate(label = glue("Retailer: {Retailer}
                      {Region} Op Margin: {comma(Operating.Margin)}%"))
    # Pembuatan Visual Statis
    plot3 <- 
      ggplot(data = sales_opmargin, 
             mapping = aes(x=Operating.Margin, y=reorder(Retailer, Operating.Margin), text=label)) +
      geom_col(mapping = aes(fill=Region), position = "fill") +
      scale_fill_brewer(palette = "Dark2")+
      scale_x_continuous(labels = comma)+
      labs(title = "Operating Margin Based on Season",
           x = "Operating Margin (%)",
           y = "Retailer",
           fill="Region") +
      theme_classic()+
      theme(plot.title = element_text(face = "bold", size = 14, hjust = 6))
    # Mengubah Visual Statis menjadi Interaktif
    ggplotly(plot3, tooltip = 'text')
  })
  
  #PLOT4
  output$plot_4 <- renderPlotly({
    sales_prod <- 
      sales%>% 
      filter(season %in% input$input_cat1,
             year %in% input$input_cat2) %>% 
      group_by(Product.ID, Retailer) %>% 
      summarise(Units.Sold = sum(Units.Sold), .groups = 'drop') %>% 
      ungroup() %>% 
      mutate(label = glue("Retailer: {Retailer}
                      Units.Sold: {comma(Units.Sold)}"))
    urutan4 <- sales_prod %>% aggregate(Units.Sold ~ Product.ID, FUN = sum) %>% arrange(Units.Sold) %>%  mutate(Product.ID = as.character(Product.ID)) %>% select(Product.ID) %>% as.list()
    # Pembuatan Visual Statis
    plot4 <- 
      ggplot(data = sales_prod, 
             mapping = aes(x=Units.Sold, y=reorder(Product.ID, Units.Sold), text=label)) +
      geom_col(mapping = aes(fill=Retailer), position = "stack") +
      scale_fill_brewer(palette = "Dark2")+
      scale_x_continuous(labels = unit_format(scale = 1e-3, suffix = "K"),
                         breaks = seq(from = 0,
                                      to = 600e3,
                                      by = 100e3))+
      scale_y_discrete(limits = urutan4$Product.ID)+
      labs(title = "Total Product Sold Based on Season",
           x = "Unit Sold (in thousand)",
           y = "Product.ID",
           fill="Retailer") +
      theme_classic()+
      theme(plot.title = element_text(face = "bold", size = 14, hjust = 6),
            axis.text.y = element_text(angle = 45, hjust = 1))
    # Mengubah Visual Statis menjadi Interaktif
    ggplotly(plot4, tooltip = 'text')
  })

  output$plot_5 <- renderPlotly({
    sales_year <- 
      sales%>% 
      group_by(month, year) %>% 
      summarise(Total.Sales = sum(Total.Sales)) %>% 
      arrange(year) %>% 
      mutate(label = glue("Month: {month}
                      Total.Sales: {comma(Total.Sales)}"))
    # Pembuatan Visual Statis
    plot5 <- 
      ggplot(data = sales_year, 
             mapping = aes(x = month,
                           y = Total.Sales,
                           text=label,
                           # mewarnai line
                           color = year), palette = "Dark2")+
      # layer 1: line plot 
      geom_line(mapping = aes(group = year),
                #menebalkan garis line
                size = 1 )+
      # layer 2: menambahkan titik
      geom_point(size = 2)+
      # menambahkan separator bilangan
      scale_y_continuous(labels = unit_format(scale = 1e-6, suffix = " M"),
                         breaks = seq(from = 0,
                                      to = 150e6,
                                      by = 25e6))+
      labs(
           x="Month",
           y="Total Sales (in million dollar)")+
      theme_minimal()+
      theme(plot.title = element_text(face = "bold", size = 14, hjust = 6))
    # Mengubah Visual Statis menjadi Interaktif
    ggplotly(plot5, tooltip = 'text')
  })
  
  output$plot_6 <- renderPlotly({
    sales_method <- 
      sales%>% 
      group_by(Sales.Method) %>% 
      summarise(Total.Sales = sum(Total.Sales)) %>% 
      arrange(-Total.Sales) %>% 
      mutate(label = glue("Sales Method: {Sales.Method}
                      Total Sales: {comma(Total.Sales)}"))
    # Pembuatan Visual Statis
    plot6 <- 
      ggplot(data = sales_method, 
             mapping = aes(x=reorder(Sales.Method, Total.Sales), y=Total.Sales, text=label)) +
      geom_col(fill = "darkgreen") +
      scale_y_continuous(labels = unit_format(scale = 1e-6, suffix = " M"),
                         breaks = seq(from = 0,
                                      to = 400e6,
                                      by = 50e6))+
      labs(
           x = "Sales Method",
           y = "Total Sales (in million dollar)",
           fill="Retailer") +
      theme_classic()+
      theme(plot.title = element_text(face = "bold", size = 14, hjust = 6))
    # Mengubah Visual Statis menjadi Interaktif
    ggplotly(plot6, tooltip = 'text')
  })
  
  output$plot_7 <- renderPlotly({
    sales_region <- 
      sales%>% 
      filter(Region %in% input$input_cat3) %>%
      group_by(Retailer, Sales.Method) %>% 
      summarise(Total.Sales = sum(Total.Sales)) %>% 
      arrange(-Total.Sales) %>% 
      mutate(label = glue("Sales Method: {Sales.Method}
                      Total Sales: {comma(Total.Sales)}"))
    #Membuat agregasi urutan peringkat
    urutan7 <- sales_region %>% 
      aggregate(Total.Sales ~ Retailer, FUN = sum) %>% 
      arrange(Total.Sales) %>%  
      mutate(Retailer = as.character(Retailer)) %>% 
      select(Retailer) %>% 
      as.list()
    # Pembuatan Visual Statis
    plot7 <- 
      ggplot(data = sales_region, 
             mapping = aes(x=reorder(Retailer, -Total.Sales), y=Total.Sales, text=label)) +
      geom_col(mapping = aes(fill=Sales.Method), position = "stack") +
      scale_fill_brewer(palette = "Dark2")+
      scale_x_discrete(limits = urutan7$Retailer)+
      scale_y_continuous(labels = unit_format(scale = 1e-6, suffix = " M"),
                         breaks = seq(from = 0,
                                      to = 150e6,
                                      by = 25e6))+
      labs(title = glue("Total Sales on ", input$input_cat3),
           x = "Retailer",
           y = "Total Sales (in million dollar)",
           fill="Sales Method") +
      theme_classic()+
      theme(plot.title = element_text(face = "bold", size = 14, hjust = 6),
            axis.text.x = element_text(angle = 20, hjust = 1))
    # Mengubah Visual Statis menjadi Interaktif
    ggplotly(plot7, tooltip = 'text')
  })
  
  output$plot_8 <- renderPlotly({
    sales_region1 <- 
      sales%>% 
      filter(Region %in% input$input_cat3) %>%
      group_by(Retailer, Sales.Method) %>% 
      summarise(Units.Sold = sum(Units.Sold)) %>% 
      arrange(-Units.Sold) %>% 
      mutate(label = glue("Sales Method: {Sales.Method}
                      Units Sold: {comma(Units.Sold)}"))
    #Membuat agregasi urutan peringkat
    urutan8 <-  sales_region1 %>% 
      aggregate(Units.Sold ~ Retailer, FUN = sum) %>% 
      arrange(Units.Sold) %>%  
      mutate(Retailer = as.character(Retailer)) %>% 
      select(Retailer) %>% 
      as.list()
    # Pembuatan Visual Statis
    plot8 <- 
      ggplot(data = sales_region1, 
             mapping = aes(x=reorder(Retailer, -Units.Sold), y=Units.Sold, text=label)) +
      geom_col(mapping = aes(fill=Sales.Method), position = "stack") +
      scale_fill_brewer(palette = "Dark2")+
      scale_x_discrete(limits = urutan8$Retailer)+
      scale_y_continuous(labels = unit_format(scale = 1e-3, suffix = " K"),
                         breaks = seq(from = 0,
                                      to = 400e3,
                                      by = 50e3))+
      labs(title = glue("Total Product Sold on ", input$input_cat3),
           x = "Retailer",
           y = "Unit Sold (in thousand)",
           fill="Sales Method") +
      theme_classic()+
      theme(plot.title = element_text(face = "bold", size = 14, hjust = 6),
            axis.text.x = element_text(angle = 20, hjust = 1))
    # Mengubah Visual Statis menjadi Interaktif
    ggplotly(plot8, tooltip = 'text')
  })
  
  #TAB MAPS
  output$leaflet <- renderLeaflet({
    sales_state <- 
      sales%>% 
      #filter(Region %in% "West") %>%
      group_by(State) %>% 
      summarise(Total.Sales = sum(Total.Sales)) %>% 
      arrange(-Total.Sales) %>% 
      mutate(label = glue("State: {State}
                      Total Sales: {comma(Total.Sales)}"))
    # Make the merge
    my_sf_merged <- sales_state %>%
      left_join(my_sf, by = c("State" = "name"))
    #Color palette
    mybins <- c(0, 1000000, 10000000, 30000000, 50000000, 70000000)
    mypalette <- colorBin(palette = "viridis", domain = my_sf_merged$Total.Sales, na.color = "transparent", bins = mybins)
    # Prepare the text for tooltips:
    mytext <- paste(
      "State: ", my_sf_merged$State, "<br/>",
      "Total Sales: $", {comma(my_sf_merged$Total.Sales)} ,"<br/>",
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    #Data perlu dirubah ke bentuk sf lagi supaya R bisa membaca data geometry nya
    my_sf_merged <- sf::st_as_sf(my_sf_merged)
    
    maps <- leaflet(my_sf_merged) %>%
      addTiles() %>%
      setView(lat = 50, lng = -100, zoom = 2.5) %>%
      addPolygons(
        fillColor = ~ mypalette(my_sf_merged$Total.Sales),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette, values = ~my_sf_merged$Total.Sales, opacity = 0.9,
        title = "Total Sales", position = "bottomleft"
      )
    maps
  })
  
  # TAB DATA
  
  output$data1 <- DT::renderDataTable(datadis, options = list(scrollX = T))
  })




