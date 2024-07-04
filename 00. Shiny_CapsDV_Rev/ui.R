# Fungsi dashboardPage() diperuntuhkan untuk membuat ketiga bagian pada Shiny
dashboardPage(skin = "green",# untuk melakukan perubah warna, tapi warnanya msh terbatas
              
                               
              # Fungsi dashboardHeader() adalah bagian untuk membuat header
              dashboardHeader(title = "Adidas Sales Report"), # untuk memberika judul pada bagian header
              
              # Fungsi dashboardSidebar() adalah bagian untuk membuat sidebar
              dashboardSidebar(
                sidebarMenu(
                  menuItem(
                    text = "Overview", # Memberikan pada tampilan UI menu
                    tabName = "Overview", # Sebuah key/indentifier untuk mengisi bagian menu 1
                    icon = icon("house")
                  ),
                  menuItem(
                    text = "Trend Analysis",
                    tabName = "Trend_Analysis",
                    icon = icon("chart-simple")
                  ),
                  menuItem(
                    text = "Maps",
                    tabName = "Maps",
                    icon = icon("map")
                  ),
                  menuItem(
                    text = "Data Set",
                    tabName = "Data_Set",
                    icon = icon("database")
                  ),
                  menuItem("Source Code", 
                           icon = icon("file-code"), 
                           href = "https://github.com/galuhchynintya/Caps_Adidas_Sales_Report")
                )
              ),
              
              # Fungsi dashboardBody() adalah bagian untuk membuat isi body
              dashboardBody(
                # untuk menampun semua informasi ataupun menu yang sudah kita persiapkan
                tabItems(
                  # untuk mempersiapkan menu 1
                  tabItem(
                    tabName = "Overview", # parameter ini harus diisi sama persis dengan parameter tabName pada bagian menu yang ingin kita isi
                    fluidPage(
                      h2(tags$b("Adidas Sales Report 2020-2021")),
                      br(),
                      div(style = "text-align:justify",
                          p("An Adidas sales dataset is a collection of data that includes information on the sales of Adidas products in United States.", 
                            "This type of dataset may include details such as the number of units sold, the total sales revenue, 
                            the location of the sales, the type of product sold, and any other relevant information.",
                            "This report will help you to understand the summary of sales data based on season, region, product, and state."),
                          br()
                      )
                    ),
                    # fluidRow untuk mengatur infoBox()
                    fluidPage(
                      theme = "superhero",
                      valueBox(
                         "6", 
                         "Total Retailer",
                         width = 4,
                         icon = icon("shop"),
                         color = "maroon"),
                      valueBox(
                        "52", 
                        "Total City", 
                        width = 4,
                        icon = icon("city"),
                        color = "orange"),
                      valueBox(
                        "West Gear",
                        "Best Selling Retailer",
                        width = 4,
                        icon = icon("shoe-prints"),
                        color = "green"),
                      valueBox(
                        "619,715",
                        "Average Products Sold per Season",
                        width = 4,
                        icon = icon("bag-shopping"),
                        color = "purple"),
                      valueBox(
                        "$224,975,531",
                        "Average Sales per Season",
                        width = 4,
                        icon = icon("sack-dollar"),
                        color = "fuchsia"),
                      valueBox(
                        "42.3%",
                        "Average Operating Margin per Season",
                        width = 4,
                        icon = icon("money-bill-trend-up"),
                        color = "teal")
                      
                      ),
                      
                    # fluidRow untuk select input
                    fluidRow(
                      # box untuk menjadi wadah visual kita, dan agar visual kita tidak blend dengan background body shiny
                      box(width = 6, # untuk mengatur width dari box
                          # mempersiapkan select input
                          awesomeCheckboxGroup(
                            inputId = "input_cat1",
                            label = "Please Choose a Season", 
                            choices = unique(sales$season),
                            selected = "Winter",
                            inline = TRUE, 
                            status = "danger"
                          )
                          
                          
                          #checkboxGroupInput(inputId = "input_cat1", # parameter untuk key/identifier selecinput
                                             #choices = unique(sales$season), # parameter untuk menujukan pilihan yang akan diberikan
                                             #label = "Please Choose a Season") # untuk memberikan nama pada input
                      ),
                      box(width = 6,
                          awesomeCheckboxGroup(
                            inputId = "input_cat2",
                            label = "Please Choose a Year", 
                            choices = unique(sales$year),
                            selected = "2020",
                            inline = TRUE, 
                            status = "warning"
                          )
                     )
                     ),
                    fluidRow(
                      box(width = 6,
                          plotlyOutput(outputId = "plot_1")),
                      box(width = 6,
                          plotlyOutput(outputId = "plot_2"))
                    ),
                    fluidRow(
                      box(width = 6,
                          plotlyOutput(outputId = "plot_3")),
                      box(width = 6, 
                          plotlyOutput(outputId = "plot_4")
                    )
                  )
              ),
              #TAB ITEM 2
              tabItem(
                tabName = "Trend_Analysis",
                fluidPage(
                  tabBox(width = 12,
                         title = tags$b("Total Sales per Month"),
                         id = "tabset1",
                         side = "right",
                         tabPanel(tags$b("Year"), 
                                  plotlyOutput("plot_5")
                         ),
                         tabPanel(tags$b("Sales Method"), 
                                  plotlyOutput("plot_6"),
                         )
                         
                  )
                ),
                fluidRow(
                  box(width = 6,
                      selectInput(inputId = "input_cat3",
                                         choices = unique(sales$Region),
                                         label = "Please Choose a Region") ),
                  valueBox(
                    "Men's Street Footwear",
                    "Best Seller Product",
                    width = 6,
                    icon = icon("bag-shopping"),
                    color = "green")
                ),
                fluidRow(
                  box(width = 6, 
                      plotlyOutput(outputId = "plot_7")),
                  box(width = 6, 
                      plotlyOutput(outputId = "plot_8"))
                )
              ),
              
              #TAB ITEM 3
              tabItem(
                tabName = "Maps",
                fluidRow(
                  box(width = 12,
                      solidHeader = T,
                      h3(tags$b("Total Sales of Each State")),
                      leafletOutput("leaflet", height = 530))
                )
              ),
              
              #TAB ITEM 4
              tabItem(
                tabName = "Data_Set",
                h2(tags$b("Adidas Sales 2020-2021 Data")),
                DT::dataTableOutput("data1")
              )
          )
     )
)
                
                
                    
             