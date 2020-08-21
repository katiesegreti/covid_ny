fluidPage(
  shiny::tags$head(
    shiny::tags$title("NY COVID"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  theme = shinytheme("cyborg"),
  titlePanel("COVID-19 tracker for New York"),
  #title = "COVID-19 tracker for New York",
  div(id = "links", a(href = "http://www.data-chips.com/", "data chips homepage"),
  a(href = "https://data-chips.shinyapps.io/covid_app/", "data chips multi-state dashboard")),
  br(),
      # sidebarPanel(
      #   width = 3,
      #   uiOutput("county_selector"),
      # uiOutput("states_msg")
      # 
      # ),
  
     fluidPage(
       
       
                uiOutput("county_selector"),
                
                tabsetPanel(id = "desktop_or_mobile",
                            tabPanel(title = "desktop (interactive charts)",
                                     uiOutput("latest_date"),
                                     tabsetPanel(id = "state_county",
                                                 tabPanel(title = "State summary",
                                                          br(),
                                                          plotlyOutput("state_plot",  width = "90%"),
                                                          br(),
                                                          reactableOutput("county_table", width = "90%")
                                                 ),
                                                 tabPanel(title = "County summary",
                                                          br(),
                                                          uiOutput("county_msg"),
                                                          plotlyOutput("county_plot",  width = "90%"),
                                                          #make county table for NY
                                                          reactableOutput("date_table", width = "90%"),
                                                          #plotlyOutput("county_plot1"),
                                                          br(),
                                                          plotlyOutput("county_plot2",  width = "90%")
                                                 )
                                     ),
                                     
                            ),
                            tabPanel(title = "mobile (static charts)",
                                     uiOutput("latest_date_mobile"),
                                     tabsetPanel(id = "state_country_mobile",
                                                 tabPanel(title = "State summary",
                                                          br(),
                                                          plotOutput("state_plot_mobile",  width = "90%"),
                                                          br(),
                                                          reactableOutput("county_table_mobile", width = "90%")
                                                 ),
                                                 tabPanel(title = "County summary",
                                                          br(),
                                                          uiOutput("county_msg_mobile"),
                                                          plotOutput("county_plot1_mobile",  width = "90%"),
                                                          reactableOutput("date_table_mobile", width = "90%"),
                                                          br(),
                                                          plotOutput("county_plot2_mobile",  width = "90%")
                                                 )
                                     )
                                     
                            )
                ),
                #uiOutput("testing"),
                
                br(),
                uiOutput("states_msg")
                #plotlyOutput("county_plot"),
                #uiOutput("positives_title"),
                
       
        
      )

   
  
)
