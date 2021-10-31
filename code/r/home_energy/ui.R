

source("global.R")

ui <- dashboardPage(
  
  dashboardHeader(title = "Home Energy"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Primary Energy", tabName = "scotland_overall"),
      menuItem("CO2 Emissions", tabName = "co2_emissions_household"),
      menuItem("Current vs Potential", tabName = "current_potential_tab"),
      menuItem("Overview", tabName = "scotland_overview"),
      menuItem("Home Energy by Area", tabName = "home_energy_area"),
      menuItem("Map of Scotland", tabName = "map"),
      menuItem("About", tabName = "about")
    )
  ),
  
  ## Body content
  dashboardBody(
    shinyDashboardThemes(theme = "poor_mans_flatly"),
    tabItems(
      
      #overvall
      tabItem(tabName = "scotland_overall",
               h2("Average Household Primary Energy Consumption in Scotland"),
               
               fluidRow(
                 box(
                  title = "Scotland Overall",
                   status = "primary",
                   solidHeader = TRUE,
                   plotOutput("scotland_energy_overall", height = 400)
                 ),
                 #Tenure
                 box(
                   title = "by Tenure",
                   status = "primary",
                   solidHeader = TRUE,
                   plotOutput("tenure_primary", height = 400)
                 )
               )
               ),
      
      #CO2 emissions per household
      tabItem(tabName = "co2_emissions_household",
              h2("Average CO2 Emissions in Scotland"),
              
              fluidRow(
                box(
                  title = "Scotland Overall",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("scotland_co2_household", height = 400)
                ),
                #Tenure
                box(
                  title = "by Tenure",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("tenure_emissions", height = 400)
                )
              )
      ),
      
      #overvall
      tabItem(tabName = "current_potential_tab",
              h2("Current vs Potential CO2 Emissions in Scotland"),
              
              fluidRow(
                box(
                  #title = "Current vs Potential CO2 Emissions in Scotland",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("current_potential_plot", height = 400)
                )
              )
      ),
      
      # overview of Scotland
      tabItem(tabName = "scotland_overview",
              h2("Home Energy in Scotland"),
              
              fluidRow(
                box(width = 12,
                    background = "light-blue",
                    column(width = 6, 
                           selectInput(inputId = "year",
                                       label = "Year",
                                       choices = sort(unique(home_energy$year)),
                                       selected = "2020"
                           )
                           )
                    )
                ),
                        
              
              fluidRow(
                box(
                  title = "Primary Energy",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("primary_overview", height = 400)
                ),
                
                box(
                  title = "Current CO2 Emissions",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("current_overview", height = 400)
                )
              )
              ),
      
      
      
      # home energy content
      tabItem(tabName = "home_energy_area",
              h2("Home Energy by Area"),
              
              fluidRow(
                box(width = 12,
                    background = "light-blue",
                    column(width = 6,
                           div(style="display:inline-block; vertical-align:top; width: 150px;",
                               selectInput(inputId = "ca_name",
                                           label = "Local Authority",
                                           choices = sort(unique(home_energy$ca_name)),
                                           selected = "Aberdeen City")
                               ),
                           
                           div(style="display: inline-block;vertical-align:top; width: 80px;",HTML("<br>")),
                           
                               div(style="display:inline-block; vertical-align:top; width: 150px;",
                                   selectizeInput(inputId = "postcode",
                                       label = "Postcode",
                                       choices = sort(unique(home_energy$postcode)),
                                       options = list("max-options" = 1),
                                       selected = ""),
                                   tags$head(tags$style(HTML(".selectize-input {height: 50px; width: 150px;}")))
                           )
                    )
              )
              ),
              
              fluidRow(
                box(
                  title = "Primary Energy", 
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("primary_energy_output", height = 250)
                ),
        
        box(
          title = "Current CO2 Emissions", 
          solidHeader = TRUE,
          status = "primary",
          plotOutput("current_emissions_output", height = 250)
        )
              )
    ), 
    
    
    #map content
    
    tabItem(tabName = "map",
            h2("Map of Scotland"),
            fluidRow(
             
              leafletOutput("map", height = 500),
              )
            ),
      
      
      
      # About tab content
      tabItem(tabName = "about",
              h1("About"),
              h3("Author:", tags$a(href = "https://www.linkedin.com/in/stephanie-mpd/",
                                   "Stephanie Duncan")),
              "This interactive dashboard gives insights and trends on home energy in Scotland between 2012 - 2020.",
              br(),
              "The code I wrote to produce this dashboard can be found on my ", tags$a(href = "https://github.com/stephanieduncan/climate_challenge_cop26/", "Github Repository"),
              br(),
              "The data used to carry out analysis for this dashboard is open source and can be found in the links below.",
              br(),
              br(),
              fluidRow(
                box(title = "Data Sources", solidHeader = TRUE, status = "primary",
                    h5(strong("Statistics.gov.scot")),
                    
                    tags$a(href="https://statistics.gov.scot/data/domestic-energy-performance-certificates", 
                           "Home Energy Performance Certificates Data"),
                    br(),
                    tags$a(href="https://www.opendata.nhs.scot/es_AR/dataset/geography-codes-and-labels/resource/e92d19d4-ced7-40c8-b628-e28e4528fc41", 
                           "Geography Local Authority Area Labels Lookup")
                )
              )
      )
    )
  )
)

