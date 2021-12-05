library(shiny)
library(bslib)
library(shinydashboard)
library(DT)



shinyUI(
  fluidPage( theme = bs_theme(version = 4, bootswatch = "minty"),
    navbarPage(id = 'US_Arrest',
        title=div(img(src="Report.png",height = '50px', width = '50px'), "US Arrest - Information", class="navbar-light"),
        ####--- POBLACION - MAPA Y DATA --------------------------------------
        tabPanel("Poblacion", id='Tab_Poblacion', icon= icon('globe-americas'),
                 sidebarLayout(
                     sidebarPanel(
                       radioButtons("Crimen", 
                                    label= "Tipo de crimen:",
                                    c("Poblacion EEUU" = "UrbanPop")
                                    ),
                       sliderInput("range",
                                   label = "Rango de informacion a mostrar:",
                                   min = 0, max = 100, value = c(0, 100)),
                       br(),
                       actionButton("btn_Limpiar", "Limpiar", class="btn btn-warning"),
                       
                     ),
                     mainPanel(
                       tabsetPanel(type = "tabs",
                                   tabPanel("Mapa por Estados", id='Tab_Mapa',  
                                            plotOutput("USmap")),
                                   tabPanel("Datos por Estado",id='Tab_Datos1',
                                            fluidRow(
                                              column(6,
                                                     h3('EEUU - Datos por estado (1993)'),
                                                     DT::dataTableOutput("UStable"),
                                                     br(),
                                                     h3('EEUU - Generales Estados Seleccionados (1997)'),
                                                     DT::dataTableOutput("select_table")
                                              )
                                            ))
                       )
                       
                     )
                 )
        ),
        ####--- CRIMEN - GRAFICAS---------------------------------------
        tabPanel('Graficas', id='Tab_Crimen2', icon= icon('user-secret'),
                 sidebarLayout(
                   sidebarPanel(
                    textInput("TipoCrimen", "Ingrese el tipo de crime en la URL: (Murder, Assault, Rape)", "Murder"),
                    radioButtons("Crime", "Seleccione Crimen", choices = c("Murder", "Assault", "Rape")),
                    selectInput("Estado", "Resumen por Estado", choices = c('Alabama','Alaska','Arizona','Arkansas','California','Colorado',
                                                                            'Connecticut','Delaware','Florida','Georgia','Hawaii','Idaho',
                                                                            'Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana',
                                                                            'Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi',
                                                                            'Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey',
                                                                            'New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma',
                                                                            'Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee',
                                                                            'Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin',
                                                                            'Wyoming'), 
                                selected = "Alabama"),
                    fluidRow(column(6,
                                    plotOutput("EstadodistPlot"))
                    ),
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     plotOutput("distPlot"),
                     fluidRow(column(6,
                                     plotOutput("TopdistPlot")),
                              column(6,
                                     plotOutput("TopNdistPlot"))
                     ),
                     fluidRow(column(6,
                                     plotOutput("TopdistPlotpor")),
                              column(6,
                                     plotOutput("TopNdistPlotpor"))
                     ),
                     plotOutput("TreePlot"),
                     
                   )
                 ),         
        )
        ####---OTROS---------------------------------------
        # navbarMenu("subpanels", 
        #            tabPanel("panel 4a", "four-a"),
        #            tabPanel("panel 4b", "four-b"),
        #            tabPanel("panel 4c", "four-c")
        # )
        ####------------------------------------------------
      )
  )
)