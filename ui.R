library(shiny)

shinyUI(fixedPage( 
  HTML("<body style=background:##FAFAFA> </body>"),
  fixedRow(
    HTML("<hr color=SteelBlue noshade=noshade />"),
    column(width=4,offset = 0,
           tags$img(src = "logo12.jpg", width = "200px", height = "90px",border="2")
    ),
    column(width=4,offset = 0,
           HTML("<div align=left> <font color=SteelBlue face=Arial size=6> Informes BemerCar </font> </div>")
    ),
    column(width=4,offset = 0,
           tags$img(src = "logo21.jpg", width = "210px", height = "90px",border="3") 
    )
  ),    
  HTML("<hr color=SteelBlue noshade=noshade />"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId="data", label="Cargar Archivo", multiple = FALSE, 
                accept = c(".xls",".xlsx", ".csv")),
      helpText("Tamaño máximo del archivo 5MB"),
      tags$hr(),
      sliderInput("slide", 
                  label = "Número de Variables:",
                  min = 3, max = 7, value = 3)
    ),
    mainPanel(
      uiOutput("tb")
      
    )
    
  )
      )
  )

