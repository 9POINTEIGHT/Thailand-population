ui <- fluidPage(
  titlePanel("Thailand Population / November 2563"),
  br(),
  fluidRow(column(2,
                  selectInput("levels",
                              label = "Select Levels",
                              choices = list("Country", "Province", "Amphoe")
                  )
  ),
  conditionalPanel(
    condition = "input.levels == 'Province' || input.levels == 'Amphoe' ",
    column(2,selectInput("provinces", 
                         label = "Select Provinces", 
                         choices = prov,
                         selected = NULL)
    )
  ),
  conditionalPanel(
    condition = "input.levels == 'Amphoe' ",
    column(2,
           selectInput("amphoes",
                       label = "Select Amphoes",
                       choices = "",
                       selected = NULL))
  ),
  column(2,
         selectInput("types",
                     label = "Select Gender",
                     choices = list("Total", "Female", "Male"))),
  br(),
  column(2,
         actionButton("go", "Submit")
  )
  ),
  hr(),
  tabsetPanel(
    tabPanel(title = "Population Map",
             leafletOutput("chmap")
    ),
    tabPanel(title = "Population Table",
             fluidRow(
               br(),
               column(12,
                      DT::DTOutput('popTbl'))
             )
    )
  )
)