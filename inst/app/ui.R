header <- tags$header(class = "main-header", HTML(
  '<header class="main-header">
  <span class="logo">Employment Prospects</span>
  <nav class="navbar navbar-static-top" role="navigation">
  <span style="display:none;">
  <i class="fa fa-bars"></i>
  </span>
  <a href="#" class="sidebar-toggle" data-toggle="offcanvas" role="button">
  <span class="sr-only">Toggle navigation</span>
  </a>
  <div class="navbar-custom-menu">
  <ul class="nav navbar-nav"></ul>
  </div>
  </nav>
  </header>'
))

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "menu",
    menuItem("Information", tabName = "info", icon = icon("info-circle"),
             selected = TRUE),
    menuItem("Employment Tool", tabName = "maps", icon = icon("map-marker")),
    conditionalPanel(
      "input.menu == 'maps'",
      selectInput(
        "statOne",
        label = "Select Measure One",
        choices = list(
          "Resilience of current employment measures" = dataColumnChoices[c(4:9), "full"],
          "Future Prospect Measures" = dataColumnChoices[c(10:20), "full"],
          "Other Measures" = dataColumnChoices[c(3, 21:44), "full"]
        ),
        selected = dataColumnChoices[12, "full"]
      ),
      selectInput(
        "statTwo",
        label = "Select Measure Two",
        choices = list(
          "Resilience of current employment measures" = dataColumnChoices[c(4:9), "full"],
          "Future employment prospect measures" = dataColumnChoices[c(10:20), "full"],
          "Other Measures" = dataColumnChoices[c(3, 21:44), "full"]
        ),
        selected = dataColumnChoices[24, "full"]
      ),
      selectizeInput(
        "ladSel",
        label = "Select up to 15 LADs",
        choices = c(Choose = "", sort(unique(shape@data$lad15nm))),
        multiple = TRUE,
        options = list(
          maxItems = 15,
          plugins = list('remove_button')
        )
      ),
      actionButton(
        "clearSelection",
        "Clear Selection(s)",
        icon = icon("trash-o")
      )
    ),
    downloadButton("downloadData", "Download all data")
  ),
  width = 300
)

body <- dashboardBody(
  shinyjs::useShinyjs(),
  # code to reset plotly's event_data("plotly_click", source="A") to NULL ->
  # executed upon action button click
  extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-A', 'null'); }"),
  tabItems(
    # Welcome page content
    tabItem(
      tabName = "info",
      fluidRow(
        column(
          width = 12,
          includeMarkdown("welcome.md")
        )
      )
    ),
    # Analysis page
    tabItem(
      tabName = "maps",
      tabBox(
        selected = "Maps",
        width = 12,
        tabPanel(
          "Maps",
          fluidRow(
            column(
              width = 6,
              tags$div(mapChoicesUI("mapOneChoices", select = 12),
                   class = "tab-pane"),
              leafletOutput("mapOne"),
              textOutput("mapOneCap")
            ),
            column(
              width = 6,
              tags$div(mapChoicesUI("mapTwoChoices", select = 24),
                       class = "tab-pane"),
              leafletOutput("mapTwo"),
              textOutput("mapTwoCap")
            )
          ),
          fluidRow(
            column(
              width = 6,
              br(),
              downloadButton("saveMapOne", label = "Download png"),
              br()
            ),
            column(
              width = 6,
              br(),
              downloadButton("saveMapTwo", label = "Download png"),
              br()
            )
          ),
          fluidRow(
            uiOutput("warnPJS")
          )
        ),
        tabPanel(
          "Bar Charts",
          fluidRow(
            column(
              width = 6,
              plotlyOutput("barOne"),
              textOutput("barOneCap")
            ),
            column(
              width = 6,
              plotlyOutput("barTwo"),
              textOutput("barTwoCap")
            )
          )
        ),
        tabPanel(
          "Scatter Plot",
          fluidRow(
            column(
              width = 6,
              selectInput(
                "scatColour",
                label = "Choose a statistic",
                choices = dataColumnChoices[c(3, 25:53), "full"],
                selected = dataColumnChoices[3, "full"]
              )
            ),
            column(
              width = 6,
              radioButtons(
                "colScale",
                label = "Choose a colour scale",
                choices = c("Continous" = "cont", "Categorical" = "cat"),
                selected = "cont",
                inline = TRUE
              )
            )
          ),
          textOutput("scatCap"),
          plotlyOutput("scatFig", height = 500)
        )#,
        #tabPanel(
        #  "Table",
        #  conditionalPanel(
        #    condition = "input$ladSel != ''",
        #    dataTableOutput("dataTable")
        #  )
        #)
      )
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)

dashboardPage(
  header,
  sidebar,
  body, title = "Employment Prospects"
)
