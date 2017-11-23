server <- function(input, output, session) {

  #############################################################################
  ## Determine the reactive values

  rvs <- reactiveValues(
    # Plotly event data
    e = NULL,
    # mapOne data
    mapOne = 0,
    # mapTwo data
    mapTwo = 0
  )

  #############################################################################
  ## Download All Data

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      write.csv(emp, con)
    }
  )

  #############################################################################
  ## Load in Data

  # extract the short name to match the data
  shortNameOne <- reactive({
    dataColumnChoices[dataColumnChoices$full == input[["statOne"]], "short"]
  })
  shortNameTwo <- reactive({
    dataColumnChoices[dataColumnChoices$full == input[["statTwo"]], "short"]
  })

  # create the leaflet and hex datasets
  geoData <- reactive({
    createLeafletData(data = emp)
  })
  # merge the statistics data with the hexagon map data
  hexData <- reactive({
    hexMapJson@data <- dplyr::left_join(
      hexMapJson@data, emp, by = c("lad15nm" = "la_name")
    )
    hexMapJson
  })

  #############################################################################
  ## Create the maps

  # Map One
  mapOneData <- reactive({
    if (input[["mapOneChoices-mapType"]] == "hex") {
      hexQuints <- calculateQuintiles(emp, shortNameOne())
      hexMapJson@data <- dplyr::left_join(
        hexMapJson@data, hexQuints, by = c("lad15nm" = "la_name")
      )
      hexMapJson
    } else {
      createLeafletData(calculateQuintiles(emp, shortNameOne()))
    }
  })
  mapOneType <- reactive({
    if (input[["mapOneChoices-mapType"]] == "hex") {
      TRUE
    } else {
      FALSE
    }
  })
  mapOneFill <- reactive({
    if (input[["mapOneChoices-fillType"]] == "quint") {
      "quints"
    } else {
      shortNameOne()
    }
  })
  output$mapOne <- renderLeaflet({
    rvs$mapOne <- leafMap(
      data = mapOneData(),
      fill = mapOneFill(),
      hex = mapOneType(),
      addLegend = TRUE
    )
    rvs$mapOne
  })

  # Map Two
  mapTwoData <- reactive({
    if (input[["mapTwoChoices-mapType"]] == "hex") {
      hexQuints <- calculateQuintiles(emp, shortNameTwo())
      hexMapJson@data <- dplyr::left_join(
        hexMapJson@data, hexQuints, by = c("lad15nm" = "la_name")
      )
      hexMapJson
    } else {
      createLeafletData(calculateQuintiles(emp, shortNameTwo()))
    }
  })
  mapTwoType <- reactive({
    if (input[["mapTwoChoices-mapType"]] == "hex") {
      TRUE
    } else {
      FALSE
    }
  })
  mapTwoFill <- reactive({
    if (input[["mapTwoChoices-fillType"]] == "quint") {
      "quints"
    } else {
      shortNameTwo()
    }
  })

  output$mapTwo <- renderLeaflet({
    rvs$mapTwo <- leafMap(
      data = mapTwoData(),
      fill = mapTwoFill(),
      hex = mapTwoType(),
      addLegend = TRUE
    )
    rvs$mapTwo
  })

  #############################################################################
  ## Add marker events to the maps

  mapOneProxy <- leafletProxy("mapOne")

  mapOneMarkers <- reactive({
    markers <- mapOneData()[mapOneData()$lad15nm %in% input[["ladSel"]], ]

    ranks <- markers@data %>%
      mutate(origRows = as.numeric(rownames(.))) %>%
      arrange_(.dots = paste0("desc(", mapOneFill(), ")")) %>%
      mutate(rank = rownames(.)) %>%
      arrange(origRows)

    markers@data$rank <- ranks$rank[ranks$lad15nm %in% input[["ladSel"]]]

    markers
  })

  detMarkSelOne <- reactive({
    sprintf(
      "<strong>LAD: </strong>%s<br><strong>Value: </strong>%s<br><strong>Rank: </strong>%s",
      mapOneMarkers()@data$lad15nm,
      mapOneMarkers()@data[, mapOneFill()],
      mapOneMarkers()@data[, "rank"]
    ) %>%
      lapply(htmltools::HTML)
  })

  observeEvent({
    input[["statOne"]]
    mapOneMarkers()
    input[["mapOneChoices-fillType"]]
  }, {
    ladOne <-
      mapOneProxy
    if (!is.null(input[["ladSel"]])) {
      ladOne <-
        ladOne %>%
        addPolygons(
          data = mapOneMarkers(),
          fill = NULL,
          color = "#000000",
          weight = 3,
          opacity = 1,
          label = detMarkSelOne(),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          ),
          highlightOptions = highlightOptions(
            color = "#000000",
            weight = 3,
            opacity = 1,
            bringToFront = TRUE
          )
        )
    }
    ladOne
  })

  mapTwoProxy <- leafletProxy("mapTwo")

  mapTwoMarkers <- reactive({
    markers <- mapTwoData()[mapTwoData()$lad15nm %in% input[["ladSel"]], ]

    ranks <- mapTwoData()@data %>%
      mutate(origRows = as.numeric(rownames(.))) %>%
      arrange_(.dots = paste0("desc(", mapTwoFill(), ")")) %>%
      mutate(rank = rownames(.)) %>%
      arrange(origRows)

    markers@data$rank <- ranks$rank[ranks$lad15nm %in% input[["ladSel"]]]

    markers
  })

  detMarkSelTwo <- reactive({
    sprintf(
      "<strong>LAD: </strong>%s<br><strong>Value: </strong>%s<br><strong>Rank: </strong>%s",
      mapTwoMarkers()@data$lad15nm,
      mapTwoMarkers()@data[, mapTwoFill()],
      mapTwoMarkers()@data[, "rank"]
    ) %>%
      lapply(htmltools::HTML)
  })


  observeEvent({
    input[["statTwo"]]
    mapTwoMarkers()
    input[["mapTwoChoices-fillType"]]
  }, {
    ladTwo <-
      mapTwoProxy
    if (!is.null(input[["ladSel"]])) {
      ladTwo <-
        ladTwo %>%
        addPolygons(
          data = mapTwoMarkers(),
          fill = NULL,
          color = "#000000",
          weight = 3,
          opacity = 1,
          label = detMarkSelTwo(),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          ),
          highlightOptions = highlightOptions(
            color = "#000000",
            weight = 3,
            opacity = 1,
            bringToFront = TRUE
          )
        )
    }
    ladTwo
  })

  #############################################################################
  ## Save the maps

  # if PhatomJS is not installed, warn the user
  observe({
    if (is.null(phantom)) {
      output$warnPJS <- renderUI({
        shiny::includeMarkdown("PhantomJS-warn.md")
      })
    }
  })

  # if PhantomJS is not installed, allow the user to download the plot
  observe({
    if (!is.null(phantom)) {
      Sys.sleep(1)
      # enable the download button
      shinyjs::enable("saveMapOne")
    }
  })
  output$saveMapOne <- downloadHandler(
    filename = function() paste0(shortNameOne(), ".png"),

    content = function(file) {
      withProgress(
        mapshot(rvs$mapOne, file = file),
        message = "Downloading plot..."
      )
    }
  )
  # if PhantomJS is not installed, do not allow the user to download the plot
  shinyjs::disable("saveMapOne")

  # if PhantomJS is not installed, allow the user to download the plot
  observe({
    if (!is.null(phantom)) {
      Sys.sleep(1)
      # enable the download button
      shinyjs::enable("saveMapTwo")
    }
  })
  output$saveMapTwo <- downloadHandler(
    filename = function() paste0(shortNameTwo(), ".png"),

    content = function(file) {
      withProgress(
        mapshot(rvs$mapTwo, file = file),
        message = "Downloading plot..."
      )
    }
  )
  # if PhantomJS is not installed, do not allow the user to download the plot
  shinyjs::disable("saveMapTwo")

  #############################################################################
  ## Clear the select inputs

  # reset region selection inputs on a button click
  observeEvent(input$clearSelection, {
    shinyjs::js$resetClick()
    updateSelectizeInput(
      session,
      "ladSel",
      choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
    )
  })

  #############################################################################
  ## Add inputs on map click

  observeEvent(input[["mapOne_shape_click"]], {
    updateSelectizeInput(
      session,
      "ladSel",
      choices = sort(unique(shape@data$lad15nm)),
      selected = c(
        input$ladSel,
        shape@data[
          shape@data$lad15cd %in% input[["mapOne_shape_click"]]$id,
          ]
      )
    )
  })
  observeEvent(input[["mapTwo_shape_click"]], {
    updateSelectizeInput(
      session,
      "ladSel",
      choices = sort(unique(shape@data$lad15nm)),
      selected = c(
        input$ladSel,
        shape@data[
          shape@data$lad15cd %in% input[["mapTwo_shape_click"]]$id,
          ]
      )
    )
  })

  #############################################################################
  ## Add the bar plots

  output$barOne <- renderPlotly({
    withProgress(
      ladBarChart(
        data = emp,
        var = shortNameOne(),
        highlight = input[["ladSel"]]
      ),
      message = "Rendering bar chart..."
    )
  })

  output$barTwo <- renderPlotly({
    withProgress(
      ladBarChart(
        data = emp,
        var = shortNameTwo(),
        highlight = input[["ladSel"]]
      ),
      message = "Rendering bar chart..."
    )
  })

  #############################################################################
  ## Create the scatter plot

  observe({
    rvs$e <- plotly::event_data("plotly_click")
  })

  # output the scatter graph
  output$scatFig <- renderPlotly({

    # calculate the scatter plot colour statistic
    fullColour <- dataColumnChoices[
      dataColumnChoices$full %in% input[["scatColour"]],
      "short"
      ]
    colourQuant <- quantile(
      emp[, fullColour],
      probs = c(1 / 3, 2 / 3),
      na.rm = TRUE
    )

    compDat <- emp %>%
      mutate(
        colourCol = factor(if_else(
          emp[, fullColour] < colourQuant[1], paste0("<", colourQuant[1]),
          if_else(
            emp[, fullColour] >= colourQuant[1] &
              emp[, fullColour] < colourQuant[2],
            paste0(colourQuant[1], "-", colourQuant[2]),
            paste0(">", colourQuant[2])
          )
        ),
        levels = c(paste0("<", colourQuant[1]),
                   paste0(colourQuant[1], "-", colourQuant[2]),
                   paste0(">", colourQuant[2]))
        ))

    updateSelectizeInput(
      session,
      "ladSel",
      choices = sort(unique(shape@data$lad15nm)),
      selected = c(
        input$ladSel,
        rvs$e$key
      )
    )

    xLab <- paste(
      strwrap(
        dataColumnChoices[dataColumnChoices$full %in% input[["statTwo"]],
                          "full"],
        width = 80
      ),
      collapse = "<br>"
    )
    yLab <- paste(
      strwrap(
        dataColumnChoices[dataColumnChoices$full %in% input[["statOne"]],
                          "full"],
        width = 50
      ),
      collapse = "<br>"
    )

    scatPlot(
      compDat,
      x = shortNameTwo(),
      y = shortNameOne(),
      colour = ifelse(input[["colScale"]] == "cat", "colourCol", fullColour),
      key = "la_name",
      xLab = xLab,
      yLab = yLab,
      highlight = input$ladSel,
      discrete = ifelse(input[["colScale"]] == "cat", TRUE, FALSE)
    ) %>%
      layout(margin = list(
        b = max(54, 1.5 * 18 * length(unlist(strsplit(xLab, "<br>")))),
        l = max(1.5 * 18 * length(unlist(strsplit(yLab, "<br>"))), 54)
      ))
  })

  #############################################################################
  ## Create the datatable

  # on selection from the drop down menu, display the region's data in a table
  subDat <- reactive ({
    colsToShow <- dataColumnChoices[c(12, 5:7, 24, 13:17, 32:46), "short"]
    rowNames <- dataColumnChoices[c(12, 5:7, 24, 13:17, 32:46), "full"]
    subDat <- geoData()[geoData()@data$lad15nm %in% input$ladSel, ]@data
    subDat <- subDat[order(match(subDat$lad15nm, input$ladSel)), ]
    nmSub <- subDat$lad15nm
    subDat <- subDat[, colnames(subDat) %in% colsToShow]
    subDat <- as.data.frame(t(subDat))
    subDat$measure <- rownames(subDat)
    subDat <- merge(columnMeans(geoData()[, colsToShow]@data), subDat)
    subDat <- subDat[match(colsToShow, subDat$measure), ]
    subDat <- subDat[, !(colnames(subDat) %in% "measure")]
    # We need a check for a dataframe here otherwise we get an error when it
    # tries to set the columns names of something that isn't a dataframe.
    # I think this is due to the renderDataTable taking precedence over the
    # observe test
    if (is.data.frame(subDat)) {
      colnames(subDat) <- c("National Average", nmSub)
      rownames(subDat) <- rowNames
    }
    subDat
  })

  output$dataTable <- renderDataTable({
    if (is.data.frame(subDat())) {
      datatable(
        subDat(),
        extensions = c("Buttons", "ColReorder", "FixedColumns", "FixedHeader"),
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 20, 30, 40),
          autoWidth = TRUE,
          columnDefs = list(list(width = "400px", targets = 0)),
          dom = "Btlp",
          buttons = c("csv", "copy"),
          colReorder = TRUE,
          scrollX = TRUE,
          #fixedColumns = list(leftColumns = 1),
          fixedHeader = TRUE
        )
      )
    } else {
      return(NULL)
    }
  })

  #############################################################################
  ## Download Handler

  output$downloadData <- downloadHandler(
    filename = "Download.csv",
    content = function(file) {
      write.csv(subDat(), file)
    }
  )

  #############################################################################
  ## Add figure captions

  output$mapOneCap <- renderText({
    paste0(
      "Figure 1: ",
      ifelse(mapOneType() == TRUE,
             "Hexagonal representation of the UK showing ",
             "Geographical representation of the UK showing "),
      tolower(
        dataColumnChoices[dataColumnChoices$full %in% input[["statOne"]],
                          "full"]
      )
    )
  })

  output$mapTwoCap <- renderText({
    paste0(
      "Figure 2: ",
      ifelse(mapTwoType() == TRUE,
             "Hexagonal representation of the UK showing ",
             "Geographical representation of the UK showing "),
      tolower(
        dataColumnChoices[dataColumnChoices$full %in% input[["statTwo"]],
                          "full"]
      )
    )
  })

  output$barOneCap <- renderText({
    paste0(
      "Figure 3: Ordered bar chart of ",
      tolower(
        dataColumnChoices[dataColumnChoices$full %in% input[["statOne"]],
                          "full"]
      )
    )
  })

  output$barTwoCap <- renderText({
    paste0(
      "Figure 4: Ordered bar chart of ",
      tolower(
        dataColumnChoices[dataColumnChoices$full %in% input[["statTwo"]],
                          "full"]
      )
    )
  })

  output$scatCap <- renderText({
    paste0(
      "Figure 5: Scatterplot of ",
      tolower(
        dataColumnChoices[dataColumnChoices$full %in% input[["statOne"]],
                          "full"]
      ),
      " vs. ",
      tolower(
        dataColumnChoices[dataColumnChoices$full %in% input[["statTwo"]],
                          "full"]
      ),
      " where points are coloured by ",
      tolower(
        dataColumnChoices[dataColumnChoices$full %in% input[["scatColour"]],
                          "full"]
      )
    )
  })
}
