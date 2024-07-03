#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("./gracePlotter.R")
source("./indexPlotter.R")
source("./climatePlotter.R")
source("./climatePlotterMonthly.R")
source("./locationTableMaker.R")
source("./locationSummaryTableMaker.R")
#source("./twoByTwoPlotter.R")
source("./twoByTwoPlotter_soloPlots.R")
source("./towByTwoTables.R")
source("./twoByTwoSummaryText.R")
source("./waterIndextText.R")
source("./climateText.R")
source("./locationSpecificReanalysis.R")
source("./waterSupplyProjectionsPlotter.R")
source("./classSummaryTableMaker.R")
library(mapview)
library(leaflet)
library(viridis)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(DT) # for interactive tables via datatable()
#library(plotly)
customerInputTable = data.table::fread("./Data/Customer Onboarding Information_NuveenChile_May2024.csv", skip=1)
#customerInputTable = data.table::fread("./Data/Customer Onboarding Information_McCain_Mar2024.csv", skip=1)
#customerInputTable = data.table::fread("./Data/Customer Onboarding Information_McCain.csv", skip=1)
#customerInputTable = data.table::fread("./Data/Customer Onboarding Information_RaboChile.csv", skip=1)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        .myRow1 {
          height: 350px;
        }
        .myRow2 {
          height: 400px;
        }
        .myRow3 {
          height: 500px;
        }

      ")
    )
  ),
  
  
  # Application title
  titlePanel(title=div(img(src="./CAi2.png", height=60, width=200), "Water Risk Index (beta)")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #        fileInput("file", "Upload CSV file"),
      width=2,
      selectInput("location", label = "Choose a Location", choices = customerInputTable$Location_Name),  
      radioButtons(inputId = "scenario",
                   label = "Climate Scenario:",
                   choiceValues = c(2,1,3),
                   choiceNames = c("Middle of the Road", "Low Emissions", "High Emissions"),
      ),
      radioButtons(inputId = "valType",
                   label = "Units:",
                   choiceValues = c(2,1),
                   choiceNames = c("mm", "ratio")
      )
    ),
    #    ),
    
    
    # Structure of the page
    mainPanel(
      tabsetPanel(type="tabs",
                  # First Tab
                  tabPanel("Portfolio View",
                           fluidRow(
                             class="myRow2",
                             column(7,
                                    fluidRow(
                                      column(4,
                                             radioButtons(inputId = "locationToggle", label = "Toggle Locations",
                                                          choices = list("Locations" = FALSE, "Watersheds" = TRUE), selected = FALSE
                                             )
                                      ),
                                      column(4,
                                             selectInput(inputId = "mapValueToPlot", label = "Value to Map:",
                                                         c("Recent Storage Trends [mm/yr]" = 5, "Recent Storage Trends (scaled) [mm/yr]" = 6, 
                                                           "Local (drought) [-]" = 2, "Local (typical) [-]" = 1, 
                                                           "Regional (drought) [-]" = 4, "Regional (typical) [-]" = 3,
                                                           "Projected Precip Trends [mm/yr/dc]" = 7, "Projected Streamflow Trends [km3/yr/dc]" = 8,
                                                           "Aridity Index [-]" = 10,"Current Avg Precip [mm/yr]" = 9)
                                             )
                                      ),
                                      column(4,
                                             selectInput(inputId = "mapIndexClassToPlot", label = "Index Classes:",
                                                         c("A"="A", "B"="B", "C"="C","D"="D","E"="E"),
                                                         multiple = TRUE, selected = c("A","B","C","D","E")
                                             )
                                      )
                                    ),
                                    mapviewOutput("pfMap", height = "400px")
                             ),
                             column(5,
                                    tags$footer(
                                      "For further information visit the ",
                                      tags$a(
                                        "Water Risk Index Summary page",
                                        target = "_blank",
                                        href = "https://climateaiteam.notion.site/Water-Risk-Index-Summary-Overview-fa0b8822e3f44f119d7916f961d10c3e?pvs=4"
                                      ),
                                      style = "position: absolute; width: 100%; color: black; text-align: center;"
                                    ),
                                    hr(),
                                    h4("Summary by Index Class and Trend", style = "text-align: center;"),
                                    plotOutput("classSummaryBarplot"),
                                    tags$footer(
                                      tags$em(
                                      "(A) Local resources consistently sufficient. (B) Local resources stressed during drought. (C) Local + Regional resources consistently sufficient. (D) Local + Regional resources stressed during drought. (E) Demand consistently exceeds Local + Regional resources."
                                      ),
                                      style = "position: absolute; width: 100%; color: black; text-align: left;"
                                    )
                             )
                           ),
                           hr(),
                           hr(),
                           fluidRow(
                             #                    class="myrow3",
                             h4(". ", style = "color: white"),
                             h4(". ", style = "color: white"),
                             h4(". ", style = "color: white"),
                             hr(),
                             h3("2x2 Hazard Overview:"),
                             column(8,
                                    column(6,
                                           selectInput(inputId = "stressClass", label = "Stress Class:",
                                                       c("Regional (typical) [-]" = 3, "Regional (drought) [-]" = 4,
                                                         "Local (typical) [-]" = 1, "Local (drought) [-]" = 2)),
                                    ),
                                    column(6,
                                           sliderInput("twoByTwosToPlot", "Locations to Plot:",
                                                       min=1, max=nrow(customerInputTable), c(1, min(nrow(customerInputTable), 100)))
                                    ),
                                    plotOutput("twoByTwoPlot", height="550px"),
                                    hr(),
                                    hr(),
                                    hr(),
                                    h4('Highlights:'),
                                    textOutput("portfolioHighlights1"),
                                    hr(),
                                    textOutput("portfolioHighlights2")
                             ),
                             column(4,
                                    hr(),
                                    hr(),
                                    h4('Index Summary Table', style = "text-align: center;"),
                                    DT::dataTableOutput("portfolioSummaryTable"),
                                    img(src='locationSummaryLegend.png', align = "right", width = "100%")
                             )
                           ),
                           fluidRow(
                             class="myrow3",
                             hr(),
                             hr(),
                             hr(),
                             column(12,
                                    h4('Index Values by Decade', style = "text-align: center;"),
                                    DT::dataTableOutput("portfolioTable")
                             )
                           )
                  ),
                  # Second Tab
                  tabPanel("Watershed View",
                           fluidRow(
                             class="myRow2",
                             column(7,
                                    mapviewOutput("wsMap", height = "400px")),
                             column(5,
                                    fluidRow(
                                      class="myRow2",
                                      h4("Recent Changes in Watershed Storage", style = "text-align: center;"),
                                      plotOutput("gracePlot", height = "200px"),
                                      hr(),
                                      h4("Watershed Summary Information:"),
                                      div(style = "height: 150px;", textOutput("summaryText")))
                             )
                           ),
                           hr(),
                           fluidRow(
                             h3("Water Risk Index", style = "text-align: center;"),
                           ),
                           fluidRow(
                             column(6,
                                    h4("Entire Watershed: Index Value Projections", style = "text-align: center;"),
                                    plotOutput("watershedIndexPlot")
                             ),
                             column(6,
                                    h4("User's Fields: Index Value Projections", style = "text-align: center;"),
                                    plotOutput("usersFieldsIndexPlot")
                             ),
                           ),
                           fluidRow(
                             column(5,
                                    hr(),
                                    h4("Highlights:"),
                                    textOutput("waterIndexText")
                             ),
                             column(7,
                                    h4('Index Values by Decade', style = "text-align: center;"),
                                    tableOutput("indexTable"))
                           ), 
                           fluidRow(
                             hr(),
                             h3("Hydroclimate Projections", style = "text-align: center;"),
                             column(8, plotOutput("climatePlot")),
                             column(4, 
                                    h4('Highlights:'),
                                    textOutput("climateText"))
                           ),
                           fluidRow(
                             hr(),
                             h3("Monthly Hydroclimate Projections", style = "text-align: center;"),
                             selectInput(inputId = "monthlyClimVar", label = "Hydroclimate Variable:",
                                         c("Precipitation [mm]" = 1, "Potential Evapotranspiration [mm]" = 2,
                                           "Streamflow [km3]" = 4, "Soil Moisture [%]" = 5)),
                             plotOutput("climateMonthlyPlot"),
                             hr()
                           )
                  ),
                  # Third Tab
                  tabPanel("Location Analysis",
                           column(3,
                                  selectInput("irrigationMethod", "Irrigation Method:",
                                              c("Use Current Inputs" = 999, "Sprinkler" = 0.85, "Drip" = 0.95, "Surface" = 0.65)),
                                  selectInput("detentionPond", "Rainfall Detention Ponds:",
                                              c("Use Current Inputs" = 999, "No" = 0.5, "Yes" = 0))
                           ),
                           column(3,
                                  sliderInput("irrigationAllotment", "Adjust Allotment for Irrigation (%):",
                                              min = -99, max = 100, 0),
                                  numericInput("marAllotment", "Allotment for MAR (hectare meters):",
                                               value=0, min = 0, max = 1000)
                           ),
                           column(3,
                                  selectInput("groundwaterAccess", "Groundwater Access:",
                                              c("Use Current Inputs" = 999, "Yes" = 1, "No" = 0)),
                                  selectInput("soilMoistureAccess", "Crops in Ground or Potted:",
                                              c("Use Current Inputs" = 999, "In Ground" = 1, "[tool in progress]" = 1))
                           ),
                           column(3,
                                  sliderInput("growArea", "Adjust Irrigated Crop Area (%):",
                                              min = -99, max = 100, 0),
                                  selectInput("plantHybrid", "Plant Hybrid:",
                                              c( "User Current Inputs" = 1, "Drought Tolerant" = 0.85, "Drought Intolerant" = 1.15))
                           ),
                           fluidRow(
                             column(6,
                                    h4("Index Value Projections with Updates", style = "text-align: center;"),
                                    plotOutput("indexPlotUpdates")
                             ),
                             column(6,
                                    h4("Projected Change in Total Water Storage"),
                                    plotOutput("waterSupplyProjections")
                             )
                           ),
                           hr(), 
                           fluidRow(
                             column(6,
                                    h4('Index Values by Decade with Updates', style = "text-align: center;"),
                                    tableOutput("indexTableUpdates")),
                             column(6,
                                    h4('Watershed Summary:'),
                                    textOutput("waterSupplyProjectionsSummary")
                             )
                           ),
                           fluidRow(
                             class="myRow2",
                             #                   column(7,
                             #                         mapviewOutput("pfMap", height = "400px")),
                             column(12,
                                    h3('Coming Soon: Cost Analysis', style = "text-align: center;"),
                                    textOutput("locationIndexText"),
                                    plotOutput("locationIndexPlot"))
                           )
                           
                  )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  customerInputTable = data.table::fread("./Data/Customer Onboarding Information_NuveenChile_May2024.csv", skip=1)
  customerInputSf = sf::st_as_sf(customerInputTable, coords=c("Longitude", "Latitude"), crs = 4326)
  climateArray = readRDS("./Data/NuveenChile_May2024_regional_rawValues.rds")
  indexArray = readRDS("./Data/NuveenChile_May2024_local_waterIndex.rds")
  watershedIndexArray = readRDS("./Data/NuveenChile_May2024_regional_waterIndex.rds")
  graceHistorical = data.table::fread("./Data/NuveenChile_May2024_graceHistorical.csv")
  basinSummary_ws = data.table::fread("./Data/NuveenChile_May2024_regional_hydroBasins_wIndex.csv")
  basinSummary = data.table::fread("./Data/NuveenChile_May2024local_hydroBasins_wIndex.csv")
  basinSummary$AridityIndex = basinSummary$currentPrecip_avg / basinSummary$currentPET_avg
  basinShapes = sf::st_read("./Data/NuveenChile_May2024_hydroBasins_shapesOnly.shp")
  thisCategoryThreshold = 0.95 # setting the threshold below which a location falls into a higher investment category
  
  graceSub <- reactive({
    list(subset(graceHistorical, Location == input$location), which(customerInputTable$Location_Name == input$location))
  })
  
  ################################
  # outputs for watershed view tab
  output$wsMap = renderLeaflet({
    Watershed_Boundary = basinShapes[as.numeric(graceSub()[[2]]), ]
    mapviewOptions(basemaps = c("Esri.WorldImagery", "OpenStreetMap.DE","OpenTopoMap"))
    myMap =
      mapview(
        Watershed_Boundary,
        color = "green2", lwd = 3, alpha.regions = 0.0, col.regions="grey90", legend=FALSE
      ) 
    myMap@map
  })
  
  
  
  # grace plotter
  output$gracePlot <- renderPlot({
    plotData = graceSub()[[1]]
    
    gracePlotter_f(plotData)
  })
  
  # summary text
  output$summaryText = renderText({
    thisBasin = as.numeric(graceSub()[[2]])
    basinSub = basinSummary[thisBasin, ]
    basinSub = basinSummary_ws[thisBasin, ]
    if(basinSub$thisFrcAreaUnderCult >= 0.05)  {
      incOrDecGrace = ifelse(basinSub$recentHistoricSlope >= 0, "increase", "decrease") 
      myText = paste0("Recent historical storage ", incOrDecGrace, " of ", round(basinSub$recentHistoricSlope, 0), " mm per year, or ",
                      round(basinSub$rescaledRecentHistoricSlope, 0), " mm per irrigated acre per year. ",
                      "An estimated ", round(basinSub$thisFrcAreaUnderCult*100, 0), "% of the watershed is devoted to agriculture, of which ",
                      round(100 * basinSub$thisFrcCultAreaWthIrr / basinSub$thisFrcAreaUnderCult, 0), "% irrigated. And the average water needs for crops in this watershed are ",
                      round(basinSub$thisWplant, 0), " mm per year.")
    } else {
      incOrDecGrace = ifelse(basinSub$recentHistoricSlope >= 0, "increased", "decreased") 
      myText = paste0("Agriculture has been neglibible in this watershed, and recent historical storage has ", 
                      incOrDecGrace, " at a rate of ", round(basinSub$recentHistoricSlope, 0), " mm per year."
      )
    }
    myText
  })
  
  # water risk index plotter
  output$usersFieldsIndexPlot <- renderPlot({
    thisLoc = as.numeric(graceSub()[[2]])
    thisScen = as.numeric(input$scenario)
    thisValType = as.numeric(input$valType)
    indexPlotter_f(waterIndexDataPlot = indexArray, thisLoc = thisLoc, thisScen = thisScen, indexValuesToPlot = c(4,2,8,6), thisValType = thisValType)
  })
  
  output$watershedIndexPlot <- renderPlot({
    thisLoc = as.numeric(graceSub()[[2]])
    thisScen = as.numeric(input$scenario)
    thisValType = as.numeric(input$valType)
    indexPlotter_f(waterIndexDataPlot = watershedIndexArray, thisLoc = thisLoc, thisScen = thisScen, indexValuesToPlot = c(4,2,8,6), thisValType = thisValType)
  })
  
  
  # Index values table
  output$indexTable <- renderTable({
    thisLoc = as.numeric(graceSub()[[2]])
    thisScen = as.numeric(input$scenario)
    thisValType = as.numeric(input$valType)
    myTable = data.table::data.table(
      "Decade" = factor(seq(2010, 2090, 10)), 
      "Local (drought)" =        indexArray[thisLoc, , 8, thisScen, 4, thisValType],
      "Local (average)" =        indexArray[thisLoc, , 8, thisScen, 2, thisValType],
      "Regional (drought)" = indexArray[thisLoc, , 8, thisScen, 8, thisValType],
      "Regional (average)" = indexArray[thisLoc, , 8, thisScen, 6, thisValType]
    )
    myTable
  })
  
  # water risk index text
  output$waterIndexText = renderText({
    thisLoc = as.numeric(graceSub()[[2]])
    waterIndexText_f(basinSummary = basinSummary, scenario = as.numeric(input$scenario), thisLoc = thisLoc)
  })
  
  # climate plotter
  output$climatePlot <- renderPlot({
    climatePlotter_f(climateDataPlot = climateArray, thisLoc = as.numeric(graceSub()[[2]]), thisScen = as.numeric(input$scenario))
  })
  
  # hydroclimate text
  output$climateText = renderText({
    climateText_f(basinSummary = basinSummary, scenario = as.numeric(input$scenario), thisLoc = as.numeric(graceSub()[[2]]))
  })

  # climate monthly plotter
  output$climateMonthlyPlot <- renderPlot({
    climatePlotterMonthly_f(climateDataPlot = climateArray, thisLoc = as.numeric(graceSub()[[2]]), thisScen = as.numeric(input$scenario), thisClimVar = as.numeric(input$monthlyClimVar))
  })

  ###################################  
  # outputs for portfolio view tab
  Watershed_Boundaries = cbind(customerInputTable$Location_Name, basinShapes, basinSummary)
  
  #### for portvolio view map:
  deficitTable = data.table::data.table("Index_Class" = rep("E", nrow(basinSummary)))
  deficitTable$Index_Class[which(basinSummary$currentRatio_C >= thisCategoryThreshold)] = "D"
  deficitTable$Index_Class[which(basinSummary$currentRatio_D >= thisCategoryThreshold)] = "C"
  deficitTable$Index_Class[which(basinSummary$currentRatio_A >= thisCategoryThreshold)] = "B"
  deficitTable$Index_Class[which(basinSummary$currentRatio_B >= thisCategoryThreshold)] = "A"

  colsToPlot = c("currentRatio_A", "currentRatio_B", "currentRatio_C", "currentRatio_D",
                 "recentHistoricSlope", "rescaledRecentHistoricSlope",
                 "trendMed_Precip_avg", "trendMed_Streamflow_avg",
                 "currentPrecip_avg", "AridityIndex")
  
  output$pfMap = renderLeaflet({
    thisStressClass = as.numeric(input$mapValueToPlot)
    plotThisCol = colsToPlot[thisStressClass]
    plotTheseRows = which(deficitTable$Index_Class %in% input$mapIndexClassToPlot)
    watershed_plotter = cbind(Watershed_Boundaries[plotTheseRows, plotThisCol], customerInputTable$Location_Name[plotTheseRows])
    names(watershed_plotter) = c("Value","Location Name", "geometry")
    thisPal = ifelse(thisStressClass %in% c(1:8), colorRampPalette(c("red3", "white", "blue3")), colorRampPalette(c("red1",'yellow2', "blue4")))
    if(thisStressClass < 5) {thisRange = seq(0,2,0.2)}
    if(thisStressClass %in% c(5:8)) {
      absMax = max(abs(watershed_plotter[[1]]), na.rm=TRUE)
      thisRange = seq(-absMax, absMax, length.out = 10)
    }
    if(thisStressClass == 9) {thisRange = seq(0, 2000,200)}
    if(thisStressClass == 10) {thisRange = seq(0,2, length.out = 10)}
    mapviewOptions(
      basemaps = c("OpenStreetMap.DE","Esri.WorldImagery", "Esri.WorldShadedRelief", "OpenTopoMap"),
      legend.pos = "bottomright")
    if(input$locationToggle) {
      myMap =
        mapview(
          watershed_plotter, at = thisRange, col.regions = thisPal, 
          zcol = "Value",
          #zcolor = thisPal, #color = 'grey20',
          lwd = 0.5, 
          alpha.regions = 0.5, 
          legend=TRUE,
          layer.name="Values"
        )       
    } else {
      sf::st_geometry(watershed_plotter) = sf::st_geometry(customerInputSf[plotTheseRows, ])
      myMap = 
        mapview(
          watershed_plotter, at = thisRange, 
          zcol = "Value",
          cex = 3.75,
          col.regions = thisPal, 
          legend=TRUE,
          layer.name="Values"
        )
    }
    myMap@map
  })
  
  output$classSummaryBarplot <- renderPlot({
    classSummaryTableMaker_f(basinSummary = basinSummary, scenario = as.numeric(input$scenario), categoryThreshold = thisCategoryThreshold)
  })
  
  output$portfolioSummaryTable <- DT::renderDataTable({
    locationSummaryTableMaker_f(customerInputTable, basinSummary, scenario = as.numeric(input$scenario), categoryThreshold = thisCategoryThreshold)
  })
  
  output$portfolioTable <- DT::renderDataTable({
    locationTableMaker_f(customerInputTable, basinSummary, scenario = as.numeric(input$scenario), valType = as.numeric(input$valType))
  })
  
  # generating two by two table
  twoByTwoTables = reactive({
    twoByTwoTables_f(basinSummary = basinSummary, customerInputTable = customerInputTable, thisScen = as.numeric(input$scenario), thisStressClass =  as.numeric(input$stressClass))
  })
  
  # two by two plotter
  output$twoByTwoPlot <- renderPlot({
    #    twoByTwoPlotter_f(twoByTwoTables = twoByTwoTables(), as.numeric(input$twoByTwosToPlot))
    twoByTwoPlotter_f(twoByTwoTables = twoByTwoTables(), whichLocs = as.numeric(input$twoByTwosToPlot), stressClass = as.numeric(input$stressClass)) 
  })
  
  # two by two highlights
  output$portfolioHighlights1 = renderText({
    twoByTwoSummaryText_f(twoByTwoTables = twoByTwoTables(), whichText=1, categoryThreshold = thisCategoryThreshold)
  })
  output$portfolioHighlights2 = renderText({
    twoByTwoSummaryText_f(twoByTwoTables = twoByTwoTables(), whichText=2, categoryThreshold = thisCategoryThreshold)
  })
  
  ##################################################################
  # outputs for location analysis tab
  
  locationSpecificWRIcalcs = reactive({
    waterIndexCalculations_locationSpecific_f(
      customerTable_input = customerInputTable,
      thisRow = as.numeric(graceSub()[[2]]),
      hydroBasins = basinSummary,
      climateData = climateArray,
      irrigationMethod = as.numeric(input$irrigationMethod),
      detentionPonds = as.numeric(input$detentionPond),
      irrigatedCropAreaPctChng = as.numeric(input$growArea),
      irrigationAllotments = as.numeric(input$irrigationAllotment),
      allotmentForMar = as.numeric(input$marAllotment),
      groundwaterAccess = as.numeric(input$groundwaterAccess),
      soilMoistureAccess = as.numeric(input$soilMoistureAccess),
      plantHybrid = as.numeric(input$plantHybrid)
    )
  })
  
  #  output$locationIndexText = renderText({
  #    paste0(locationSpecificWRIcalcs())
  #  })
  
  output$indexPlotUpdates <- renderPlot({
    thisLoc = as.numeric(graceSub()[[2]])
    thisScen = as.numeric(input$scenario)
    thisValType = as.numeric(input$valType)
    newIndexInfo = locationSpecificWRIcalcs()
    thisArray = newIndexInfo[[1]]
    #    basinSummary = newIndexInfo[[2]]
    indexPlotter_f(waterIndexDataPlot = thisArray, 
                   indexValuesToPlot = c(4,2,8,6), thisValType = thisValType, thisLoc = thisLoc, thisScen = thisScen)
  })
  
  # Index values table
  output$indexTableUpdates <- renderTable({
    thisLoc = as.numeric(graceSub()[[2]])
    thisScen = as.numeric(input$scenario)
    thisValType = as.numeric(input$valType)
    thisArray = locationSpecificWRIcalcs()[[1]]
    myTable = data.table::data.table(
      "Decade" = factor(seq(2010, 2090, 10)), 
      "Local (drought)" =        thisArray[thisLoc, , 8, thisScen, 4, thisValType],
      "Local (average)" =        thisArray[thisLoc, , 8, thisScen, 2, thisValType],
      "Regional (drought)" = thisArray[thisLoc, , 8, thisScen, 8, thisValType],
      "Regional (average)" = thisArray[thisLoc, , 8, thisScen, 6, thisValType]
    )
    myTable
  })
  
  # updated water risk index text gen
  output$waterSupplyProjectionsSummary <- renderText({
    thisLoc = as.numeric(graceSub()[[2]])
    waterIndexText_f(basinSummary = locationSpecificWRIcalcs()[[2]], scenario = as.numeric(input$scenario), thisLoc = thisLoc)
  })
  
  output$waterSupplyProjections <- renderPlot({
    adjustedOutputs = locationSpecificWRIcalcs()
    waterSupplyProjectionsPlotter_f(waterIndexDataPlot = adjustedOutputs[[1]], basinSummary = adjustedOutputs[[2]], 
                                    groundwaterAccess = as.numeric(input$groundwaterAccess),
                                    thisLoc =  as.numeric(graceSub()[[2]]),
                                    thisScen = as.numeric(input$scenario)
    )
  })
  
  
  
  
  #  output$hoverInfo <- renderPrint({
  #   thisPoint = nearPoints(myTable, input$plotHover, allRows = FALSE)
  #  paste0("thisVal is ", thisPoint$Current_value, " and ", thisPoint$Trajectory)
  #  })
  
  #  paste("mpg:", hover$mpg, "<br>",
  #       "wt:", hover$wt)
  
  
  ####################################
  #outputs for location analysis tab
}

# Run the application 
shinyApp(ui = ui, server = server)
