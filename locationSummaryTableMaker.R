locationSummaryTableMaker_f = function(customerInputTable = NULL, basinSummary = NULL, scenario = NULL, categoryThreshold = 0.95)  {
  

  if(scenario == 1){
     myTable = data.table::data.table(
        "Location" =       customerInputTable$Location_Name, 
        "Local_Value" =    basinSummary$currentRatio_A,
        "Local_Value_Drought" =    basinSummary$currentRatio_B,
        "Local_Trend" =    basinSummary$trendLow_Deficit_A,
        "Regional_Value" = basinSummary$currentRatio_C,
        "Regional_Value_Drought" = basinSummary$currentRatio_D,
        "Regional_Trend" = basinSummary$trendLow_Deficit_C
      )
  }
  
  if(scenario == 2){
      myTable = data.table::data.table(
        "Location" =      customerInputTable$Location_Name, 
        "Local_Value" =    basinSummary$currentRatio_A,
        "Local_Value_Drought" =    basinSummary$currentRatio_B,
        "Local_Trend" =    basinSummary$trendMed_Deficit_A,
        "Regional_Value" = basinSummary$currentRatio_C,
        "Regional_Value_Drought" = basinSummary$currentRatio_D,
        "Regional_Trend" = basinSummary$trendMed_Deficit_C
        )
  }
  
  
  if(scenario == 3){
      myTable = data.table::data.table(
        "Location" = customerInputTable$Location_Name, 
        "Local_Value" =    basinSummary$currentRatio_A,
        "Local_Value_Drought" =    basinSummary$currentRatio_B,
        "Local_Trend" =    basinSummary$trendHigh_Deficit_A,
        "Regional_Value" = basinSummary$currentRatio_C,
        "Regional_Value_Drought" = basinSummary$currentRatio_D,
        "Regional_Trend" = basinSummary$trendHigh_Deficit_C
        )
  }
  

  myTable$Local = "=" ;            myTable$Local[myTable$Local_Trend < 0] = "-" ;                          myTable$Local[myTable$Local_Trend > 0] = "+"
  myTable$Local_Direction = 1 ;    myTable$Local_Direction[myTable$Local_Value_Drought < categoryThreshold] = 0 ;myTable$Local_Direction[myTable$Local_Value < categoryThreshold] = -1
  myTable$Regional = "=";          myTable$Regional[myTable$Regional_Trend < 0] = "-" ;          myTable$Regional[myTable$Regional_Trend > 0] = "+"
  myTable$Regional_Direction = 1 ; myTable$Regional_Direction[myTable$Regional_Value_Drought < categoryThreshold] = 0 ; myTable$Regional_Direction[myTable$Regional_Value < categoryThreshold] = -1
  
  df = DT::datatable(myTable,
                options=list(
                  pageLength=100,
                  buttons = c("csv","excel"),
                  dom = "Bfrtip",
                  scrollX = TRUE,
                  scrollY = "350px",
                  columnDefs = list(list(targets=c(2:7,9,11), visible=FALSE))
                ),
                extensions="Buttons",
                selection = "single",
                rownames = TRUE#,
                #              server = FALSE # change to TRUE if the datatable function is slowing down the entire page
            ) 
  df %>% DT::formatStyle(
    c("Local","Regional"), c("Local_Direction", "Regional_Direction"),
    backgroundColor = styleEqual(c(-1,0,1), c("tomato","beige","royalblue")),
    fontSize = "150%",
    fontweight = "bold"
    )
}