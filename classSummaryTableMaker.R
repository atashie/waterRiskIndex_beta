classSummaryTableMaker_f = function(basinSummary = NULL, scenario = NULL) {
  numRows = nrow(basinSummary)
  
  if(scenario == 1){
    myTable = data.table::data.table(
      "Index_Class" = rep("E", numRows),
      "Trend_raw" = basinSummary$trendLow_Deficit_C
    )
    
    theseRows = which(basinSummary$currentDeficit_C >= 0)
    myTable$Index_Class[theseRows] = "D"
    myTable$Trend_raw[theseRows] = basinSummary$trendLow_Deficit_C[theseRows]
    
    theseRows = which(basinSummary$currentDeficit_D >= 0)
    myTable$Index_Class[theseRows] = "C"
    myTable$Trend_raw[theseRows] = basinSummary$trendLow_Deficit_D[theseRows]
    
    theseRows = which(basinSummary$currentDeficit_A >= 0)
    myTable$Index_Class[theseRows] = "B"
    myTable$Trend_raw[theseRows] = basinSummary$trendLow_Deficit_A[theseRows]
    
    theseRows = which(basinSummary$currentDeficit_B >= 0)
    myTable$Index_Class[theseRows] = "A"
    myTable$Trend_raw[theseRows] = basinSummary$trendLow_Deficit_B[theseRows]
    }

  # Med road
  if(scenario == 2){
    myTable = data.table::data.table(
      "Index_Class" = rep("E", numRows),
      "Trend_raw" = basinSummary$trendMed_Deficit_C
    )
    
    theseRows = which(basinSummary$currentDeficit_C >= 0)
    myTable$Index_Class[theseRows] = "D"
    myTable$Trend_raw[theseRows] = basinSummary$trendMed_Deficit_C[theseRows]
    
    theseRows = which(basinSummary$currentDeficit_D >= 0)
    myTable$Index_Class[theseRows] = "C"
    myTable$Trend_raw[theseRows] = basinSummary$trendMed_Deficit_D[theseRows]
    
    theseRows = which(basinSummary$currentDeficit_A >= 0)
    myTable$Index_Class[theseRows] = "B"
    myTable$Trend_raw[theseRows] = basinSummary$trendMed_Deficit_A[theseRows]
    
    theseRows = which(basinSummary$currentDeficit_B >= 0)
    myTable$Index_Class[theseRows] = "A"
    myTable$Trend_raw[theseRows] = basinSummary$trendMed_Deficit_B[theseRows]
  }

  # high emissiosn
  if(scenario == 3){
    myTable = data.table::data.table(
      "Index_Class" = rep("E", numRows),
      "Trend_raw" = basinSummary$trendHigh_Deficit_C
    )

    theseRows = which(basinSummary$currentDeficit_C >= 0)
    myTable$Index_Class[theseRows] = "D"
    myTable$Trend_raw[theseRows] = basinSummary$trendHigh_Deficit_C[theseRows]
    
    theseRows = which(basinSummary$currentDeficit_D >= 0)
    myTable$Index_Class[theseRows] = "C"
    myTable$Trend_raw[theseRows] = basinSummary$trendHigh_Deficit_D[theseRows]
    
    theseRows = which(basinSummary$currentDeficit_A >= 0)
    myTable$Index_Class[theseRows] = "B"
    myTable$Trend_raw[theseRows] = basinSummary$trendHigh_Deficit_A[theseRows]
    
    theseRows = which(basinSummary$currentDeficit_B >= 0)
    myTable$Index_Class[theseRows] = "A"
    myTable$Trend_raw[theseRows] = basinSummary$trendHigh_Deficit_B[theseRows]
  }
  
  # trends should be neg, none, pos, and saved as factors
  myTable$Trend_raw[is.na(myTable$Trend_raw)] = 0
  myTable$Trend = "None"
  myTable$Trend[which(myTable$Trend_raw < 0)] = "Negative"
  myTable$Trend[which(myTable$Trend_raw > 0)] = "Positive"
  
  
  # basic barplot
  ggplot(myTable, aes(x=Index_Class, fill=Trend)) +
    geom_bar() +
    scale_fill_manual(values = c("Negative" = "tomato", "None" = "beige", "Positive" = "royalblue")) +
    ylab("Number of Locations") +
    xlab("Index Class") +
    theme_minimal(base_size = 20)
}