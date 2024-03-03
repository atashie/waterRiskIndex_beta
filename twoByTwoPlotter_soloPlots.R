twoByTwoPlotter_f = function(twoByTwoTables = NULL, whichLocs = NULL, stressClass = NULL)  {
  
  locRange = seq(whichLocs[1], whichLocs[2])
  # replacing trend nas with zeroes
  if(any(is.na(twoByTwoTables$Trajectory))) {twoByTwoTables$Trajectory[is.na(twoByTwoTables$Trajectory)] = 0}
  
  personal_theme = theme(
    panel.background = element_rect(fill = 'transparent'), panel.grid.major = element_line(color = 'grey80'), panel.grid.minor = element_line(color = 'grey90'),
    axis.title.x = element_blank(), axis.title.y = element_blank())
  
  if(stressClass == 2) {
    ylimRange = max(abs(range(twoByTwoTables$Trajectory)))
    myTable = subset(twoByTwoTables, waterClass == 2)[locRange]
    myPlot = ggplot(myTable) +
      geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=ylimRange),fill='beige', alpha=.1) +
      geom_rect(aes(xmin=1,xmax=2,ymin=-ylimRange,ymax=0),fill='beige', alpha=.1) +
      geom_rect(aes(xmin=0,xmax=1,ymin=-ylimRange,ymax=0),fill='tomato', alpha = .1) +
      geom_rect(aes(xmin=1,xmax=2,ymin=0,ymax=ylimRange),fill='royalblue', alpha=.1) +
      geom_point(aes(x=Current_Value, y=Trajectory), fill='grey80',color='grey20') +
      geom_text_repel(data=myTable, aes(x=Current_Value, y=Trajectory, label=Location),
                      max.overlaps=50,  color='grey0', segment.color='grey20') +
      xlim(0,2) +
      ylim(-ylimRange,ylimRange) +
      ggtitle("Local (drought)") +
      personal_theme
  }

  
  if(stressClass == 1) {
    ylimRange = max(abs(range(twoByTwoTables$Trajectory)))
    myTable = subset(twoByTwoTables, waterClass == 1)[locRange]
    myPlot = ggplot(myTable) +
      geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=ylimRange),fill='beige', alpha=.1) +
      geom_rect(aes(xmin=1,xmax=2,ymin=-ylimRange,ymax=0),fill='beige', alpha=.1) +
      geom_rect(aes(xmin=0,xmax=1,ymin=-ylimRange,ymax=0),fill='tomato', alpha = .1) +
      geom_rect(aes(xmin=1,xmax=2,ymin=0,ymax=ylimRange),fill='royalblue', alpha=.1) +
      geom_point(aes(x=Current_Value, y=Trajectory), fill='grey80',color='grey20') +
      geom_text_repel(data=myTable, aes(x=Current_Value, y=Trajectory, label=Location),
                      max.overlaps=50,  color='grey0', segment.color='grey20') +
      xlim(0,2) +
      ylim(-ylimRange,ylimRange) +
      ggtitle("Local (typical)") +
      personal_theme # + theme(plot.title=element_text(hjust=0.5))
  }
  

  if(stressClass == 4) {
    ylimRange = max(abs(range(twoByTwoTables$Trajectory)))
    myTable = subset(twoByTwoTables, waterClass == 4)[locRange]
    if(any(myTable$Current_Value > 2)) {myTable$Current_Value[myTable$Current_Value > 2] = 2}
    myPlot = ggplot(myTable) +
      geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=ylimRange),fill='beige', alpha=.1) +
      geom_rect(aes(xmin=1,xmax=2,ymin=-ylimRange,ymax=0),fill='beige', alpha=.1) +
      geom_rect(aes(xmin=0,xmax=1,ymin=-ylimRange,ymax=0),fill='tomato', alpha = .1) +
      geom_rect(aes(xmin=1,xmax=2,ymin=0,ymax=ylimRange),fill='royalblue', alpha=.1) +
      geom_point(aes(x=Current_Value, y=Trajectory), fill='grey80',color='grey20') +
      geom_text_repel(data=myTable, aes(x=Current_Value, y=Trajectory, label=Location),
                      max.overlaps=50,  color='grey0', segment.color='grey20') +
      xlim(0,2) +
      ylim(-ylimRange,ylimRange) +
      ggtitle("Regional (drought)") +
      personal_theme # + theme(plot.title=element_text(hjust=0.5))
  }
  
  if(stressClass == 3) {
    ylimRange = max(abs(range(twoByTwoTables$Trajectory)))
    myTable = subset(twoByTwoTables, waterClass == 3)[locRange]
    if(any(myTable$Current_Value > 2)) {myTable$Current_Value[myTable$Current_Value > 2] = 2}
    myPlot = ggplot(myTable) +
      geom_rect(aes(xmin=0,xmax=1,ymin=0,ymax=ylimRange),fill='beige', alpha=.1) +
      geom_rect(aes(xmin=1,xmax=2,ymin=-ylimRange,ymax=0),fill='beige', alpha=.1) +
      geom_rect(aes(xmin=0,xmax=1,ymin=-ylimRange,ymax=0),fill='tomato', alpha = .1) +
      geom_rect(aes(xmin=1,xmax=2,ymin=0,ymax=ylimRange),fill='royalblue', alpha=.1) +
      geom_point(aes(x=Current_Value, y=Trajectory), fill='grey80',color='grey20') +
      geom_text_repel(data=myTable, aes(x=Current_Value, y=Trajectory, label=Location),
                      max.overlaps=50,  color='grey0', segment.color='grey20') +
      xlim(0,2) +
      ylim(-ylimRange,ylimRange) +
      ggtitle("Regional (typical)") +
      personal_theme # + theme(plot.title=element_text(hjust=0.5))
  }
#  grid.arrange(myPlot2, myPlot1, myPlot4, myPlot3, ncol=2, bottom="Current Index Value", left = "Trajectory of Change (per decade)")

  myPlot  
}
