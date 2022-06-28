#' Creates Base style for all plots
#'
#' @export
#'
#' @param data Database containing columns: mean, database_id, cohort_id, strata
#' @param mode String:
#' 'Comorbidities' or 'C' or '1',
#' 'Tumor characteristics' or 'T' or '2',
#' 'Age groups' or 'A' or '3'

baseFreqPlot <- function(
  data,
  ylab = "Freq(%)",
  mode = "Comorbidities"
){
  data <- cleanDatabaseNames(data)

  data$feature_name <- labelsLewLine(data$feature_name, 25)

  database_name <- unique(data$database_id)
  cohorts_list <- unique(data$cohort_id)
  l <- length(database_name)
  c <- length(cohorts_list)


  if (l > 1){print('Multiple plots mode')
    database_name=''}
  else{print('Single plot mode')}

  if(l == 0){stop('Database should have \'database_id\' column')}


  #set different plot parameters for 2 cohorts VS MULT cohorts modes:


  #set different file parameters depending on the plot mode (single plot for one
  # database OR consolidated plot for all databases)
  if (database_name==''){
    png(file = paste0('barchart_', mode, '.png'),
        width     = 8.25,
        height    = 7.25,
        units     = "in",
        res       = 1400,
        pointsize = 5)
  }
  else{
    png(file = paste0('barchart_', mode,'_', database_name,'.png'),
        width     = 5.25,
        height    = 4.25,
        units     = "in",
        res       = 1200,
        pointsize = 5)}
  par(
    mar      = c(5, 5, 2, 2),
    xaxs     = "i",
    yaxs     = "i",
    cex.axis = 2,
    cex.lab  = 2
  )

  if (c == 2){
    print('2 cohorts mode')
    plot <- barchartTwoCohorts(data)
  }
  if (c > 2){
    print('Multiple cohorts mode')
    plot <- barchartMultCohorts(data)
  }

  plot <- basePlotDesign(plot)

  #set different plot parameters depending on the plot mode (single plot for one
  #database OR consolidated plot for all databases)
  if (database_name==''){
    plot <- plot +
      ggplot2::labs(x=mode,
          #title=paste0('Frequency of ', mode, ' at baseline for Immediate Treatment\nand Conservative Management Target cohorts'),

           y="Freq(%)",
           fill="Cohorts") +

      ggplot2::facet_wrap(~database_id, ncol=4) +
      geom_col(width=0.9, position=position_dodge())
  }

  else {
    plot <- plot +
      ggplot2::labs(
        title=database_name,
        #subtitle = 'Frequency of comorbidities at baseline for\nImmediate Treatment and Conservative\nManagement Target cohorts',
        x=mode,
        y="Freq(%)",
        fill="Cohorts") +

      geom_col(width=0.5, position=position_dodge())}

  print(plot)
  dev.off()
}
