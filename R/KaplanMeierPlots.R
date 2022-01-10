#' Creates grid Kaplan-Meier Plot
#'
#' @export
#'
gridKMplot <- function(
  data,
  palette = wesanderson::wes_palette("Darjeeling1", 6 , type = c("continuous")),
  conf.int = TRUE,
  ylab = "Probability",
  xlab = "Time in years",
  ggtheme = ggplot2::theme_bw(),
  savePlots = FALSE,
  directoryToSave,
  censor = FALSE,
  conf.int.style = "ribbon"
){
  listOfPlots <- sapply(c(10, 20, 30), function(year){
    survminer::ggsurvplot(
      data,
      conf.int = TRUE,
      palette = palette,
      ylab  = ylab, #"Probability",
      xlim = c(0, year),
      xlab = xlab, # "Time in years",
      break.time.by = dplyr::if_else(year == 10, 2,
                                     dplyr::if_else(year == 20, 4, 5)),
      ggtheme = ggtheme,
      ncensor.plot = FALSE,
      conf.int.style = conf.int.style,
      censor = censor
    ) + ggplot2::facet_grid(outcome_id
                            ~ database_id)
    if(savePlots){
      setwd(directoryToSave)
    ggplot2::ggsave(paste0("Grid","_", year, "_.jpeg"),
           width = 24, height = 24, units = "cm")
    }
  })
}
