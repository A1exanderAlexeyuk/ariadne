getThisPackageName <- function() {
  return("ariadne")
}


barchartTwoCohorts <- function(data){

  max_value <- as.numeric(max(data$mean))
  x_min <- max_value*(-100) + 10
  x_max <- max_value*100 + 10

  data <- orderByNumCohorts(data)
  data <- createMeanNegColumn(data)


  plot <- ggplot2::ggplot(data, ggplot2::aes(x = feature_name, y = mean_neg, fill = as.factor(cohort_id))) +

    ggplot2::geom_bar(data = subset(data, as.factor(cohort_id) == "103"), stat = "identity") +
    ggplot2::geom_bar(data = subset(data, as.factor(cohort_id) == "112"), stat = "identity") +

    #fill bars according to cohort_id, set names of legend labels
    ggplot2::scale_fill_manual(values=c("#0b6bb6", "#6eaf46"),name="", breaks=c("103", "112"),labels=c("Immediate Treatment", "Conservative Management"))+

    #format bar text labels
    ggplot2::geom_text(ggplot2::aes(label=as.factor(round(mean*100, digits=0))),
              hjust='outward', size=3) +

    ggplot2::scale_y_continuous(breaks=seq(-100, 100, by=50),
                               limits=c(-100, 100),
                               labels=c("", "50%", "0%","50%", ""))
    return(plot)
}


barchartMultCohorts <- function(data){

  #max value
  max_value <- plotMaxValue(data$mean)

  breaks_seq <- seq(0, max_value, by=max_value/5)

  #reorder bars
  data$feature <- factor(data$feature,                                    # Change ordering manually
                   levels = c(strsplit(paste(sort(unique(data$feature)), collapse=', '),", "))[[1]])
  data <- data %>% arrange(feature, days)
  # Plot
  p <- ggplot2::ggplot(data, aes(x=feature, y=mean, fill=as.factor(cohort_id))) +

    ggplot2::geom_col(position='dodge') +

  #add breaks and x axis labels
  ggplot2::scale_y_continuous(breaks=breaks_seq,
                             limits=c(0,max_value),
                             labels=c(paste0(breaks_seq[1]*100,'%'), paste0(breaks_seq[2]*100,'%'),
                                      paste0(breaks_seq[3]*100,'%'), paste0(breaks_seq[4]*100,'%'),
                                      paste0(breaks_seq[5]*100,'%'), paste0(breaks_seq[6]*100,'%'))) +
  ggplot2::scale_fill_manual(values=c(brewer.pal(6, "Paired")), name="", breaks=c(sort(unique(df$cohort_id))),
                             labels=addline_format(paste0(sort(unique(df$days)), " days")), guide=guide_legend(ncol = 1)) +
  ggplot2::geom_text(ggplot2::aes(label=as.factor(round(mean*100, digits=0))),
              position = position_dodge(0.9), size=1.9,
              vjust = 0.5,hjust = -0.2) +
  ggplot2::scale_x_discrete(limits = rev(levels(data$feature)))

  return(plot)
}


plotMaxValue <- function(column){
  max_value <- as.numeric(max(column))
  max_value <- round(max_value*100 %/% 10)
  max_value <- (max_value + 1)*10/100
}


orderByNumCohorts <- function(data){

  databases_one_cohort <- list()
  databases_two_cohorts <- list()

  database_name <- unique(data$database_id)
  iter <- length(database_name)
  for (i in 1:iter){
    subdf <- data[data$database_id == database_name[i]]

    if (length(unique(subdf$cohort_id))==1){
      databases_one_cohort <- append(databases_one_cohort, database_name[i])
    }
    else{
      databases_two_cohorts <- append(databases_two_cohorts, database_name[i])
    }
  }
  databases_one_cohort <- unlist(databases_one_cohort)
  databases_two_cohorts <- unlist(databases_two_cohorts)


  data <- data %>%
    mutate(across(database_id, factor, levels=c(databases_two_cohorts, databases_one_cohort)))

  return(data)
}


createMeanNegColumn <- function(data){
  # check if there are 2 unique cohorts present in the dataframe;
  # create mean_neg column with negative values for one of the columns
  # to be later used in the barchart
  unique_cohorts <- unique(data$cohort_id)
  if (length(unique_cohorts) == 2){
    data <- data %>%
      arrange(database_id, cohort_id) %>%
      mutate(mean_neg = case_when(
        cohort_id == unique_cohorts[2] ~ mean*(-100),
        cohort_id == unique_cohorts[1] ~ mean*100)) %>%
      arrange(cohort_id, mean_neg)}


}


#this function drops rows with <= N cohorts per strata
dropLessNCohorts <- function(data, numCohorts){
  database_name <- unique(data$database_id)

  for (dn in database_name){

    subset_dn <- data[data$database_id == dn]

    ages <- unique(subset_dn$age)
    for (a in ages){
      subset_age <- subset_dn[subset_dn$age == a]
      num_cohorts <- length(unique(subset_age$cohort_id))

      if (num_cohorts <= numCohorts){
        data <- data %>% filter(!(age==a & database_id == dn))
      }
    }
  }
}


basePlotDesign <- function(plot){

  plot <- plot +
    ggplot2::coord_flip() +
    cowplot::background_grid() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = 'bottom',
      #legend.position = c(0.93, 0.065),
      legend.margin = ggplot2::margin(6, 6, 6, 6),
      legend.box.background = ggplot2::element_rect(fill='lightgrey'),
      legend.text = ggplot2::element_text(size=9, colour="black"),
      axis.text.x = ggplot2::element_text(angle=0),
      plot.title = ggplot2::element_text(size=15),
      strip.background = ggplot2::element_rect(colour="black", fill="gray95"),
      strip.text = ggtext::element_textbox(size=9, color='black'),
      panel.grid.major.y = ggplot2::element_line(color = "gray99",
                                        size = 0),
      panel.grid.major.x = ggplot2::element_line(color = "gray95",
                                        size = 0.5),
      panel.grid.minor.x = ggplot2::element_line(color = "gray99",
                                        size = 0),
      panel.grid.minor.y = ggplot2::element_line(color = "gray99",
                                        size = 0)
    )

  return(plot)
}


chooseStrata <- function(mode = 'Comorbidities'){
  if (mode == 'Comorbidities' || mode == 'C' || mode == 1){
    filterStrata <- c('Type 2 Diabetes',
                      'Hypertension',
                      'Obesity',
                      'VTE',
                      'Anxiety',
                      'Prevalent Asthma or COPD',
                      'Any malignancy except malignant neoplasm of skin')
  }

  if (mode == 'Tumor Characteristics' || mode == 'T' || mode == 2){
    filterStrata <- c('Metastatic PCa', 'Locally Advanced PCa','Localized PCa',
                      'PSA >20 at Diagnosis', 'PSA <10 at Diagnosis', 'PSA 10-20 at Diagnosis',
                      'Stage cT1 at Dx',
                      'Stage cT2 at Dx',
                      'Stage cT3/cT4 at Dx',
                      'Grade 1 (GS 2-6)',
                      'Grade 2 (GS 3+4)',
                      'Grade 3 (GS 4+3)',
                      'Grade 4 (GS 8)',
                      'Grade 5 (GS 9-10)',
                      'EAU High Risk','EAU Low Risk','EAU Intermediate Risk')

  }

  return(filterStrata)

}


cleanDatabaseNames <- function(data){
  data <- data %>% mutate(database_id =
                        case_when(
                          database_id == "ambEMRSR" ~ "IQVIA AmbEMR",
                          database_id == "MktScan" ~ "MarketScan",
                          database_id == "oncoEMRSR" ~ "IQVIA OncoEMR",
                          database_id == "Pplus1124" ~ "PharmetricsPlus",
                          database_id == "FIMIM_IMASIS" ~ "FIMIM",
                          TRUE ~ database_id
                        )) %>%
    filter(!database_id %in% c('NCR', 'Truven1123'))
  return(data)
}


# add new line symbol('\n') after every Nth character (for legends)
labelsLewLine <- function(strings_vector, num_split){

  len <- length(strings_vector)

  for (i in 1:len){

    label_string <- strings_vector[i]
    len <- nchar(label_string)

    if (len <= num_split){
      next
    }

    if (len <= 2*num_split){

      result_string <- paste0(substring(label_string, 1, num_split),
                              '\n',
                              substring(label_string, num_split+1, len))
      strings_vector[i] <- result_string
      next
    }

    breaks <- seq(1, len, num_split)

    b = 1
    result_string <- list()
    for (br in breaks){
      if (br==1){
        next
      }
      sub <- substring(label_string, b, br)
      result_string <- append(result_string,
                              sub)
      b = b + br
    }

    result_string <- append(result_string,
                            substring(label_string, br+1, len))
    result_string <- unlist(result_string)
    result_string <- paste(result_string,
                           collapse='\n')
    strings_vector[i]<-result_string
  }

  return(strings_vector)
}
