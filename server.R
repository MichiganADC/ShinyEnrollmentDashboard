library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(reshape2)



# server <-
shinyServer(function(input, output) {

  library(scales)
  library(ggplot2)

  ## ========================================================= # This section
  #reads in data and does all the calculations
  #
  #UMMAP tracker data; maintained by Stephen Campbell
  #Example for reacitve() in Rshiny:
  #https://shiny.rstudio.com/gallery/kmeans-example.html
  tracker1 <- read_xlsx("data/Progress_Tracker_8-22-17.xlsx")

  # i_dateGe <- reactive({
  #   tracker1$`UMMAP_VisitDate` >= input$dateRange[1]
  # })
  # i_dateLe <- reactive({
  #   tracker1$`UMMAP_VisitDate` <= input$dateRange[2]
  # })


  tracker <- reactive({
    i_dateGe <- tracker1$`UMMAP_VisitDate` >= input$dateRange[1]
    i_dateLe <- tracker1$`UMMAP_VisitDate` <= input$dateRange[2]
    tracker1$Month <- as.Date(cut(tracker1$`UMMAP_VisitDate`, breaks = "month"))
    tracker1$Week <- as.Date(cut(tracker1$`UMMAP_VisitDate`, breaks = "week", start.on.monday = FALSE))
    tracker1[i_dateGe & i_dateLe,]
    })

  # counting key performance indicators

  output$enroll_n <- renderValueBox({
    valueBox(
      paste0(sum(!is.na(tracker()$Participant))),
      "Current Enrollment",
      icon = icon("users"),
      color = "purple"
      # , width = 3
      )})

  output$mriShedule_n <- renderValueBox({
    valueBox(
      paste0(sum(!is.na(as.integer(tracker()$`MRI scheduled`)))),
      "MRI Scheduled",
      icon = icon("user-circle", lib = "font-awesome"),
      color = "blue"
      # , width = 3
  )})

  output$mriComplete_n <- renderValueBox({
    valueBox(
      paste0(sum(!is.na(as.integer(tracker()$`MRI completed`)))),
      "MRI Completed",
      icon = icon("user", lib = "glyphicon"),
      color = "green"
      # ,width = 3
  )})

  output$bloodComplete_n <- renderValueBox({
    valueBox(
      paste0(sum(!is.na(as.integer(tracker()$`Blood completed`)))),
      "Blood Completed",
      icon = icon("tint", lib = "font-awesome"),
      color = "red"
      # , width = 3
    )
  })



  # output$enroll_n <- renderText{(paste0(sum(!is.na(tracker$Participant))))}
  # output$enroll_n <- sum(!is.na(tracker$Participant))
  # mriShedule_n <- sum(!is.na(as.integer(tracker$`MRI scheduled`)))
  # mriComplete_n <- sum(!is.na(as.integer(tracker$`MRI completed`)))
  # bloodComplete_n <- sum(!is.na(as.integer(tracker$`Blood completed`)))

  # binning dates in to month and week
  # create variables of the week and month of each observation:
  # inspired by http://blog.mollietaylor.com/2013/08/plot-weekly-or-monthly-totals-in-r.html


  # summarizing into tables first; warnings happening in coercion
  tracker_tbl <- reactive({
    tracker() %>%
      group_by(Month) %>%
      summarise_all(funs(sum(!is.na(as.integer(.)))))
  })


  ### Enrollment Data
  file_dir = "data"
  file_name = "UDS Enrollment 2016.xlsx"

  enrolldata <- read_xlsx(file.path(file_dir,file_name))

  test <- enrolldata
  # test <- enrolldata %>%
  # filter(is.na(`Date subject not interested in being contacted for other research`) & is.na(`Date subject no longer eligible for research`))

  # change Race factors other than White/Black to Other
  # inspired by https://stackoverflow.com/questions/24459752/can-dplyr-package-be-used-for-conditional-mutating
  # test <- test %>%
  #   mutate(Race = !Race %in% c("White", "Black"), )
  test$Race[!test$Race %in% c("White", "Black")] <- "Other"

  test$`UDS dx`[as.numeric(difftime(Sys.Date(), test$`Exam Date`)) < 90] <- "unavailable yet"

  # find the last date for each subject and then create new variable to test if it's a delayed visit or not
  # last date > 1.5 * 365 days is delayed
  # also exclude the "Drop cases"
  # inspired by https://blog.exploratory.io/dplyr-0-5-is-awesome-heres-why-be095fd4eb8a
  test <- test %>%
    group_by(`Subject ID`) %>%
    arrange(desc(`Exam Date`)) %>%
    mutate(delayed = ifelse(as.numeric(difftime(Sys.Date(), nth(`Exam Date`, 1))) > 1.5 * 365, "delayed", "non_delayed")) %>%
    mutate(death = as.numeric(!is.na(`Date of Death`))) %>%
    mutate(drop = as.numeric(!(is.na(`Date subject not interested in being contacted for other research`) & is.na(`Date subject no longer eligible for research`))))

  # Last visit of delayed Patients
  pt_delayed <- test %>%
    group_by(`Subject ID`) %>%
    arrange(desc(`Exam Date`)) %>%
    filter(`Exam Date`== nth(`Exam Date`, 1)) %>%
    filter(delayed=="delayed")

  # Use frequncy table to summary the delayed visits
  # freq_vals = c(`Sex`, `Race`)
  dx_sex <- pt_delayed %>%
    group_by(`UDS dx`, Sex) %>%
    summarise (n = n()) %>% #mutate(freq = n / sum(n)) %>%
    spread(Sex, n)

  dx_race <- pt_delayed %>%
    group_by(`UDS dx`, Race) %>%
    summarise (n = n()) %>%
    spread(Race, n)

  # Frequency table with dplyr
  test %>%
    group_by(`UDS dx`) %>%
    summarise(freq = n())

  # # A tibble: 13 x 2
  # `UDS dx`  freq
  # <chr> <int>
  #   1          Amnestic MCI-memory only    22
  # 2          Amnestic MCI-memory plus    41
  # 3              Dem with Lewy bodies     7
  # 4                               FTD     4
  # 5                 Impaired, not MCI    32
  # 6                                NL   279
  # 7 Non-Amnestic MCI-multiple domains     7
  # 8    Non-Amnestic MCI-single domain    17
  # 9                             Other     4
  # 10                       Possible AD     7
  # 11                       Probable AD    42
  # 12                      Vascular dem     2
  # 13                              <NA>    22

  # using melt/dcast to give me variables I need

  # melt needed variables
  test$death[test$death == 1] = "death"
  test$drop[test$drop == 1] = "drop"

  test$`Autopsy Approved?`[test$`Autopsy Approved?` == "Yes"] = "Autopsy_approved"
  test.m <- melt(test, id.vars = "UDS dx", measure.vars = c("Race","Sex","Autopsy Approved?","death","delayed", "drop"))
  test.m[is.na(test.m[,1]),1] = "unkown"

  freq_table <- dcast(test.m, `UDS dx` ~ value, length)

  # change the column order
  i_match = match(c("UDS dx", "Female",	"Male",	"Black",	"White",	"Other",
                    "Autopsy_approved",	"death",	"drop",
                    "delayed"), colnames(freq_table))
  freq_table <- freq_table[,i_match]
  freq_table$total = freq_table$Female + freq_table$Male


  #                     UDS dx Black delayed FALSE Female Male non_delayed Other TRUE White Yes  NA
  # 1 Amnestic MCI-memory only    10       8    22     12   10          14     0    0    12   5  17
  # 2 Amnestic MCI-memory plus    22      20    41     25   16          21     0    0    19   6  35
  # 3     Dem with Lewy bodies     1       4     6      1    6           3     0    1     6   1   6
  # 4                      FTD     0       3     2      0    4           1     0    2     4   2   2
  # 5        Impaired, not MCI    27      24    32     22   10           8     1    0     4   9  23
  # 6                       NL   130     145   277    221   58         134     2    2   147 124 155

  ## =========================================================
  ## This section starts rendering the Shiny app

  # generating line plot with ggplot
  # inspired by http://blog.mollietaylor.com/2013/08/plot-weekly-or-monthly-totals-in-r.html
  output$plot1 <- renderPlot({
    if(input$radio==1){
      ggplot(tracker_tbl(), aes(Month, UMMAP_VisitDate)) +
        geom_line(color = "red")
    } else if(input$radio==2){
      ggplot(tracker_tbl(), aes(Week, UMMAP_VisitDate)) +
        geom_line(color = "red")
    }

    # scale_x_date(breaks = "Month",
    #              labels = date_format("%m-%Y"),
    #              limits = input$dataRange)
  })

  # testing line plot example code
  # trenddata <- floor(sqrt(1:500))
  # output$plot1 <- renderPlot({
  #   data <- trenddata[seq_len(input$slider)]
  #   plot(data, type = 'l', ylab = '# Enrollment', xlab = 'day')
  # })

  # generating data table view
  output$individual_data <- renderDataTable({
    tracker()
  }, options = list(lengthMenu = c(15, 35, 50), pageLength = 15))

  # Code backup on dynamic InfoBox
  # output$MRICompBox <- renderInfoBox({
  #   infoBox("MRI Completed", paste0(25 + input$count), icon = icon("user", lib = "glyphicon"), color = "yellow")
  # })

  output$Freq_table <- renderDataTable({
    freq_table
  }, options = list(autoWidth = FALSE,lengthMenu = c(15, 35, 50), pageLength = 15))

})
