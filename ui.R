library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(reshape2)


## =========================================================
## This section reads in data and does all the calculations
#
# UMMAP tracker data; maintained by Stephen Campbell
# tracker <- read_xlsx("data/Progress Tracker.xlsx")
# # 
# # # counting key performance indicators
# # enroll_n <- sum(!is.na(tracker$Participant))
# mriShedule_n <- sum(!is.na(as.integer(tracker$`MRI scheduled`)))
# mriComplete_n <- sum(!is.na(as.integer(tracker$`MRI completed`)))
# bloodComplete_n <- sum(!is.na(as.integer(tracker$`Blood completed`)))
# 
# # binning dates in to month and week
# # create variables of the week and month of each observation:
# # inspired by http://blog.mollietaylor.com/2013/08/plot-weekly-or-monthly-totals-in-r.html
# tracker$Month <- as.Date(cut(tracker$`UMMAP_VisitDate`, breaks = "month"))
# tracker$Week <- as.Date(cut(tracker$`UMMAP_VisitDate`, breaks = "week", start.on.monday = FALSE))
# 
# # summarizing into tables first; warnings happening in coercion
# tracker_tbl <- tracker %>%
#   group_by(Month) %>%
#   summarise_all(funs(sum(!is.na(as.integer(.)))))
# 
# # trackertst_tbl <- tracker %>%
# #   group_by(Month) %>%
# #   summarise_if(is.numeric, n_distinct, na.rm = TRUE)


# header for MADC
# TODO: need a rectangular logo
# ui <- 
shinyUI(dashboardPage(
  dashboardHeader(title = tags$a(href='http://alzheimers.med.umich.edu/',
                               tags$img(src='MADC-horizontal-white.png', height='60', width='180'))),

  # sidebar
  # side <- 
  dashboardSidebar(
    # defining sidebar width
    width = 180,
    sidebarMenu(
      # two sidebar menus right now
      menuItem("Patient Enrollment", tabName = "enroll_dashboard", icon = icon("dashboard")),
      menuItem("Summary Table", tabName = "summary_table", icon = icon("th"))
    )
  ),
  
  
  # body
  # body <- 
  dashboardBody( tabItems(
  
    # First tab: Patient Enrollment
    tabItem(
      tabName = "enroll_dashboard",
  
      # 1st row: key performance indices
      fluidRow(
        # A static infoBox
        valueBoxOutput("enroll_n", width = 3),
        valueBoxOutput("mriShedule_n", width = 3),
        valueBoxOutput("mriComplete_n", width = 3),
        valueBoxOutput("bloodComplete_n", width = 3)
        # valueBox(
        #   enroll_n,
        #   "Current Enrollment",
        #   icon = icon("users"),
        #   color = "purple",
        #   width = 3
        # ),
        # valueBox(
        #   mriShedule_n,
        #   "MRI Scheduled",
        #   icon = icon("user-circle", lib = "font-awesome"),
        #   color = "blue",
        #   width = 3
        # ),
        # valueBox(
        #   mriComplete_n,
        #   "MRI Completed",
        #   icon = icon("user", lib = "glyphicon"),
        #   color = "green",
        #   width = 3
        # ),
        # valueBox(
        #   bloodComplete_n,
        #   "Blood Completed",
        #   icon = icon("tint", lib = "font-awesome"),
        #   color = "red",
        #   width = 3
        # )
  
        # Dynamic infoBoxes
        # infoBoxOutput("enrollmentBox"),
        # infoBoxOutput("MRIScheBox"),
        # infoBoxOutput("MRICompBox")
      ),
  
      # 2nd row: weekly/monthly enrollment line graphs
      fluidRow(
        box(
          title = "UMMAP weekly/monthly enrollment graphs",
          status = "primary",
          solidHeader = TRUE,
          plotOutput("plot1", height = 400),
          width = 8
        ),
  
        # date range input panel & choice selections for week/month plots
        box(
          title = "Date ranges to view",
          width = 4,
          dateRangeInput(
            'dateRange',
            label = 'Date range input: yyyy-mm-dd',
            start = "2017-03-01",
            end = Sys.Date()
          ),
  
          radioButtons(
            "radio",
            label = h4("Monthly or weekly view"),
            choices = list("Monthly view" = 1, "Weekly view" = 2),
            selected = 1
          )
        )
      ),
  
      # temp row:
      fluidRow(
        box(
          title = "Enrollment Trend",
          sliderInput("slider", "Number of observations:", 1, 50, 40),
          width = 4
        )
      ),
      #sliderInput("range", "Range:", min = 1, max = 1000, value = c(200,500)))),
  
      # 3rd row: individual patient raw data table
      # inspired by this post: https://mgiglia.solutions/shiny/msa8215_1/
      fluidRow(
        box(
          title = "Individual patient data veiwer",
          width = 12,
          dataTableOutput('individual_data')
        )
      )
    ),
  
      # Second tab content
      tabItem(
        tabName = "summary_table",
  
        fluidRow(
          box(
            title = "Enrollment Summary Table",
            width = 12,
            dataTableOutput('Freq_table')
            )
          )
        )
    )
    )
  )
  )
# dashboardPage(
#   head,
#   side,
#   body
# )
