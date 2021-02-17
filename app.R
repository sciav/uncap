library(shiny)
library(shinyjs)
library(DT)
load("reapp.RData")

source("apportion.R")
source("chooser.R")
states <- states[complete.cases(states), ]
min_seats <- 50
max_seats <- 1e5

ui <- fluidPage(
    useShinyjs(),
    titlePanel("Reapportionment Simulator"),
    sidebarLayout(
        sidebarPanel(
            numericInput("seats",
                         sprintf("Number of House seats (%d-%d)",
                                 min_seats,
                                 max_seats),
                         435,
                         min = min_seats,
                         max = max_seats),
            selectInput("year",
                        "Population Year",
                        choices = sort(unique(states[["Year"]])),
                        selected = "2010"),
            uiOutput("add_state"),
            "Population data from:",
            HTML("<a href = https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html>www.census.gov</a>")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Apportionment",
                         "Apportionment formula:",
                         HTML("<a href = https://www.everycrsreport.com/files/20130802_R41357_06aeca1ea6c97545c522ec0aedf047b705d37f69.html>www.everycrsreport.com</a>"),
                         DT::dataTableOutput("apportionment")),
                tabPanel("Historical Election Results",
                         selectInput("election",
                                     "Election Year",
                                     choices = unique(pres_results[["Year"]]),),
                         uiOutput("award_pr"),
                         verbatimTextOutput("election_results"),
                         DT::dataTableOutput("er"),
                         "Election data from:",
                         HTML("<a href = https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX>dataverse.harvard.edu</a>")
                         ),
                tabPanel("Election Simulator",
                         uiOutput("chooser"),
                         verbatimTextOutput("custom_election")
                         )


            ))
    )
)
server <- function(input, output, session) {
    state_data <- reactive({
        year <- input[["year"]]
        droplevels(states[states[["Year"]] == year, ])
    })
    election_results <- reactive({
        year <- input[["election"]]
        pres_results[pres_results[["Year"]] == year, ]
    })
    app <- reactive({
        apportion(states = state_data(),
                     nseats = min(max(input[["seats"]], min_seats, na.rm = TRUE), max_seats),
                     new_states = input[["new_states"]])
    })
    output[["add_state"]] <- renderUI({
        # ch <- setdiff(state_data()[["State"]], state_list)
        ch <- setdiff(states[["State"]], state_list)
        checkboxGroupInput("new_states", label = "New States", choices = ch)
    })
    output[["apportionment"]] <- DT::renderDataTable({
        DT::datatable(app(),
                      extensions = c("FixedHeader", "Buttons"),
                      rownames = FALSE,
                      options = list(
                          pageLength = nrow(app()),
                          paging = TRUE,
                          searching = TRUE,
                          fixedColumns = TRUE,
                          fixedHeader = TRUE,
                          autoWidth = TRUE,
                          ordering = TRUE,
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'print')
                      )) %>%
            formatPercentage(c("Population %", "Seat %", "Electoral Vote %", "Rep Factor", "EV Factor"), digits = 2) %>%
            formatRound("Population", mark = ",", digits = 0)
    })
    output[["election_results"]] <- renderPrint({
        prw <- input[["pr_win"]]
        get_ev_results(election_results(), app(), prw)
    })
    output[["award_pr"]] <- renderUI({
        if ("Puerto Rico" %in% input[["new_states"]]) {
            choices <- sort(unique(election_results()[["Winner"]]))
            radioButtons("pr_win", "Award Puerto Rico to:", choices, inline = TRUE)
        }
    })
    output[["chooser"]] <- renderUI({
        er <- election_results()
        if ("Puerto Rico" %in% input[["new_states"]]) {
            new_dat <- list(Year = 0, State = "Puerto Rico", Winner = min(er[["Winner"]]))
            er <- rbind(er, as.data.frame(new_dat))
        }
        st <- with(er, split(State, Winner))
        candidate <- names(st)
        chooserInput("assign_state", candidate[[1]], candidate[[2]],
                     st[[1]],
                     st[[2]],
                     size = 20,
                     multiple = TRUE)
    })

    output[["custom_election"]] <- renderPrint({
        er <- election_results()
        st <- with(er, split(State, Winner))
        st[[1]] <- c(st[[1]], "Puerto Rico")
        candidate <- names(st)

        l <- input[["assign_state"]][["left"]]
        r <- input[["assign_state"]][["right"]]
        er2 <- data.frame(State = c(l, r),
                         Winner = ifelse(c(l, r) %in% l,
                                         candidate[[1]],
                                         candidate[[2]]))
        get_ev_results(er2, app(), NULL)
        })

}

shinyApp(ui = ui, server = server)
