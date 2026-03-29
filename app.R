library(shiny)
library(bslib)
library(DT)
library(dplyr)

source("R/scraper.R")

# -- UI -------------------------------------------------------------------

ui <- page_navbar(
  id = "main_nav",
  title = uiOutput("app_title", inline = TRUE),
  theme = bs_theme(bootswatch = "flatly"),
  header = tags$head(tags$script(src = "custom.js")),

  sidebar = sidebar(
    width = 300,
    h5("Select Date"),
    helpText("Enter a date from the past 14 days (DD / MM / YYYY)"),
    fluidRow(
      column(4, textInput("dd", "DD", placeholder = "DD")),
      column(4, textInput("mm", "MM", placeholder = "MM")),
      column(4, textInput("yyyy", "YYYY", placeholder = "YYYY"))
    ),
    actionButton("btn_load", "Load Data", class = "btn-primary w-100"),
    hr(),
    uiOutput("status_msg")
  ),

  nav_panel(
    "By Work Code",
    DT::dataTableOutput("tbl_workcode")
  ),
  nav_panel(
    "By Panchayat",
    DT::dataTableOutput("tbl_panchayat")
  ),
  nav_panel(
    "Panchayat Detail",
    fluidRow(
      column(4, selectInput("sel_block", "Block:", choices = NULL)),
      column(4, selectInput("sel_panchayat", "Panchayat:", choices = NULL))
    ),
    DT::dataTableOutput("tbl_drilldown")
  )
)

# -- Server ----------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(data = NULL, date_label = NULL)

  output$app_title <- renderUI({
    if (is.null(rv$date_label)) {
      "Basti NREGA Daily Person-Days"
    } else {
      paste0("Basti NREGA Daily Person-Days ", rv$date_label)
    }
  })

  # ---- Load button ----
  observeEvent(input$btn_load, {
    dd   <- trimws(input$dd)
    mm   <- trimws(input$mm)
    yyyy <- trimws(input$yyyy)

    # basic presence check
    if (dd == "" || mm == "" || yyyy == "") {
      output$status_msg <- renderUI(tags$span(style = "color:red;", "Please fill in all date fields."))
      return()
    }

    err <- validate_date(dd, mm, yyyy)
    if (!is.null(err)) {
      output$status_msg <- renderUI(tags$span(style = "color:red;", err))
      return()
    }

    date_tag <- paste0(sprintf("%02d", as.integer(dd)),
                       sprintf("%02d", as.integer(mm)),
                       yyyy)
    csv_path <- file.path("data", paste0("data_", date_tag, ".csv"))

    if (file.exists(csv_path)) {
      showModal(modalDialog(
        title = "Existing Data Found",
        paste0("A data file for ", dd, "/", mm, "/", yyyy, " already exists."),
        footer = tagList(
          actionButton("btn_use_existing", "Use Existing"),
          actionButton("btn_rescrape", "Re-scrape"),
          modalButton("Cancel")
        )
      ))
    } else {
      do_scrape(dd, mm, yyyy)
    }
  })

  # ---- Use existing CSV ----
  observeEvent(input$btn_use_existing, {
    removeModal()
    dd   <- trimws(input$dd)
    mm   <- trimws(input$mm)
    yyyy <- trimws(input$yyyy)
    date_tag <- paste0(sprintf("%02d", as.integer(dd)),
                       sprintf("%02d", as.integer(mm)),
                       yyyy)
    csv_path <- file.path("data", paste0("data_", date_tag, ".csv"))
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
    rv$data <- df
    rv$date_label <- paste0(dd, "/", mm, "/", yyyy)
    output$status_msg <- renderUI(
      tags$span(style = "color:green;",
                paste0("Loaded existing data: ", nrow(df), " rows."))
    )
  })

  # ---- Re-scrape ----
  observeEvent(input$btn_rescrape, {
    removeModal()
    do_scrape(trimws(input$dd), trimws(input$mm), trimws(input$yyyy))
  })

  # ---- Scrape function ----
  do_scrape <- function(dd, mm, yyyy) {
    output$status_msg <- renderUI(tags$span(style = "color:blue;", "Scraping in progress..."))

    withProgress(message = "Scraping NREGA data...", value = 0, {
      result <- scrape_basti_data(dd, mm, yyyy, progress_callback = function(val, msg) {
        setProgress(value = val, detail = msg)
      })
    })

    if (result$success) {
      fname <- save_data(result$data, sprintf("%02d", as.integer(dd)),
                         sprintf("%02d", as.integer(mm)), yyyy)
      rv$data <- result$data
      rv$date_label <- paste0(dd, "/", mm, "/", yyyy)
      output$status_msg <- renderUI(
        tags$span(style = "color:green;",
                  paste0("Scraped ", nrow(result$data), " rows. Saved to ", fname))
      )
    } else {
      output$status_msg <- renderUI(
        tags$span(style = "color:red;", paste0("Error: ", result$error))
      )
    }
  }

  # ==== Section 1: Work Code Level ========================================

  output$tbl_workcode <- DT::renderDataTable({
    req(rv$data)
    df <- rv$data %>%
      group_by(Block, Panchayat, Work_Code) %>%
      summarise(
        Mustroll_Nos = paste0(
          '<a href="', Mustroll_Link, '" target="_blank">', Mustroll_No, '</a>'
        ) %>% paste(collapse = ", "),
        Persondays = sum(Persondays, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      select(Block, Panchayat,
             `Work Code` = Work_Code,
             `Mustroll No(s)` = Mustroll_Nos,
             Persondays) %>%
      mutate(Block = factor(Block), Panchayat = factor(Panchayat),
             `Work Code` = factor(`Work Code`))

    datatable(df, escape = FALSE, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE,
                             order = list(list(4, "desc")),
                             columnDefs = list(list(
                               targets = 1,
                               render = DT::JS(
                                 "function(data, type, row, meta) {",
                                 "  if (type !== 'display') return data;",
                                 "  var block = row[0];",
                                 "  return '<a href=\"#\" class=\"panchayat-link\" data-block=\"' + block + '\" data-panchayat=\"' + data + '\">' + data + '</a>';",
                                 "}")
                             ))),
              class = "compact stripe hover")
  })

  # ==== Section 2: Panchayat Level ========================================

  output$tbl_panchayat <- DT::renderDataTable({
    req(rv$data)
    df <- rv$data %>%
      group_by(Block, Panchayat) %>%
      summarise(
        `Total Persondays` = sum(Persondays, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Block = factor(Block), Panchayat = factor(Panchayat))

    datatable(df, escape = FALSE, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE,
                             order = list(list(2, "desc")),
                             columnDefs = list(list(
                               targets = 1,
                               render = DT::JS(
                                 "function(data, type, row, meta) {",
                                 "  if (type !== 'display') return data;",
                                 "  var block = row[0];",
                                 "  return '<a href=\"#\" class=\"panchayat-link\" data-block=\"' + block + '\" data-panchayat=\"' + data + '\">' + data + '</a>';",
                                 "}")
                             ))),
              class = "compact stripe hover")
  })

  # ==== Section 3: Panchayat Drill-down ===================================

  # Update block choices when data loads
  observe({
    req(rv$data)
    blocks <- sort(unique(rv$data$Block))
    updateSelectInput(session, "sel_block",
                      choices = c("Select block..." = "", blocks))
  })

  # Cascade: update panchayat when block changes
  observeEvent(input$sel_block, {
    if (is.null(input$sel_block) || input$sel_block == "") {
      updateSelectInput(session, "sel_panchayat", choices = c("Select panchayat..." = ""))
      return()
    }
    panchs <- rv$data %>%
      filter(Block == input$sel_block) %>%
      pull(Panchayat) %>% unique() %>% sort()
    updateSelectInput(session, "sel_panchayat",
                      choices = c("Select panchayat..." = "", panchs))
  })

  output$tbl_drilldown <- DT::renderDataTable({
    req(input$sel_block, input$sel_panchayat)
    req(input$sel_block != "", input$sel_panchayat != "")
    df <- rv$data %>%
      filter(Block == input$sel_block, Panchayat == input$sel_panchayat) %>%
      group_by(Work_Code) %>%
      summarise(
        `Mustroll No(s)` = paste0(
          '<a href="', Mustroll_Link, '" target="_blank">', Mustroll_No, '</a>'
        ) %>% paste(collapse = ", "),
        Persondays = sum(Persondays, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      select(`Work Code` = Work_Code, `Mustroll No(s)`, Persondays)

    datatable(df, escape = FALSE, rownames = FALSE,
              options = list(pageLength = 50, scrollX = TRUE,
                             order = list(list(2, "desc"))),
              class = "compact stripe hover")
  })

  # ==== Cross-tab navigation ==============================================

  observeEvent(input$navigate_to_panchayat, {
    info <- input$navigate_to_panchayat

    # First update block, then wait for panchayat choices to populate,
    # then set panchayat, then switch tab
    updateSelectInput(session, "sel_block", selected = info$block)

    # Need two flushes: first for block to update, which triggers the
    # observeEvent that populates panchayat choices; second to set panchayat
    session$onFlushed(function() {
      session$onFlushed(function() {
        updateSelectInput(session, "sel_panchayat", selected = info$panchayat)
      }, once = TRUE)
    }, once = TRUE)

    nav_select("main_nav", "Panchayat Detail")
  })
}

shinyApp(ui, server)
