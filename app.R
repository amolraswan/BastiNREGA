library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(writexl)

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
    checkboxInput("chk_muster",
                  "Scrape muster roll details (work names & 2nd photo, ~6 min)",
                  value = FALSE),
    actionButton("btn_load", "Load Data", class = "btn-primary w-100"),
    hr(),
    uiOutput("status_msg"),
    uiOutput("dl_zip_ui")
  ),

  nav_panel(
    "By Work Code",
    downloadButton("dl_workcode", "Download Excel", class = "btn-sm btn-outline-primary mb-2"),
    DT::dataTableOutput("tbl_workcode")
  ),
  nav_panel(
    "By Panchayat",
    downloadButton("dl_panchayat", "Download Excel", class = "btn-sm btn-outline-primary mb-2"),
    DT::dataTableOutput("tbl_panchayat")
  ),
  nav_panel(
    "Panchayat Detail",
    fluidRow(
      column(4, selectInput("sel_block", "Block:", choices = NULL)),
      column(4, selectInput("sel_panchayat", "Panchayat:", choices = NULL))
    ),
    downloadButton("dl_drilldown", "Download Excel", class = "btn-sm btn-outline-primary mb-2"),
    DT::dataTableOutput("tbl_drilldown")
  )
)

# -- Server ----------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(data = NULL, date_label = NULL, html_dir = NULL)

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

    can_reuse <- FALSE
    if (file.exists(csv_path)) {
      if (input$chk_muster) {
        existing <- read.csv(csv_path, stringsAsFactors = FALSE, nrows = 5)
        can_reuse <- "Has_Second_Photo" %in% names(existing) &&
                     !all(is.na(existing$Has_Second_Photo))
      } else {
        can_reuse <- TRUE
      }
    }

    if (can_reuse) {
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
      do_scrape(dd, mm, yyyy, input$chk_muster)
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
    if (!"Work_Name" %in% names(df)) df$Work_Name <- NA_character_
    if (!"Has_Second_Photo" %in% names(df)) df$Has_Second_Photo <- NA
    rv$data <- df
    rv$date_label <- paste0(dd, "/", mm, "/", yyyy)
    rv$html_dir <- NULL
    output$status_msg <- renderUI(
      tags$span(style = "color:green;",
                paste0("Loaded existing data: ", nrow(df), " rows."))
    )
  })

  # ---- Re-scrape ----
  observeEvent(input$btn_rescrape, {
    removeModal()
    do_scrape(trimws(input$dd), trimws(input$mm), trimws(input$yyyy), input$chk_muster)
  })

  # ---- Scrape function ----
  do_scrape <- function(dd, mm, yyyy, scrape_musters = FALSE) {
    output$status_msg <- renderUI(tags$span(style = "color:blue;", "Scraping in progress..."))

    withProgress(message = "Scraping NREGA data...", value = 0, {
      result <- scrape_basti_data(dd, mm, yyyy,
                                  scrape_musters = scrape_musters,
                                  progress_callback = function(val, msg) {
        setProgress(value = val, detail = msg)
      })
    })

    if (result$success) {
      fname <- save_data(result$data, sprintf("%02d", as.integer(dd)),
                         sprintf("%02d", as.integer(mm)), yyyy)
      rv$data <- result$data
      rv$date_label <- paste0(dd, "/", mm, "/", yyyy)
      rv$html_dir <- result$html_dir
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
        Work_Name_raw = first(na.omit(Work_Name)),
        Mustroll_Nos = paste0(
          '<a href="', Mustroll_Link, '" target="_blank">', Mustroll_No, '</a>'
        ) %>% paste(collapse = ", "),
        `2nd Photo Mustrolls` = {
          photo_rolls <- Mustroll_No[!is.na(Has_Second_Photo) & Has_Second_Photo == TRUE]
          if (length(photo_rolls) == 0) "-" else paste(photo_rolls, collapse = ", ")
        },
        Persondays = sum(Persondays, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        `Work Code` = paste0(Work_Code, '<br><small>',
                             ifelse(is.na(Work_Name_raw), "", Work_Name_raw),
                             '</small>')
      ) %>%
      select(Block, Panchayat, `Work Code`,
             `Mustroll No(s)` = Mustroll_Nos,
             `2nd Photo Mustrolls`,
             Persondays) %>%
      mutate(Block = factor(Block), Panchayat = factor(Panchayat))

    datatable(df, escape = FALSE, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE,
                             order = list(list(5, "desc")),
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
        `2nd Photo` = paste0(
          sum(Has_Second_Photo == TRUE, na.rm = TRUE), "/", n()
        ),
        `Total Persondays` = sum(Persondays, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Block = factor(Block), Panchayat = factor(Panchayat))

    datatable(df, escape = FALSE, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE,
                             order = list(list(3, "desc")),
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
        Work_Name_raw = first(na.omit(Work_Name)),
        `Mustroll No(s)` = paste0(
          '<a href="', Mustroll_Link, '" target="_blank">', Mustroll_No, '</a>'
        ) %>% paste(collapse = ", "),
        `2nd Photo Mustrolls` = {
          photo_rolls <- Mustroll_No[!is.na(Has_Second_Photo) & Has_Second_Photo == TRUE]
          if (length(photo_rolls) == 0) "-" else paste(photo_rolls, collapse = ", ")
        },
        Persondays = sum(Persondays, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        `Work Code` = paste0(Work_Code, '<br><small>',
                             ifelse(is.na(Work_Name_raw), "", Work_Name_raw),
                             '</small>')
      ) %>%
      select(`Work Code`, `Mustroll No(s)`, `2nd Photo Mustrolls`, Persondays)

    datatable(df, escape = FALSE, rownames = FALSE,
              options = list(pageLength = 50, scrollX = TRUE,
                             order = list(list(3, "desc"))),
              class = "compact stripe hover")
  })

  # ==== Excel Downloads ====================================================

  output$dl_workcode <- downloadHandler(
    filename = function() {
      paste0("by_workcode_", gsub("/", "-", rv$date_label), ".xlsx")
    },
    content = function(file) {
      df <- rv$data %>%
        group_by(Block, Panchayat, Work_Code) %>%
        summarise(
          Work_Name = first(na.omit(Work_Name)),
          `Mustroll No(s)` = paste(Mustroll_No, collapse = ", "),
          `2nd Photo Mustrolls` = {
            photo_rolls <- Mustroll_No[!is.na(Has_Second_Photo) & Has_Second_Photo == TRUE]
            if (length(photo_rolls) == 0) "-" else paste(photo_rolls, collapse = ", ")
          },
          Persondays = sum(Persondays, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(Persondays))
      write_xlsx(df, file)
    }
  )

  output$dl_panchayat <- downloadHandler(
    filename = function() {
      paste0("by_panchayat_", gsub("/", "-", rv$date_label), ".xlsx")
    },
    content = function(file) {
      df <- rv$data %>%
        group_by(Block, Panchayat) %>%
        summarise(
          `2nd Photo` = paste0(sum(Has_Second_Photo == TRUE, na.rm = TRUE), "/", n()),
          `Total Persondays` = sum(Persondays, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(`Total Persondays`))
      write_xlsx(df, file)
    }
  )

  output$dl_drilldown <- downloadHandler(
    filename = function() {
      paste0("detail_", input$sel_block, "_", input$sel_panchayat, "_",
             gsub("/", "-", rv$date_label), ".xlsx")
    },
    content = function(file) {
      df <- rv$data %>%
        filter(Block == input$sel_block, Panchayat == input$sel_panchayat) %>%
        group_by(Work_Code) %>%
        summarise(
          Work_Name = first(na.omit(Work_Name)),
          `Mustroll No(s)` = paste(Mustroll_No, collapse = ", "),
          `2nd Photo Mustrolls` = {
            photo_rolls <- Mustroll_No[!is.na(Has_Second_Photo) & Has_Second_Photo == TRUE]
            if (length(photo_rolls) == 0) "-" else paste(photo_rolls, collapse = ", ")
          },
          Persondays = sum(Persondays, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(Persondays))
      write_xlsx(df, file)
    }
  )

  # ==== HTML ZIP Download ==================================================

  output$dl_zip_ui <- renderUI({
    req(rv$html_dir)
    downloadButton("dl_zip", "Download HTML Copies (ZIP)",
                   class = "btn-sm btn-outline-secondary w-100 mt-2")
  })

  output$dl_zip <- downloadHandler(
    filename = function() {
      paste0("BASTI_", gsub("/", "-", rv$date_label), ".zip")
    },
    content = function(file) {
      html_files <- list.files(rv$html_dir, pattern = "\\.html$", full.names = TRUE)
      zip(file, html_files, flags = "-j")
    },
    contentType = "application/zip"
  )

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
