library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(curl)

BASE_URL <- "https://mnregaweb4.nic.in/nregaarch/"

UA <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")

get_financial_year <- function(date) {
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m"))
  if (m >= 4) {
    paste0(y, "-", y + 1)
  } else {
    paste0(y - 1, "-", y)
  }
}

extract_asp_fields <- function(html) {
  form <- html_node(html, "form#aspnetForm")
  if (is.na(form)) form <- html_node(html, "form")
  fields <- list()
  for (inp in html_nodes(form, "input[type='hidden']")) {
    nm  <- html_attr(inp, "name")
    val <- html_attr(inp, "value")
    if (!is.na(nm)) fields[[nm]] <- if (is.na(val)) "" else val
  }
  for (sel in html_nodes(form, "select")) {
    nm <- html_attr(sel, "name")
    if (is.na(nm)) next
    opt <- html_node(sel, "option[selected]")
    if (is.na(opt)) opt <- html_node(sel, "option")
    if (!is.na(opt)) {
      val <- html_attr(opt, "value")
      fields[[nm]] <- if (is.na(val)) "" else val
    }
  }
  fields
}

extract_form_action <- function(html, base_url) {
  form <- html_node(html, "form#aspnetForm")
  if (is.na(form)) form <- html_node(html, "form")
  action <- html_attr(form, "action")
  if (is.na(action) || !nzchar(action)) return(base_url)
  if (grepl("^https?://", action)) return(action)
  base <- sub("/[^/]*$", "/", base_url)
  paste0(base, sub("^\\./", "", action))
}

validate_date <- function(dd, mm, yyyy) {
  tryCatch({
    d <- as.Date(paste(yyyy, mm, dd, sep = "-"))
    if (is.na(d)) return("Invalid date.")
    if (d > Sys.Date()) return("Date is in the future.")
    if (as.numeric(Sys.Date() - d) > 14) return("Date must be within the past 14 days.")
    return(NULL)
  }, error = function(e) "Invalid date.")
}

save_data <- function(df, dd, mm, yyyy) {
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
  fname <- file.path("data", paste0("data_", dd, mm, yyyy, ".csv"))
  write.csv(df, fname, row.names = FALSE)
  fname
}

scrape_muster_details <- function(df, progress_callback = NULL) {
  n <- nrow(df)
  batch_size <- 20
  total_batches <- ceiling(n / batch_size)

  work_names <- rep(NA_character_, n)
  has_second_photo <- rep(NA, n)

  for (b in seq_len(total_batches)) {
    idx_start <- (b - 1) * batch_size + 1
    idx_end <- min(b * batch_size, n)
    batch_idx <- idx_start:idx_end

    pool <- new_pool(total_con = 20, host_con = 20)

    for (i in batch_idx) {
      url <- df$Mustroll_Link[i]
      if (is.na(url) || url == "") next

      local({
        ii <- i
        h <- new_handle()
        handle_setheaders(h,
          "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
        handle_setopt(h, timeout = 30)

        curl_fetch_multi(url, done = function(resp) {
          tryCatch({
            html <- read_html(rawToChar(resp$content))

            # Extract work name
            wn_node <- html_node(html, "span#ContentPlaceHolder1_lbl_dtl")
            if (!is.na(wn_node)) {
              wn_text <- html_text2(wn_node)
              wn <- trimws(sub(".*Work Name\\s*:\\s*", "", wn_text))
              work_names[ii] <<- wn
            }

            # Extract second photo status
            sp_node <- html_node(html, "span#ContentPlaceHolder1_Lblsecond_photo_status")
            has_second_photo[ii] <<- is.na(sp_node)
          }, error = function(e) {
            # leave as NA on parse error
          })
        }, fail = function(msg) {
          # leave as NA on network error
        }, pool = pool, handle = h)
      })
    }

    multi_run(pool = pool)

    if (!is.null(progress_callback)) {
      progress_callback(b, total_batches)
    }
  }

  df$Work_Name <- work_names
  df$Has_Second_Photo <- has_second_photo
  df
}

scrape_basti_data <- function(dd, mm, yyyy, scrape_musters = FALSE, progress_callback = NULL) {

  notify <- function(val, msg) {
    if (!is.null(progress_callback)) progress_callback(val, msg)
  }

  date_str <- paste0(
    sprintf("%02d", as.integer(dd)), "/",
    sprintf("%02d", as.integer(mm)), "/",
    yyyy
  )

  target_date <- as.Date(paste(yyyy, mm, dd, sep = "-"))
  fin_year <- get_financial_year(target_date)

  h <- handle("https://mnregaweb4.nic.in")

  # ---- Step 1: Get NMMS link from homepage ----
  notify(0.05, "Fetching NREGA homepage...")
  home_resp <- tryCatch(
    GET("https://nrega.dord.gov.in/MGNREGA_new/Nrega_home.aspx", UA, timeout(60)),
    error = function(e) NULL
  )
  if (is.null(home_resp) || status_code(home_resp) != 200) {
    return(list(success = FALSE, error = "Could not reach NREGA homepage."))
  }

  home_html <- read_html(content(home_resp, "text", encoding = "UTF-8"))
  att_node <- html_node(home_html, xpath = "//a[contains(text(), 'View Daily attendance')]")
  if (is.na(att_node)) {
    return(list(success = FALSE, error = "Could not find the NMMS attendance link on the homepage."))
  }
  nmms_url <- sub("^http://", "https://", html_attr(att_node, "href"))
  notify(0.10, "Found NMMS attendance link.")

  # ---- Step 2: GET the NMMS attendance page ----
  notify(0.15, "Loading attendance page...")
  page_resp <- tryCatch(
    GET(nmms_url, handle = h, UA, timeout(60)),
    error = function(e) NULL
  )
  if (is.null(page_resp) || status_code(page_resp) != 200) {
    return(list(success = FALSE, error = "Could not load the NMMS attendance page."))
  }
  page_html <- read_html(content(page_resp, "text", encoding = "UTF-8"))
  post_url  <- extract_form_action(page_html, nmms_url)
  nmms_base <- sub("/[^/?#]*([?#].*)?$", "/", nmms_url)
  fields    <- extract_asp_fields(page_html)

  # Override state and date, then submit directly — both dropdowns are already
  # populated on the initial page, so intermediate PostBacks are not needed.
  fields[["ctl00$ContentPlaceHolder1$ddlstate"]]       <- "31"
  fields[["ctl00$ContentPlaceHolder1$ddl_attendance"]] <- date_str
  fields[["__EVENTTARGET"]]   <- ""
  fields[["__EVENTARGUMENT"]] <- ""

  notify(0.35, paste0("Submitting form for ", date_str, "..."))
  post3_resp <- tryCatch(
    POST(post_url, handle = h, UA, timeout(60),
         body = c(fields, list(
           `ctl00$ContentPlaceHolder1$btn_showreport` = "Show Attendance"
         )),
         encode = "form"),
    error = function(e) NULL
  )
  if (is.null(post3_resp) || status_code(post3_resp) != 200) {
    return(list(success = FALSE, error = "Failed to submit the attendance form."))
  }
  page4_html <- read_html(content(post3_resp, "text", encoding = "UTF-8"))

  # ---- Step 3: Find UTTAR PRADESH link ----
  notify(0.55, "Finding UTTAR PRADESH link...")
  up_node <- html_node(page4_html, xpath = "//a[contains(text(), 'UTTAR PRADESH')]")
  if (is.na(up_node)) {
    writeLines(as.character(page4_html), "/tmp/debug_page4.html")
    return(list(success = FALSE, error = "UTTAR PRADESH link not found in results. The date may not have data. Debug HTML saved to /tmp/debug_page4.html"))
  }
  up_href <- html_attr(up_node, "href")
  up_url <- gsub(" ", "%20", paste0(BASE_URL, up_href))

  # ---- Step 4: GET state page, find BASTI detail link ----
  notify(0.65, "Loading state page...")
  state_resp <- tryCatch(
    GET(up_url, handle = h, UA, timeout(60)),
    error = function(e) NULL
  )
  if (is.null(state_resp) || status_code(state_resp) != 200) {
    return(list(success = FALSE, error = "Could not load the UTTAR PRADESH state page."))
  }
  state_html <- read_html(content(state_resp, "text", encoding = "UTF-8"))

  basti_nodes <- html_nodes(state_html, "a")
  basti_href <- NULL
  for (node in basti_nodes) {
    href <- html_attr(node, "href")
    if (!is.na(href) &&
        grepl("View_NMMS_atten_date_dtl.aspx", href, fixed = TRUE) &&
        grepl("BASTI", href, fixed = TRUE)) {
      basti_href <- href
      break
    }
  }
  if (is.null(basti_href)) {
    return(list(success = FALSE, error = "BASTI district link not found on the state page."))
  }
  basti_url <- gsub(" ", "%20", paste0(BASE_URL, basti_href))
  notify(0.75, "Found BASTI link. Loading detail page...")

  # ---- Step 5: GET Basti detail page, parse table ----
  detail_resp <- tryCatch(
    GET(basti_url, handle = h, UA, timeout(120)),
    error = function(e) NULL
  )
  if (is.null(detail_resp) || status_code(detail_resp) != 200) {
    return(list(success = FALSE, error = "Could not load the BASTI detail page."))
  }
  detail_html <- read_html(content(detail_resp, "text", encoding = "UTF-8"))

  notify(0.85, "Parsing data table...")
  tbl <- html_element(detail_html, "table.table-bordered")
  if (is.na(tbl)) {
    return(list(success = FALSE, error = "No data table found on the BASTI detail page."))
  }
  all_rows <- html_elements(tbl, "tr")
  # Skip header rows (first 2 rows: column names + column numbers)
  if (length(all_rows) <= 2) {
    return(list(success = FALSE, error = "Data table has no data rows."))
  }
  rows <- all_rows[-(1:2)]

  records <- lapply(rows, function(row) {
    tds <- html_elements(row, "td")
    if (length(tds) < 7) return(NULL)
    link_node <- html_element(tds[6], "a")
    mustroll_no   <- trimws(html_text2(link_node))
    mustroll_href <- html_attr(link_node, "href")
    mustroll_link <- if (!is.na(mustroll_href)) gsub(" ", "%20", paste0(BASE_URL, mustroll_href)) else NA_character_
    data.frame(
      District      = trimws(html_text2(tds[2])),
      Block         = trimws(html_text2(tds[3])),
      Panchayat     = trimws(html_text2(tds[4])),
      Work_Code     = trimws(html_text2(tds[5])),
      Mustroll_No   = mustroll_no,
      Mustroll_Link = mustroll_link,
      Persondays    = as.numeric(trimws(html_text2(tds[7]))),
      stringsAsFactors = FALSE
    )
  })

  df <- bind_rows(records)
  if (nrow(df) == 0) {
    return(list(success = FALSE, error = "Parsed table was empty."))
  }

  if (scrape_musters) {
    notify(0.90, "Fetching muster roll details (work names & photo status)...")
    df <- scrape_muster_details(df, progress_callback = function(batch_done, total_batches) {
      frac <- 0.90 + 0.09 * (batch_done / total_batches)
      notify(frac, paste0("Fetching muster details... batch ", batch_done, "/", total_batches))
    })
  } else {
    df$Work_Name <- NA_character_
    df$Has_Second_Photo <- NA
  }

  notify(1.0, paste0("Done! ", nrow(df), " rows scraped."))
  list(success = TRUE, data = df)
}
