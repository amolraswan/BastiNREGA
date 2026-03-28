library(httr)
library(rvest)
library(xml2)
library(dplyr)

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
  get_val <- function(id) {
    node <- html_node(html, paste0("input[name='", id, "']"))
    if (is.na(node)) return("")
    val <- html_attr(node, "value")
    if (is.na(val)) "" else val
  }
  list(
    `__VIEWSTATE`          = get_val("__VIEWSTATE"),
    `__VIEWSTATEGENERATOR` = get_val("__VIEWSTATEGENERATOR"),
    `__EVENTVALIDATION`    = get_val("__EVENTVALIDATION")
  )
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

scrape_basti_data <- function(dd, mm, yyyy, progress_callback = NULL) {

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
  fields <- extract_asp_fields(page_html)

  # ---- Step 2a: POST — select UTTAR PRADESH (state code 31) ----
  notify(0.25, "Selecting UTTAR PRADESH...")
  post1_resp <- tryCatch(
    POST(nmms_url, handle = h, UA, timeout(60),
         body = c(fields, list(
           `__EVENTTARGET`    = "ctl00$ContentPlaceHolder1$ddlstate",
           `__EVENTARGUMENT`  = "",
           `ctl00$ContentPlaceHolder1$ddlstate` = "31"
         )),
         encode = "form"),
    error = function(e) NULL
  )
  if (is.null(post1_resp) || status_code(post1_resp) != 200) {
    return(list(success = FALSE, error = "Failed to select state."))
  }
  page2_html <- read_html(content(post1_resp, "text", encoding = "UTF-8"))
  fields2 <- extract_asp_fields(page2_html)

  # ---- Step 2b: POST — select attendance date ----
  notify(0.35, paste0("Selecting date ", date_str, "..."))
  post2_resp <- tryCatch(
    POST(nmms_url, handle = h, UA, timeout(60),
         body = c(fields2, list(
           `__EVENTTARGET`    = "ctl00$ContentPlaceHolder1$ddl_attendance",
           `__EVENTARGUMENT`  = "",
           `ctl00$ContentPlaceHolder1$ddlstate`        = "31",
           `ctl00$ContentPlaceHolder1$ddl_attendance`   = date_str
         )),
         encode = "form"),
    error = function(e) NULL
  )
  if (is.null(post2_resp) || status_code(post2_resp) != 200) {
    return(list(success = FALSE, error = "Failed to select date."))
  }
  page3_html <- read_html(content(post2_resp, "text", encoding = "UTF-8"))
  fields3 <- extract_asp_fields(page3_html)

  # ---- Step 2c: POST — click "Show Attendance" ----
  notify(0.45, "Submitting form...")
  post3_resp <- tryCatch(
    POST(nmms_url, handle = h, UA, timeout(60),
         body = c(fields3, list(
           `__EVENTTARGET`    = "",
           `__EVENTARGUMENT`  = "",
           `ctl00$ContentPlaceHolder1$ddlstate`        = "31",
           `ctl00$ContentPlaceHolder1$ddl_attendance`   = date_str,
           `ctl00$ContentPlaceHolder1$btn_showreport`   = "Show Attendance"
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
    return(list(success = FALSE, error = "UTTAR PRADESH link not found in results. The date may not have data."))
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

  notify(1.0, paste0("Done! ", nrow(df), " rows scraped."))
  list(success = TRUE, data = df)
}
