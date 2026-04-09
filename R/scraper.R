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

scrape_muster_details <- function(df, progress_callback = NULL, html_dir = NULL) {
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
            raw_content <- resp$content
            html <- read_html(rawToChar(raw_content))

            # Save raw HTML copy to disk (will be made self-contained later)
            if (!is.null(html_dir)) {
              row <- df[ii, ]
              sanitize <- function(x) gsub("[^A-Za-z0-9_-]", "_", as.character(x))
              fname <- paste0(
                sanitize(row$Block), "_",
                sanitize(row$Panchayat), "_",
                sanitize(row$Work_Code), "_",
                sanitize(row$Mustroll_No), ".html"
              )
              tryCatch(
                writeBin(raw_content, file.path(html_dir, fname)),
                error = function(e) NULL
              )
            }

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

make_self_contained <- function(html_dir, progress_callback = NULL) {
  notify <- function(val, msg) {
    if (!is.null(progress_callback)) progress_callback(val, msg)
  }

  html_files <- list.files(html_dir, pattern = "\\.html$", full.names = FALSE)
  if (length(html_files) == 0) return(invisible(NULL))

  # ---- Step 1: Download common assets once ----
  notify(0, "Downloading common assets (CSS, JS)...")
  common_assets <- list(
    list(url = "https://mnregaweb4.nic.in/nregaarch/css/bootstrap.min.css",  name = "bootstrap.min.css"),
    list(url = "https://mnregaweb4.nic.in/nregaarch/Scripts/jquery.min.js",  name = "jquery.min.js"),
    list(url = "https://mnregaweb4.nic.in/nregaarch/Scripts/excelexportjs.js", name = "excelexportjs.js"),
    list(url = "https://mnregaweb4.nic.in/nregaarch/Scripts/jQuery.print.js",  name = "jQuery.print.js"),
    list(url = "https://mnregaweb4.nic.in/nregaarch/images/title_logo.jpg",    name = "title_logo.jpg"),
    list(url = "https://www.google.com/jsapi",                                 name = "jsapi.js"),
    list(url = "https://www.gstatic.com/charts/loader.js",                     name = "loader.js"),
    list(url = "https://translation-plugin.bhashini.co.in/v2/website_translation_utility.js", name = "website_translation_utility.js")
  )

  cache_dir <- file.path(html_dir, "_common_cache")
  dir.create(cache_dir, showWarnings = FALSE)

  pool <- new_pool(total_con = 10, host_con = 6)
  for (asset in common_assets) {
    local({
      a <- asset
      h <- new_handle()
      handle_setheaders(h, "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
      handle_setopt(h, timeout = 30)
      curl_fetch_multi(a$url, done = function(resp) {
        tryCatch(writeBin(resp$content, file.path(cache_dir, a$name)), error = function(e) NULL)
      }, fail = function(msg) NULL, pool = pool, handle = h)
    })
  }
  multi_run(pool = pool)

  # ---- Step 2: Parse photo URLs from each HTML file ----
  notify(0.15, "Scanning HTML files for photo URLs...")
  # photo_map: list keyed by html filename, each entry is a list of unique image URLs
  photo_map <- list()
  for (fname in html_files) {
    fpath <- file.path(html_dir, fname)
    html_text <- tryCatch(readLines(fpath, warn = FALSE, encoding = "UTF-8"), error = function(e) "")
    html_text <- paste(html_text, collapse = "\n")
    # Find all image URLs from nregamms servers (group photos)
    urls <- regmatches(html_text, gregexpr('https?://nregamms[^"\'\\s>]+\\.jpe?g', html_text, perl = TRUE))[[1]]
    photo_map[[fname]] <- unique(urls)
  }

  # ---- Step 3: Batch-download all photos ----
  # Build a flat list of all (url, dest_path) pairs
  all_downloads <- list()
  for (fname in html_files) {
    base_name <- sub("\\.html$", "", fname)
    files_dir <- file.path(html_dir, paste0(base_name, "_files"))
    dir.create(files_dir, showWarnings = FALSE)

    # Copy common assets into this page's _files/ folder
    cached <- list.files(cache_dir, full.names = TRUE)
    for (cf in cached) {
      file.copy(cf, file.path(files_dir, basename(cf)), overwrite = TRUE)
    }

    urls <- photo_map[[fname]]
    if (length(urls) > 0) {
      for (j in seq_along(urls)) {
        ext <- if (grepl("\\.jpeg", urls[j], ignore.case = TRUE)) ".jpeg" else ".jpg"
        local_name <- paste0("photo_", j, ext)
        all_downloads[[length(all_downloads) + 1]] <- list(
          url = urls[j],
          dest = file.path(files_dir, local_name),
          html_file = fname,
          original_url = urls[j],
          local_name = local_name
        )
      }
    }
  }

  if (length(all_downloads) > 0) {
    batch_size <- 20
    total_batches <- ceiling(length(all_downloads) / batch_size)
    for (b in seq_len(total_batches)) {
      idx_start <- (b - 1) * batch_size + 1
      idx_end <- min(b * batch_size, length(all_downloads))
      frac <- 0.3 + 0.5 * (b / total_batches)
      notify(frac, paste0("Downloading photos... batch ", b, "/", total_batches))
      pool <- new_pool(total_con = 20, host_con = 20)
      for (k in idx_start:idx_end) {
        local({
          dl <- all_downloads[[k]]
          h <- new_handle()
          handle_setheaders(h, "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
          handle_setopt(h, timeout = 60, followlocation = TRUE)
          curl_fetch_multi(dl$url, done = function(resp) {
            tryCatch(writeBin(resp$content, dl$dest), error = function(e) NULL)
          }, fail = function(msg) NULL, pool = pool, handle = h)
        })
      }
      multi_run(pool = pool)
    }
  }

  # ---- Step 4: Rewrite HTML files to use local paths ----
  notify(0.85, "Rewriting HTML files to use local paths...")
  for (fname in html_files) {
    fpath <- file.path(html_dir, fname)
    html_text <- tryCatch(readLines(fpath, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
    if (is.null(html_text)) next
    html_text <- paste(html_text, collapse = "\n")

    base_name <- sub("\\.html$", "", fname)
    files_prefix <- paste0(base_name, "_files/")

    # Replace common asset references
    html_text <- gsub('href="css/bootstrap.min.css"',
      paste0('href="', files_prefix, 'bootstrap.min.css"'), html_text, fixed = TRUE)
    html_text <- gsub('src="Scripts/jquery.min.js"',
      paste0('src="', files_prefix, 'jquery.min.js"'), html_text, fixed = TRUE)
    html_text <- gsub('src="Scripts/excelexportjs.js"',
      paste0('src="', files_prefix, 'excelexportjs.js"'), html_text, fixed = TRUE)
    html_text <- gsub('src="Scripts/jQuery.print.js"',
      paste0('src="', files_prefix, 'jQuery.print.js"'), html_text, fixed = TRUE)
    html_text <- gsub('href="images/title_logo.jpg"',
      paste0('href="', files_prefix, 'title_logo.jpg"'), html_text, fixed = TRUE)
    html_text <- gsub('src="https://www.google.com/jsapi"',
      paste0('src="', files_prefix, 'jsapi.js"'), html_text, fixed = TRUE)
    html_text <- gsub('src="https://www.gstatic.com/charts/loader.js"',
      paste0('src="', files_prefix, 'loader.js"'), html_text, fixed = TRUE)
    html_text <- gsub(
      'src="https://translation-plugin.bhashini.co.in/v2/website_translation_utility.js"',
      paste0('src="', files_prefix, 'website_translation_utility.js"'), html_text, fixed = TRUE)

    # Replace photo URLs with local paths
    urls <- photo_map[[fname]]
    if (length(urls) > 0) {
      for (j in seq_along(urls)) {
        ext <- if (grepl("\\.jpeg", urls[j], ignore.case = TRUE)) ".jpeg" else ".jpg"
        local_name <- paste0("photo_", j, ext)
        # Replace both src= and href= references to this photo URL
        html_text <- gsub(urls[j], paste0(files_prefix, local_name), html_text, fixed = TRUE)
      }
    }

    writeLines(html_text, fpath, useBytes = TRUE)
  }

  # Clean up the shared cache
  unlink(cache_dir, recursive = TRUE)

  notify(1, "HTML files are now self-contained.")
  invisible(NULL)
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

  html_dir <- NULL
  if (scrape_musters) {
    notify(0.85, "Fetching muster roll details (work names & photo status)...")
    date_tag <- paste0(sprintf("%02d", as.integer(dd)), sprintf("%02d", as.integer(mm)), yyyy)
    html_dir <- file.path(tempdir(), paste0("html_", date_tag))
    unlink(html_dir, recursive = TRUE)
    dir.create(html_dir, showWarnings = FALSE, recursive = TRUE)
    df <- scrape_muster_details(df, progress_callback = function(batch_done, total_batches) {
      frac <- 0.85 + 0.08 * (batch_done / total_batches)
      notify(frac, paste0("Fetching muster details... batch ", batch_done, "/", total_batches))
    }, html_dir = html_dir)

    # Make HTML files self-contained (download CSS, JS, photos)
    notify(0.93, "Making HTML files self-contained...")
    make_self_contained(html_dir, progress_callback = function(val, msg) {
      frac <- 0.93 + 0.06 * val
      notify(frac, msg)
    })
  } else {
    df$Work_Name <- NA_character_
    df$Has_Second_Photo <- NA
  }

  notify(1.0, paste0("Done! ", nrow(df), " rows scraped."))
  list(success = TRUE, data = df, html_dir = html_dir)
}
