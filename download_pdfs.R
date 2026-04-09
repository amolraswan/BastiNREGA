# download_pdfs.R - Save muster roll pages as PDFs using headless Chrome
#
# Dependencies: R, Chrome/Chromium, R packages (chromote, httr, rvest, xml2, dplyr, curl)
# Chrome is auto-detected. Set CHROMOTE_CHROME env var if it's in a non-standard location.

# ---- SET THE DATE HERE ----
dd   <- "10"
mm   <- "04"
yyyy <- "2026"
# ---------------------------

# Install chromote if missing
if (!requireNamespace("chromote", quietly = TRUE)) {
  cat("Installing chromote package...\n")
  install.packages("chromote", repos = "https://cloud.r-project.org")
}

library(chromote)
source("R/scraper.R")

date_tag <- paste0(sprintf("%02d", as.integer(dd)),
                   sprintf("%02d", as.integer(mm)), yyyy)

cat("=== Muster Roll PDF Downloader ===\n")
cat("Date:", dd, "/", mm, "/", yyyy, "\n\n")

# ---- Step 1: Scrape to get muster roll URLs ----
cat("[1/3] Scraping NREGA data to get muster roll URLs...\n")
result <- scrape_basti_data(dd, mm, yyyy, scrape_musters = FALSE,
  progress_callback = function(val, msg) cat("  ", msg, "\n"))

if (!result$success) stop("Scrape failed: ", result$error)
df <- result$data
cat("  Found", nrow(df), "muster rolls\n\n")

# ---- Step 2: Set up output directory ----
output_dir <- file.path("MusterRollsPDF", date_tag)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Build filenames using same convention as scrape_muster_details()
sanitize <- function(x) gsub("[^A-Za-z0-9_-]", "_", as.character(x))
df$pdf_name <- paste0(
  sanitize(df$Block), "_",
  sanitize(df$Panchayat), "_",
  sanitize(df$Work_Code), "_",
  sanitize(df$Mustroll_No), ".pdf"
)

# ---- Step 3: Save each page as PDF via headless Chrome ----
cat("[2/3] Launching headless Chrome...\n")
b <- ChromoteSession$new()

cat("[3/3] Saving", nrow(df), "pages as PDF...\n")
# Force screen media so print CSS rules don't hide content
b$Emulation$setEmulatedMedia(media = "screen")

success <- 0; failed <- 0

for (i in seq_len(nrow(df))) {
  url <- df$Mustroll_Link[i]
  pdf_path <- file.path(output_dir, df$pdf_name[i])

  if (is.na(url) || url == "") {
    cat(sprintf("  [%d/%d] SKIP (no URL): %s\n", i, nrow(df), df$pdf_name[i]))
    failed <- failed + 1
    next
  }

  tryCatch({
    b$Page$navigate(url)
    # Wait for page + images to render
    Sys.sleep(5)

    pdf_data <- b$Page$printToPDF(
      landscape = FALSE,
      printBackground = TRUE,
      preferCSSPageSize = FALSE,
      paperWidth = 8.27,   # A4
      paperHeight = 11.69  # A4
    )

    raw_pdf <- jsonlite::base64_dec(pdf_data$data)
    writeBin(raw_pdf, pdf_path)
    success <- success + 1
    cat(sprintf("  [%d/%d] OK: %s\n", i, nrow(df), df$pdf_name[i]))
  }, error = function(e) {
    failed <<- failed + 1
    cat(sprintf("  [%d/%d] FAILED: %s - %s\n", i, nrow(df), df$pdf_name[i], e$message))
  })
}

b$close()

cat("\n=== Done ===\n")
cat("Saved:", success, "PDFs\n")
cat("Failed:", failed, "\n")
cat("Output:", output_dir, "\n")
