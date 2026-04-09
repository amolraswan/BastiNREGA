# test_photos.R - Verify photo downloading in make_self_contained()
# Usage: Rscript test_photos.R              # scrapes date 3 days ago
#        Rscript test_photos.R DD MM YYYY   # scrapes specific date

library(httr)
library(rvest)
library(xml2)
library(curl)
library(dplyr)

source("R/scraper.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 3) {
  dd <- args[1]; mm <- args[2]; yyyy <- args[3]
} else {
  d <- Sys.Date() - 3
  dd <- format(d, "%d"); mm <- format(d, "%m"); yyyy <- format(d, "%Y")
}
date_tag <- paste0(dd, mm, yyyy)
cat("=== Photo Download Verification ===\n")
cat("Date:", dd, "/", mm, "/", yyyy, "\n\n")

# ---- Step 1: Scrape data ----
cat("[1/4] Scraping NREGA data...\n")
result <- scrape_basti_data(dd, mm, yyyy, scrape_musters = FALSE)
if (!result$success) stop("Scrape failed: ", result$error)
cat("  Got", nrow(result$data), "muster rolls\n")

# ---- Step 2: Fetch HTML pages ----
output_dir <- file.path("test_output", paste0("html_", date_tag))
unlink(output_dir, recursive = TRUE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

cat("[2/4] Fetching muster roll HTML pages...\n")
df <- scrape_muster_details(result$data, html_dir = output_dir,
  progress_callback = function(b, total) {
    cat(sprintf("  batch %d/%d\n", b, total))
  })

html_files <- list.files(output_dir, pattern = "\\.html$")
cat("  Saved", length(html_files), "HTML files\n")

# ---- Step 3: Make self-contained ----
cat("[3/4] Making HTML files self-contained (downloading photos)...\n")
make_self_contained(output_dir, progress_callback = function(val, msg) {
  cat(sprintf("  %s\n", msg))
})

# ---- Step 4: Validate ----
cat("[4/4] Validating...\n\n")

report <- character()
rpt <- function(...) {
  line <- paste0(...)
  report <<- c(report, line)
  cat(line, "\n")
}

photos_ok <- 0; photos_bad <- 0; photos_missing <- 0
pages_0 <- 0; pages_1 <- 0; pages_2 <- 0; pages_more <- 0
broken_html <- 0

for (f in html_files) {
  base_name <- sub("\\.html$", "", f)
  files_dir <- file.path(output_dir, paste0(base_name, "_files"))
  photo_files <- list.files(files_dir, pattern = "^photo_")

  # Count photos per page
  n_photos <- length(photo_files)
  if (n_photos == 0) pages_0 <- pages_0 + 1
  else if (n_photos == 1) pages_1 <- pages_1 + 1
  else if (n_photos == 2) pages_2 <- pages_2 + 1
  else pages_more <- pages_more + 1

  # Validate each photo
  for (pf in photo_files) {
    pf_path <- file.path(files_dir, pf)
    fsize <- file.info(pf_path)$size

    if (fsize < 1000) {
      rpt("  BAD  ", f, " -> ", pf, " (", fsize, " bytes, too small)")
      photos_bad <- photos_bad + 1
      next
    }

    # Check JPEG magic bytes
    raw <- readBin(pf_path, "raw", n = 2)
    if (length(raw) >= 2 && raw[1] == as.raw(0xFF) && raw[2] == as.raw(0xD8)) {
      photos_ok <- photos_ok + 1
    } else {
      rpt("  BAD  ", f, " -> ", pf, " (not a valid JPEG, magic bytes: ",
          paste(raw, collapse = " "), ")")
      photos_bad <- photos_bad + 1
    }
  }

  # Check rewritten HTML for leftover nregamms URLs
  html_text <- paste(readLines(file.path(output_dir, f), warn = FALSE), collapse = "\n")
  leftover <- regmatches(html_text,
    gregexpr('https?://nregamms[^"\'\\s>]+\\.jpe?g', html_text, perl = TRUE))[[1]]
  if (length(leftover) > 0) {
    rpt("  BROKEN HTML  ", f, " has ", length(leftover), " unreplaced photo URLs")
    broken_html <- broken_html + 1
  }
}

rpt("")
rpt("=== SUMMARY ===")
rpt("HTML files:       ", length(html_files))
rpt("Pages with 0 photos: ", pages_0)
rpt("Pages with 1 photo:  ", pages_1)
rpt("Pages with 2 photos: ", pages_2)
if (pages_more > 0) rpt("Pages with 3+ photos: ", pages_more)
rpt("Photos OK (valid JPEG): ", photos_ok)
rpt("Photos BAD:             ", photos_bad)
rpt("Broken HTML rewrites:   ", broken_html)
rpt("")
if (photos_bad == 0 && broken_html == 0) {
  rpt("RESULT: ALL PHOTOS SAVED CORRECTLY")
} else {
  rpt("RESULT: ISSUES FOUND - see details above")
}

# Save report
report_path <- file.path("test_output", "photo_test_report.txt")
writeLines(report, report_path)
cat("\nReport saved to:", report_path, "\n")
cat("HTML files saved to:", output_dir, "\n")
cat("Open any .html file in a browser to visually verify photos.\n")
