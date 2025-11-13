#!/usr/local/bin/Rscript

dht::greeting()

withr::with_message_sink("/dev/null", library(dplyr))
withr::with_message_sink("/dev/null", library(digest))
withr::with_message_sink("/dev/null", library(knitr))

doc <- "
      Usage:
      entrypoint.R <filename> [<score_threshold>]
      "
opt <- docopt::docopt(doc)
if (is.null(opt$score_threshold)) opt$score_threshold <- 0.5

cat("[INFO] ========================================\n", file = stdout())
cat(sprintf("[INFO] geocoder entrypoint.R starting\n"), file = stdout())
cat(sprintf("[INFO] Input file: %s\n", opt$filename), file = stdout())
cat(sprintf("[INFO] Score threshold: %s\n", opt$score_threshold), file = stdout())
cat("[INFO] ========================================\n", file = stdout())

d <- readr::read_csv(opt$filename, show_col_types = FALSE)
# d <- readr::read_csv('test/my_address_file.csv')
# d <- readr::read_csv('test/my_address_file_missing.csv')

## must contain character column called address
if (!"address" %in% names(d)) stop("no column called address found in the input file", call. = FALSE)

## clean up addresses / classify 'bad' addresses
cat(sprintf("[INFO] Processing %d total addresses\n", nrow(d)), file = stdout())
d$address <- dht::clean_address(d$address)
d$po_box <- dht::address_is_po_box(d$address)
d$cincy_inst_foster_addr <- dht::address_is_institutional(d$address)
d$non_address_text <- dht::address_is_nonaddress(d$address)

## exclude 'bad' addresses from geocoding (unless specified to return all geocodes)
if (opt$score_threshold == "all") {
  d_for_geocoding <- d
  cat(sprintf("[INFO] Score threshold set to 'all' - will geocode all %d addresses\n", nrow(d_for_geocoding)), file = stdout())
} else {
  d_excluded_for_address <- dplyr::filter(d, cincy_inst_foster_addr | po_box | non_address_text)
  d_for_geocoding <- dplyr::filter(d, !cincy_inst_foster_addr & !po_box & !non_address_text)
  cat(sprintf("[INFO] Excluded %d addresses (PO boxes, institutional, or non-address text)\n", nrow(d_excluded_for_address)), file = stdout())
  cat(sprintf("[INFO] Will geocode %d addresses with score threshold %.2f\n", nrow(d_for_geocoding), as.numeric(opt$score_threshold)), file = stdout())
}

out_template <- tibble(
  street = NA, zip = NA, city = NA, state = NA,
  lat = NA, lon = NA, score = NA, precision = NA,
  fips_county = NA, number = NA, prenum = NA
)

## geocode
cli::cli_alert_info("now geocoding ...", wrap = TRUE)
geocode <- function(addr_string) {
  stopifnot(class(addr_string) == "character")

  out <- tryCatch({
    system2("ruby",
      args = c("/app/geocode.rb", shQuote(addr_string)),
      stderr = FALSE, stdout = TRUE
    )
  }, error = function(e) {
    cat(sprintf("[ERROR] geocode() system2 call failed for address: %s\n", substr(addr_string, 1, 50)), file = stdout())
    cat(sprintf("[ERROR] Error message: %s\n", e$message), file = stdout())
    return(character(0))
  })

  if (length(out) > 0 && !is.null(out) && out != "") {
    tryCatch({
      out <- out %>%
        jsonlite::fromJSON()
      
      # Handle case where fromJSON returns empty or malformed data
      if (is.null(out) || length(out) == 0) {
        cat(sprintf("[WARN] Empty JSON result for address: %s\n", substr(addr_string, 1, 50)), file = stdout())
        return(out_template)
      }
      
      out <-
        bind_rows(out_template, out) %>%
        .[2, ]
    }, error = function(e) {
      cat(sprintf("[ERROR] JSON parsing/processing failed for address: %s\n", substr(addr_string, 1, 50)), file = stdout())
      cat(sprintf("[ERROR] Error message: %s\n", e$message), file = stdout())
      cat(sprintf("[DEBUG] Raw output: %s\n", paste(out, collapse = " | ")), file = stdout())
      return(out_template)
    })
  } else {
    cat(sprintf("[WARN] No output from geocoder for address: %s\n", substr(addr_string, 1, 50)), file = stdout())
    out <- out_template
  }

  out
}

# if any geocodes are returned, regardless of score_threshold...
if (nrow(d_for_geocoding) > 0) {
  cat(sprintf("[INFO] Starting geocoding of %d addresses...\n", nrow(d_for_geocoding)), file = stdout())
  d_for_geocoding$geocodes <- mappp::mappp(d_for_geocoding$address,
                                           geocode,
                                           parallel = TRUE,
                                           cache = TRUE,
                                           cache_name = "geocoding_cache"
  )
  
  cat("[INFO] Geocoding complete, now processing results...\n", file = stdout())
  
  ## extract results, if a tie then take first returned result
  d_for_geocoding <- d_for_geocoding %>%
    dplyr::mutate(
      row_index = 1:nrow(d_for_geocoding),
      geocodes = purrr::map(geocodes, ~ .x %>%
                              purrr::map(unlist) %>%
                              as_tibble())
    ) %>%
    tidyr::unnest(cols = c(geocodes)) %>%
    dplyr::group_by(row_index) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      matched_street = street,
      matched_city = city,
      matched_state = state,
      matched_zip = zip
    ) %>%
    dplyr::select(-fips_county, -prenum, -number, -row_index) %>%
    dplyr::mutate(precision = factor(precision,
                                     levels = c("range", "street", "intersection", "zip", "city"),
                                     ordered = TRUE
    )) %>%
    dplyr::arrange(desc(precision), score)
  
  cat(sprintf("[INFO] Successfully processed %d geocoded addresses\n", nrow(d_for_geocoding)), file = stdout())
  } else if (nrow(d_for_geocoding) == 0 & opt$score_threshold != "all") { 
    # if no geocodes are returned and not returning all geocodes, 
    # then bind non-geocoded with out template
  cat("[INFO] No addresses to geocode after filtering\n", file = stdout())
  d_excluded_for_address <-
    bind_rows(d_excluded_for_address, out_template) %>%
    .[1:nrow(.) - 1, ]
  }

## clean up 'bad' address columns / filter to precise geocodes
cli::cli_alert_info("geocoding complete; now filtering to precise geocodes...", wrap = TRUE)
if (opt$score_threshold == "all") {
  cat("[INFO] Returning all geocodes without filtering\n", file = stdout())
  out_file <- d_for_geocoding
} else {
cat(sprintf("[INFO] Filtering geocodes with score threshold %.2f\n", as.numeric(opt$score_threshold)), file = stdout())
out_file <- dplyr::bind_rows(d_excluded_for_address, d_for_geocoding) %>%
  dplyr::mutate(
    geocode_result = dplyr::case_when(
      po_box ~ "po_box",
      cincy_inst_foster_addr ~ "cincy_inst_foster_addr",
      non_address_text ~ "non_address_text",
      (!precision %in% c("street", "range")) | (score < opt$score_threshold) ~ "imprecise_geocode",
      TRUE ~ "geocoded"
    ),
    lat = ifelse(geocode_result == "imprecise_geocode", NA, lat),
    lon = ifelse(geocode_result == "imprecise_geocode", NA, lon)
  ) %>%
  select(-po_box, -cincy_inst_foster_addr, -non_address_text) # note, just "PO" not "PO BOX" is not flagged as "po_box"
}

## write out file
cat(sprintf("[INFO] Writing output file with %d rows\n", nrow(out_file)), file = stdout())
dht::write_geomarker_file(
  out_file,
  filename = opt$filename,
  argument = glue::glue("score_threshold_{opt$score_threshold}")
)
cat("[INFO] Output file written successfully\n", file = stdout())

## summarize geocoding results and
## print geocoding results summary to console
if (opt$score_threshold != "all") {
  geocode_summary <- out_file %>%
    mutate(geocode_result = factor(geocode_result,
                                   levels = c(
                                     "po_box", "cincy_inst_foster_addr", "non_address_text",
                                     "imprecise_geocode", "geocoded"
                                   ),
                                   ordered = TRUE
    )) %>%
    group_by(geocode_result) %>%
    tally() %>%
    mutate(
      `%` = round(n / sum(n) * 100, 1),
      `n (%)` = glue::glue("{n} ({`%`})")
    )
  
  n_geocoded <- geocode_summary$n[geocode_summary$geocode_result == "geocoded"]
  n_total <- sum(geocode_summary$n)
  pct_geocoded <- geocode_summary$`%`[geocode_summary$geocode_result == "geocoded"]
  cli::cli_alert_info("{n_geocoded} of {n_total} ({pct_geocoded}%) addresses were successfully geocoded. See detailed summary below.",
                      wrap = TRUE
  )
  knitr::kable(geocode_summary %>% dplyr::select(geocode_result, `n (%)`))
}

