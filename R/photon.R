if (!exists("photon")) {
  photon <- new.env()
}

install_photon <- function(dir, countries = NULL, dmethod = "wget") {
  if (is.null(countries)) {
    countries <- c(
      "cz", "de", "dk", "at", "be", "gr", "es", "fr",
      "fi", "hu", "ir", "it", "ne", "pl", "ro", "pt"
    )
  }

  dir <- file.path(dir, "photon")
  
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  
  cli::cli_progress_step("Downloading executable")
  download.file(
    "https://github.com/komoot/photon/releases/download/0.4.2/photon-0.4.2.jar",
    destfile = file.path(file.path(dir, "photon.jar")),
    method = dmethod,
    quiet = TRUE
  )
  
  for (country in countries) {
    # load page and read some information before downloading
    req <- httr2::request(sprintf("https://download1.graphhopper.com/public/extracts/by-country-code/%s", country))
    req <- httr2::req_method(req, "GET")
    res <- httr2::req_perform(req)
    res <- httr2::resp_body_string(res)
    file <- str_extract_all(res, sprintf("photon-db-%s-[A-Za-z0-9]+\\.tar\\.bz2", country))[[1]]
    file <- file[length(file) - 2]
    date <- str_extract_all(res, "[0-9]{2}\\-[A-Z]{1}[a-z]{2}\\-[0-9]{4}")[[1]]
    date <- date[length(date) - 1]
    size <- str_extract_all(res, "[0-9]+M|G")[[1]]
    size <- size[length(size) - 1]
    if (str_detect(size, "G")) {
      size <- str_remove(size, "G")
      size <- as.numeric(size) * 1024
    } else {
      size <- str_remove(size, "M")
      size <- as.numeric(size)
    }
    browser()
    fpath <- file.path(dir, paste0(country, ".tar.bz2"))
    cli::cli_progress_step("Downloading country database: {toupper(country)}, {paste(size, 'MB')}, {date}")
    url <- sprintf(
      "https://download1.graphhopper.com/public/extracts/by-country-code/%s/%s",
      country, file
    )
    download.file(
      url,
      destfile = fpath,
      method = dmethod,
      quiet = TRUE
    )
    
    cli::cli_progress_step("Extracting files")
    untar(fpath, exdir = file.path(dir, country), verbose = FALSE)
    cli::cli_progress_done()
  }
}


start_photon <- function(path = "~/photon", country = NULL, min_ram = 6, max_ram = 12) {
  exec <- grep("photon\\-.+\\.jar", dir(path), value = TRUE)
  
  if (!length(exec)) {
    cli::cli_abort("Photon executable not found in the given {.var path}.")
  }
  
  path <- normalizePath(path, winslash = "/")
  
  cmd <- c(
    "-d64", sprintf("-Xms%sg", min_ram), sprintf("-Xmx%sg", max_ram),
    "-jar", exec, if (!is.null(country)) paste("-data-dir", country)
  )
  
  proc <- processx::process$new(
    command = "java",
    args = cmd,
    stdout = "|",
    stderr = "|",
    wd = path
  )
  
  cli::cli_progress_step(
    msg = "Starting photon...",
    msg_done = "Photon is now running.",
    msg_failed = "Photon could not be started."
  )
  
  out <- ""
  while (!grepl("ES cluster is now ready", out, fixed = TRUE)) {
    out <- proc$read_output()
  }
  
  assign("proc", proc, envir = photon)
  invisible(proc)
}


stop_photon <- function(proc = NULL) {
  if (is.null(proc)) {
    proc <- get("proc", envir = photon)
  }
  
  if (proc$is_alive()) {
    proc <- proc$interrupt()
  }
  
  proc
}


photon_running <- function(proc = NULL) {
  if (is.null(proc)) {
    proc <- get0("proc", envir = photon)
  }
  
  if (is.environment(proc)) {
    proc$is_alive()
  } else {
    FALSE
  }
}