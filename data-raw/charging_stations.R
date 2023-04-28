library(httr2)
library(dplyr)

countries <- c(AT = 19, BE = 26, CZ = 64, DK = 65, FI = 79, FR = 80, DE = 87, 
               EL = 112, HU = 104, IE = 3, IT = 112, NL = 159, PL = 179,
               PT = 180, RO = 184, ES = 210)

query_ocm <- function(country, maxresults = 100, ..., path = NULL) {
  countries <- NULL
  if (length(country) > 1) {
    countries <- paste(country, collapse = ", ")
    country <- NULL
  }
  
  client <- "jslth"
  req <- request("https://api.openchargemap.io") %>%
    req_url_path("v3/poi") %>%
    req_url_query(
      key = Sys.getenv("OCM_KEY"),
      countryid = countries,
      maxresults = maxresults,
      countrycode = country,
      client = client,
      ...
    ) %>%
    req_method("GET") %>%
    req_headers(Accept = "application/json")
  
  
  if (is.null(path)) {
    res <- req_perform(req) %>%
      resp_body_json(simplifyVector = TRUE, flatten = TRUE) %>%
      clean_ocm()
  } else {
    req_perform(req, path = path)
  }
}


clean_ocm <- function(res) {
  if (is.character(res)) {
    res <- jsonlite::read_json(res, simplifyVector = TRUE, flatten = TRUE)
  }
  res %>%
    select(
      id = ID,
      uuid = UUID,
      recently_verified = IsRecentlyVerified,
      last_verified = DateLastVerified,
      last_update = DateLastStatusUpdate,
      provider = DataProviderID,
      operator = OperatorID,
      usage = UsageTypeID,
      cost = UsageCost,
      equipment = Connections,
      number = NumberOfPoints,
      usage_paid = UsageType.IsPayAtLocation,
      usage_membership = UsageType.IsMembershipRequired,
      usage_key = UsageType.IsAccessKeyRequired,
      country = AddressInfo.Country.ISOCode,
      country_id = AddressInfo.CountryID,
      x = AddressInfo.Longitude,
      y = AddressInfo.Latitude
    ) %>%
    tibble::as_tibble() %>%
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)
}


country_ids <- function(countries) {
  vapply(
    countries,
    function(x) query_ocm(x, maxresults = 1)$country_id,
    FUN.VALUE = numeric(1)
  )
}