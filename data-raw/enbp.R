library(rdflib)

read_enbp <- function(doc = NULL, country = NULL, geo = TRUE, as_rdf = FALSE, inv = NULL) {
  if (is.null(inv)) {
    inv <- paste(readLines(doc), collapse = "\n")
    inv <- rdflib::rdf_parse(inv, format = "turtle")
    
    if (as_rdf) return(inv)
  }
  
  geo_query <- geo_var <- NULL
  
  if (!is.null(country)) {
    country <- sprintf("\n?cai a enbp:CAI.\n
    ?cai wikidata:P298 \"%s\".\n", country)
  } else {
    country <- "\n?cai a enbp:CAI.\n?cai wikidata:P298 ?country.\n"
  }
  
  if (geo) {
    geo_var <- " ?long ?lat "
    geo_query <- "\n?cai geo:long ?long.\n?cai geo:lat ?lat.\n"
  }
  
  sparql <- paste0(
    "PREFIX dc: <http://purl.org/dc/elements/1.1/>
    PREFIX schema: <http://schema.org/>
    PREFIX enbp: <http://www.your-domain/>
    PREFIX geo: <http://www.w3.org/2003/01/geo/wgs84_pos#>
    PREFIX wikidata: <https://www.wikidata.org/entity/>",
    
    "SELECT ?cainame ?country ?city ?foundingDate", geo_var,
    "WHERE {
      ?cai schema:name ?cainame.",
    geo_query,
    country,
    "OPTIONAL {
    ?cai schema:addressLocality ?city.
    ?cai schema:foundingDate ?foundingDate.
    }}"
  )

  inv <- rdflib::rdf_query(inv, sparql)
  
  if (geo) {
    inv <- sf::st_as_sf(inv, coords = c("long", "lat"), crs = 4326)
  }
  
  inv
}
