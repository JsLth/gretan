txts$taxonomy$title <- "Geo Taxonomy"
txts$taxonomy$icon <- icon("sitemap")
txts$taxonomy$tags <- c(
  "geo", "geographic", "levels", "taxonomy"
)
txts$taxonomy$introduction <- shinipsum::random_text(nwords = 200)
txts$taxonomy$methodology <- shinipsum::random_text(nwords = 200)
txts$taxonomy$help <- list(
  scheme = HTML("Explore the interactive scheme below to learn about GRETA's 
  geographical taxonomy. You can click on each element to understand the
  taxonomy's <b>geographical levels</b> and <b>proxmity domains</b> in more detail.
  <br>
  The taxonomy was first introduced in <a href=https://projectgreta.eu/wp-content/uploads/2022/02/GRETA_D5_1_Taxonomy-of-geographical-levels-and-drivers_v1_0.pdf#page=34>Deliverable D5.1</a> of the GRETA project."
))
txts$taxonomy$scheme <- list(
  header = list(
    title = "GRETA levels",
    content = shinipsum::random_text(nwords = 100)
  ),
  virtual = list(
    title = "Virtual level",
    content = shinipsum::random_text(nwords = 100)
  ),
  local = list(
    title = "Local level",
    content = shinipsum::random_text(nwords = 100)
  ),
  regional = list(
    title = "Regional level",
    content = shinipsum::random_text(nwords = 100)
  ),
  national = list(
    title = "National level",
    content = shinipsum::random_text(nwords = 100)
  ),
  supranational = list(
    title = "Supranational level",
    content = shinipsum::random_text(nwords = 100)
  ),
  prox = list(
    title = "Proximity domains",
    content = shinipsum::random_text(nwords = 100)
  ),
  spatial = list(
    title = "Spatial domain",
    content = shinipsum::random_text(nwords = 100)
  ),
  policy = list(
    title = "Policy domain",
    content = shinipsum::random_text(nwords = 100)
  ),
  social = list(
    title = "Social domain",
    content = shinipsum::random_text(nwords = 100)
  ),
  tech = list(
    title = "Technological domain",
    content = shinipsum::random_text(nwords = 100)
  ),
  econ = list(
    title = "Economic domain",
    content = shinipsum::random_text(nwords = 100)
  ),
  dims = list(
    title = "Dimensions & indicators",
    content = shinipsum::random_text(nwords = 100)
  )
)