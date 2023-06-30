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
    content = "Content here"
  ),
  virtual = list(
    title = "Virtual level",
    content = "Content here"
  ),
  local = list(
    title = "Local level",
    content = "Content here"
  ),
  regional = list(
    title = "Regional level",
    content = "Content here"
  ),
  national = list(
    title = "National level",
    content = "Content here"
  ),
  supranational = list(
    title = "Supranational level",
    content = "Content here"
  ),
  prox = list(
    title = "Proximity domains",
    content = "Content here"
  ),
  spatial = list(
    title = "Spatial domain",
    content = "Content here"
  ),
  policy = list(
    title = "Policy domain",
    content = "Content here"
  ),
  social = list(
    title = "Social domain",
    content = "Content here"
  ),
  tech = list(
    title = "Technological domain",
    content = "Content here"
  ),
  econ = list(
    title = "Economic domain",
    content = "Content here"
  ),
  dims = list(
    title = "Dimensions & indicators",
    content = "Content here"
  )
)