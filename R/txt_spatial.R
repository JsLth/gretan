txts$spatial$title <- "Spatial analysis"
txts$spatial$icon <- icon("layer-group")
txts$spatial$tags <- c(
  "spatial", "analysis", "moran", "investment", "coopernico", "portugal"
)
txts$spatial$introduction <- shinipsum::random_text(nwords = 200)
txts$spatial$methodology <- shinipsum::random_text(nwords = 200)
txts$spatial$discussion <- shinipsum::random_text(nwords = 550)
txts$spatial$references <- dummy_bibliography()