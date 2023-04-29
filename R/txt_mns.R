txts$exp$title <- "Explore data"
txts$exp$icon <- icon("map")
txts$exp$tags <- c(
  "explore", "data", "multinational", "survey", "map"
)
txts$exp$help <- list(
  databox = "This box helps you to select a variable from the GRETA
    multinational survey. Choose one of the topics corresponding to a survey
    question to update the map on the right. If applicable, you can also specify
    subitems and options."
)

txts$cmp$title <- "Compare data"
txts$cmp$icon <- icon("map")
txts$cmp$tags <- c(
  "compare", "data", "multinational", "survey", "map"
)
txts$cmp$help <- list(
  databoxLeft = "This box helps you to select a variable from the GRETA
    multinational survey. Choose one of the topics corresponding to a survey
    question to update the left map. If applicable, you can also specify
    subitems and options.",
  databoxRight = "This box helps you to select a variable from the GRETA
    multinational survey. Choose one of the topics corresponding to a survey
    question to update the right map. If applicable, you can also specify
    subitems and options."
)

txts$cmp$title <- "Inspect data"
txts$cmp$icon <- icon("map")
txts$cmp$tags <- c(
  "inspect", "data", "multinational", "survey", "table", "transpose",
  "download"
)