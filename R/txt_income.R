txts$income$title <- "Income stability"
txts$income$icon <- icon("money-bill-trend-up")
txts$income$tags <- c(
  "income", "arbitrary", "analysis", "example", "distribution"
)
txts$income$introduction <- shinipsum::random_text(nwords = 200)
txts$income$methodology <- shinipsum::random_text(nwords = 200)
txts$income$discussion <- shinipsum::random_text(nwords = 200)
txts$income$conclusion <- shinipsum::random_text(nwords = 150)
txts$income$references <- dummy_bibliography()