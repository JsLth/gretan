init <- function(name, session = getDefaultReactiveDomain()) {
  session$userData[[name]] <- reactiveVal(FALSE)
}

trigger <- function(name, session = getDefaultReactiveDomain()) {
  session$userData[[name]](TRUE)
}

watch <- function(name, session = getDefaultReactiveDomain()) {
  session$userData[[name]]()
}