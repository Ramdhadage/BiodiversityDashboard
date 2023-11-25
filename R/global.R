# adding disk cache location
shiny::shinyOptions(cache = cachem::cache_disk("./app_cache/cache/"))
# shiny suppress unwanted warning

options(shiny.autoload.r=FALSE)
