## Run locally
test=function(){shiny::runApp(file.path(Sys.getenv('HOME'), 'github', 'data-formatter-biol2015'))}

# test=function(){shiny::runApp('Q:/Fieldtrips/BIOL2015/2015/programs/data-formatter-biol2015')}

## Deploy online to ShinyApps
# shinyapps::deployApp("C:/Users/jeff/Documents/GitHub/data-formatter-biol2015")
# shinyapps::deployApp("/home/jeff/Github/data-formatter-biol2015")

