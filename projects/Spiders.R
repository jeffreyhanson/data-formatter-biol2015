Spiders=DATA_PREP$new(
	column_classes=list(
		"Date"="character",
		"Time"="character",
		"Group Name"="character",
		"Latitude"="numeric",
		"Longitude"="numeric",
		"Altitude"="numeric",
		"Forest Type"="character",
		"Hole Diameter"="numeric"
	),
	format=function(inpDF) {
		assign('inpDF', inpDF, envir=globalenv())
		inpDF %T>% setnames(
			c("Date","Time","Group","Latitude","Longitude","Altitude","Forest Type","Hole Diameter")
		) %>% filter(
			!is.na(`Forest Type`)
		) %>% filter(
			nchar(`Forest Type`)>0
		) %>% return()
	},
	errors=list(
		ERROR_TEMPLATE.FACTOR('Forest Type', c('Medium Open','Low Open','Tall Open')),
		ERROR_TEMPLATE.TRUNCATED('Hole Diameter'),		
		ERROR_TEMPLATE.OUTLIER('Hole Diameter')
	),
	process=function(inpDF, ...) {
		if (exists('omitRows'))
			inpDF=inpDF[!omitRows,]
		return(inpDF)
	}
)

