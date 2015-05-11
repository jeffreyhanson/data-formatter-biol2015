Mangroves_Community=DATA_PREP$new(
	column_classes=list(
		"Date"="character",
		"Time"="character",
		"group"="character",
		"Transect"="character",
		"Zone"="character",
		"Points within Zone"="character",
		"distance on tape"="numeric",
		"Latitude"="numeric",
		"Longitude"="numeric",
		"quarter"="character",
		"species"="character",
		"Distance from point (m)"="numeric",
		"Height (m)"="numeric",
		"Circumference (cm)"="numeric",
		"point location note"="character"
	),
	format=function(inpDF) {
		inpDF %>% mutate(
				Longitude=na.locf(Longitude),
				Latitude=na.locf(Latitude)
		) %>% filter(
			grepl('quarter',quarter)
		) %T>% setnames(
			c("distance on tape","quarter","species",'group'),
			c("Distance on tape (m)","Quarter","Species","Group")
		) %>% return()
	},
	errors=list(
		ERROR_TEMPLATE.FACTOR("Transect", c("Seaward", "Landward-Creek")),
		ERROR_TEMPLATE.FACTOR("Zone", c("0-20m","20-40m","40-60m","60-80m","80-100m")),
		ERROR_TEMPLATE.FACTOR("Species", c("Aegialitis annulata","Aegiceras corniculatum","Avicennia marina","Ceriops australis","Lumnitzera racemosa","Rhizophora stylosa","Osbornia octodonta","Other?")),
		ERROR_TEMPLATE.SEQUENCE.REPEATS("Points within Zone", c("Point 1","Point 2","Point 3"), by='Zone'),
		ERROR_TEMPLATE.TRUNCATED("Distance on tape (m)"),
		ERROR_TEMPLATE.SEQUENCE.NO_REPEATS("Quarter", c("quarter 1", "quarter 2", "quarter 3", "quarter 4"), by='Points within Zone'),
		ERROR_TEMPLATE.TRUNCATED("Distance from point (m)"),
		ERROR_TEMPLATE.OUTLIER("Distance from point (m)"),
		ERROR_TEMPLATE.TRUNCATED("Height (m)"),
		ERROR_TEMPLATE.OUTLIER("Height (m)"),
		ERROR_TEMPLATE.TRUNCATED("Circumference (cm)"),
		ERROR_TEMPLATE.OUTLIER("Circumference (cm)")
	),
	process=function(inpDF, ...) {
		if (exists('omitRows'))
			inpDF=inpDF[!omitRows,]
		inpDF %>% calculate_PCQ_metrics(columns=c('Transect','Zone','Group')) %>% return()
	}
)



calculate_PCQ_metrics=function(inpDF, columns) {
	## init
	allSpeciesNames=c("Aegialitis annulata","Aegiceras corniculatum","Avicennia marina","Ceriops australis","Lumnitzera racemosa","Rhizophora stylosa","Osbornia octodonta","Other?")
	inpDF$key=aaply(inpDF, 1, .drop=TRUE, .expand=FALSE, function(x) {
		return(
			paste(
				x[,columns,with=FALSE],
				collapse='_'
			)
		)
	})	
	setkey(inpDF, key)
	
	## main processing
	# calculate top level values
	inpDF[,nQuarters:=length(Group),by=key]
	inpDF[,nPoints:=n_distinct(`Points within Zone`),by=key]
	inpDF[,meanDistanceBetweenTrees:=mean(`Distance from point (m)`, na.rm=TRUE),by=key]
	inpDF[,meanAverageDensityAllSpp:=10000/(meanDistanceBetweenTrees^2),by=key]
	
	# calculate species level data
	inpDF[,
		list(
			Longitude=mean(Longitude,na.rm=TRUE),
			Latitude=mean(Latitude,na.rm=TRUE),
			Group=first(Group),
			Transect=first(Transect),
			Zone=first(Zone),
			nQuarters=first(nQuarters),
			nPoints=first(nPoints),
			nTrees=length(Group),
			meanAverageDensityAllSpp=mean(meanAverageDensityAllSpp),
			meanDistanceBetweenTrees=mean(meanDistanceBetweenTrees),
			Height=mean(`Height (m)`),
			Frequency=n_distinct(`Points within Zone`),
			`Mean Circumference (cm)`=mean(`Circumference (cm)`, na.rm=TRUE)
		),
		by=list(key, Species)
	] %>% mutate(
		`Frequency per Zone`=Frequency/nPoints,
		`Trees per HA`=`Frequency per Zone`*meanAverageDensityAllSpp,
		Density=(nTrees/nQuarters) * meanDistanceBetweenTrees,
		`Basal Area per HA`=(`Mean Circumference (cm)` * `Trees per HA`) / 10000
	) -> sppDF
	sppDF[,`Relative Cover`:=`Basal Area per HA`/sum(`Basal Area per HA`),by=key]
	sppDF[,`Relative Frequency`:=`Frequency per Zone`/sum(`Frequency per Zone`),by=key]
	sppDF[,`Relative Density`:=nTrees/sum(nTrees),by=key]
	sppDF[,`Relative Importance`:=(`Relative Cover`+`Relative Frequency`+`Relative Density`)/3,by=key]
	sppDF %<>% select(
		key,
		Longitude,
		Latitude,
		Transect,
		Zone,
		Group,
		Species,
		Height,
		`Mean Circumference (cm)`,
		Frequency,
		`Frequency per Zone`,
		`Trees per HA`,
		Density,
		`Basal Area per HA`,
		`Relative Cover`,
		`Relative Frequency`,
		`Relative Density`,
		`Relative Importance`
	)
	
	# add in missing species with zeros
	sppDF[,
		list(
			Longitude=mean(Longitude,na.rm=TRUE),
			Latitude=mean(Latitude,na.rm=TRUE),
			Group=first(Group),
			Transect=first(Transect),
			Zone=first(Zone),
			Species=allSpeciesNames[!allSpeciesNames %in% Species],
			Height=NA,
			`Mean Circumference (cm)`=NA,
			Frequency=0,
			`Frequency per Zone`=0,
			`Trees per HA`=0,
			Density=0,
			`Basal Area per HA`=0,
			`Relative Cover`=0,
			`Relative Frequency`=0,
			`Relative Density`=0,
			`Relative Importance`=0
		),
		by=key
	] %>% rbind(
		sppDF
	) %>% arrange(
		key,
		Species
	) %>% select(
		-key
	) %>% return()
}


	