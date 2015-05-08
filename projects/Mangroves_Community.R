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
	process=function(inpDF) {
		inpDF %>% calculate_PCQ_metrics(by=c('Transect','Zone')) %>% return()
	}
)



calculate_PCQ_metrics=function(inpDF, columns) {
	## init
	allSpeciesNames=c("Aegialitis annulata","Aegiceras corniculatum","Avicennia marina","Ceriops australis","Lumnitzera racemosa","Rhizophora stylosa","Osbornia octodonta","Other?")
	inpDF$key=aaply(inpDF, 1, function(x){paste(x[,columns,with=FALSE,drop=TRUE])})
	setkey(inpDF, key)
	
	## main processing
	# calculate top level values
	inpDF[,nSamples:=n_distinct(Group),by=key]
	inpDF[,nQuarter:=n(),by=key]
	inpDF[,meanDistBetweenTrees:=mean(`Distance from point (m)`, na.rm=TRUE),by=key]
	inpDF[,densityAllSpp:=10000/(meanDistanceBetweenTrees^2),by=key]

	# calculate species level data
	sppDF=inpDF[,
		list(
			Longitude=mean(Longitude,na.rm=TRUE),
			Latitude=mean(Latitude,na.rm=TRUE),
			Height=mean(`Height (m)`),
			Freq=n(),
			FreqPerQt=Freq/first(nQuarter),
			TreesPerHA=FreqPerQt*first(densityAllSpp),
			Density=n_distinct(Group)/first(nSamples),
			BasalAreaPerHA=(mean(`Circumference (m)`, na.rm=TRUE) * TreesPerHA) / 10000
		),
		by=list(key, Species)
	]
	sppDF[,RelativeCover:=BasalAreaPerHA/sum(BasalAreaPerHA),]
	sppDF[,RelativeFrequency:=FreqPerQt/sum(FreqPerQt),]
	sppDF[,RelativeDensity:=Density*densityAllSpp,]
	sppDF[,RelativeImportance=RelativeCover+RelativeFrequency+RelativeDensity,]
	
	# add in missing species with zeros
	sppDF[,
		list(
			Longitude=mean(Longitude,na.rm=TRUE),
			Latitude=mean(Latitude,na.rm=TRUE),
			Species=allSpeciesNames[!allSpeciesNames %in% Species]
		),
		by=key
	] %>% mutate(
			Height=NA,
			Freq=0,
			FreqPerQt=0,
			TreesPerHA=0,
			Density=0,
			BasalAreaPerHA=0,
			RelativeCover=0,
			RelativeFrequency=0,
			RelativeDensity=0,
			RelativeImportance=0
	) %>% rbind(
		sppDF
	) %>% arrange(
		key
	) %>% select(
		-key, -nSamples, -nQuarter, -meanDistBetweenTrees, -densityAllSpp
	) %>% return()
}
