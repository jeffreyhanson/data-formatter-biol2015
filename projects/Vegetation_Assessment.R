Vegetation_Assessment=DATA_PREP$new(
	column_classes=list(
		'Date'='character',
		'Time'="character",
		'Group name'="character",
		' Quadrat size'="character",
		'Vegetation type'="character",
		'data entry'="character",
		'Quadrat #'="numeric",
		'Tallest tree'="numeric",
		'foliage cover'="numeric",
		'ground cover'="numeric",
		'litter depth'="numeric",
		'DBH'="numeric",
		'small trees'="numeric",
		'Latitude'="numeric",
		'Longitude'="numeric",
		'Shrub size'="character",
		'DBH>10'="character",
		'Acacia'="character",
		'Angophora'="character",
		'Araucaria'="character",
		'Banksia'="character",
		'Callitris'="character",
		'Casuarina'="character",
		'Eucalyptus'="character",
		'Lophostemon'="character",
		'Melaleuca'="character",
		'Syncarpia'="character"
	),
	format=function(inpDF) {
		setnames(inpDF, names(inpDF), c('Date','Time','Group','Quadrat size','Vegetation type','data entry','Quadrat #','Tallest tree','Foliage cover','Ground cover','Litter depth','DBH','Small trees','Latitude','Longitude','Shrub size','DBH>10','Acacia','Angophora','Araucaria','Banksia','Callitris','Casuarina','Eucalyptus','Lophostemon','Melaleuca','Syncarpia'))
		
		if (all(!is.na(inpDF$Longitude)))
			inpDF[,Longitude:=na.locf(Longitude)]
		
		if (all(!is.na(inpDF$Latitude)))
			inpDF[,Latitude:=na.locf(Latitude)]

		inpDF[,`Foliage cover`:=`Foliage cover`/100]
		inpDF[,`Ground cover`:=`Ground cover`/100]

		inpDF[,`Acacia Dominant`:=ifelse(is.blank(Acacia),0,1),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Angophora Dominant`:=ifelse(is.blank(Angophora),0,1),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Araucaria Dominant`:=ifelse(is.blank(Araucaria),0,1),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Banksia Dominant`:=ifelse(is.blank(Banksia),0,1),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Callitris Dominant`:=ifelse(is.blank(Callitris),0,1),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Casuarina Dominant`:=ifelse(is.blank(Casuarina),0,1),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Eucalyptus Dominant`:=ifelse(is.blank(Eucalyptus),0,1),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Lophostemon Dominant`:=ifelse(is.blank(Lophostemon),0,1),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Melaleuca Dominant`:=ifelse(is.blank(Melaleuca),0,1),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Syncarpia Dominant`:=ifelse(is.blank(Syncarpia),0,1),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		
		inpDF[,`Acacia DBH>10cm`:=extractValue(inpDF=.SD, refCol='DBH>10', refValue='Acacia', extractCol='DBH'),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Angophora DBH>10cm`:=extractValue(inpDF=.SD, refCol='DBH>10', refValue='Angophora', extractCol='DBH'),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Araucaria DBH>10cm`:=extractValue(inpDF=.SD, refCol='DBH>10', refValue='Araucaria', extractCol='DBH'),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Banksia DBH>10cm`:=extractValue(inpDF=.SD, refCol='DBH>10', refValue='Banksia', extractCol='DBH'),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Callitris DBH>10cm`:=extractValue(inpDF=.SD, refCol='DBH>10', refValue='Callitris', extractCol='DBH'),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Casuarina DBH>10cm`:=extractValue(inpDF=.SD, refCol='DBH>10', refValue='Casuarina', extractCol='DBH'),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Eucalyptus DBH>10cm`:=extractValue(inpDF=.SD, refCol='DBH>10', refValue='Eucalyptus', extractCol='DBH'),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Lophostemon DBH>10cm`:=extractValue(inpDF=.SD, refCol='DBH>10', refValue='Lophostemon', extractCol='DBH'),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Melaleuca DBH>10cm`:=extractValue(inpDF=.SD, refCol='DBH>10', refValue='Melaleuca', extractCol='DBH'),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[,`Syncarpia DBH>10cm`:=extractValue(inpDF=.SD, refCol='DBH>10', refValue='Syncarpia', extractCol='DBH'),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		
		inpDF[,`Tallest tree`:=ifelse(all(is.na(`Tallest tree`)),as.numeric(NA),last(na.omit(`Tallest tree`))),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[['Shrub size']][which(nchar(inpDF[['Shrub size']])==0)]=NA
		inpDF[,`Shrub size`:=ifelse(all(is.na(`Shrub size`)),as.character(NA),last(na.omit(`Shrub size`))),by=list(`Group`, `Quadrat size`, `Vegetation type`)]

		inpDF %>% 
		filter(`data entry`=='Vegetation Assessment') %>%
		select(
			Date,
			Time,
			Longitude,
			Latitude,
			`Group`,
			`Quadrat size`,
			`Vegetation type`,
			`Quadrat #`,
			`Tallest tree`,
			`Foliage cover`,
			`Ground cover`,
			`Litter depth`,
			`Small trees`,
			`Shrub size`,
		
			`Acacia Dominant`,
			`Angophora Dominant`,
			`Araucaria Dominant`,
			`Banksia Dominant`,
			`Callitris Dominant`,
			`Casuarina Dominant`,
			`Eucalyptus Dominant`,
			`Lophostemon Dominant`,
			`Melaleuca Dominant`,
			`Syncarpia Dominant`,

			`Acacia DBH>10cm`,
			`Angophora DBH>10cm`,
			`Araucaria DBH>10cm`,
			`Banksia DBH>10cm`,
			`Callitris DBH>10cm`,
			`Casuarina DBH>10cm`,
			`Eucalyptus DBH>10cm`,
			`Lophostemon DBH>10cm`,
			`Melaleuca DBH>10cm`,
			`Syncarpia DBH>10cm`
		) %>% return()
	},
	errors=list(
		ERROR_TEMPLATE.FACTOR('Quadrat size', c('10x10m', '20x20m')),
		ERROR_TEMPLATE.FACTOR('Vegetation type', c(
			'Tall Open forest (Blackbutt_Satinay)',
			'Wallum Heath',
			'Retrogression',
			'Low Open forest',
			'Medium Open forest (Scribbly Gum)',
			'Mangroves',
			'Foredunes',
			'Rainforest'
		)),
		ERROR_TEMPLATE.TRUNCATED('Tallest tree'),
		ERROR_TEMPLATE.OUTLIER('Tallest tree'),
		ERROR_TEMPLATE.PERCENT('Foliage cover'),
		ERROR_TEMPLATE.PERCENT('Ground cover'),
		ERROR_TEMPLATE.TRUNCATED('Litter depth'),
		ERROR_TEMPLATE.FACTOR('Shrub size',c('0-2m','2-8m')),
		
		ERROR_TEMPLATE.BINARY('Acacia Dominant'),
		ERROR_TEMPLATE.BINARY('Angophora Dominant'),
		ERROR_TEMPLATE.BINARY('Araucaria Dominant'),
		ERROR_TEMPLATE.BINARY('Banksia Dominant'),
		ERROR_TEMPLATE.BINARY('Callitris Dominant'),
		ERROR_TEMPLATE.BINARY('Casuarina Dominant'),
		ERROR_TEMPLATE.BINARY('Eucalyptus Dominant'),
		ERROR_TEMPLATE.BINARY('Lophostemon Dominant'),
		ERROR_TEMPLATE.BINARY('Melaleuca Dominant'),
		ERROR_TEMPLATE.BINARY('Syncarpia Dominant'),
	
		ERROR_TEMPLATE.TRUNCATED('Acacia DBH>10cm'),
		ERROR_TEMPLATE.OUTLIER('Acacia DBH>10cm'),

		ERROR_TEMPLATE.TRUNCATED('Angophora DBH>10cm'),
		ERROR_TEMPLATE.OUTLIER('Angophora DBH>10cm'),
		
		ERROR_TEMPLATE.TRUNCATED('Araucaria DBH>10cm'),
		ERROR_TEMPLATE.OUTLIER('Araucaria DBH>10cm'),
		
		ERROR_TEMPLATE.TRUNCATED('Banksia DBH>10cm'),
		ERROR_TEMPLATE.OUTLIER('Banksia DBH>10cm'),

		ERROR_TEMPLATE.TRUNCATED('Callitris DBH>10cm'),
		ERROR_TEMPLATE.OUTLIER('Callitris DBH>10cm'),
		
		ERROR_TEMPLATE.TRUNCATED('Casuarina DBH>10cm'),
		ERROR_TEMPLATE.OUTLIER('Casuarina DBH>10cm'),
		
		ERROR_TEMPLATE.TRUNCATED('Eucalyptus DBH>10cm'),
		ERROR_TEMPLATE.OUTLIER('Eucalyptus DBH>10cm'),
		
		ERROR_TEMPLATE.TRUNCATED('Lophostemon DBH>10cm'),
		ERROR_TEMPLATE.OUTLIER('Lophostemon DBH>10cm'),
		
		ERROR_TEMPLATE.TRUNCATED('Melaleuca DBH>10cm'),
		ERROR_TEMPLATE.OUTLIER('Melaleuca DBH>10cm'),
		
		ERROR_TEMPLATE.TRUNCATED('Syncarpia DBH>10cm'),
		ERROR_TEMPLATE.OUTLIER('Syncarpia DBH>10cm')
		
	),
	process=function(inpDF, ...) {
		if (exists('omitRows'))
			inpDF=inpDF[!omitRows,]
		return(inpDF)
	}
)

