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
		setnames(inpDF, names(inpDF), c('Date','Time','Group','Quadrat size','Vegetation type','data entry','Quadrat #','Tallest tree','Foliage cover','Ground cover','Litter depth','Tree Species DBH','Small trees','Latitude','Longitude','Shrub size','Tree Species Name','Acacia','Angophora','Araucaria','Banksia','Callitris','Casuarina','Eucalyptus','Lophostemon','Melaleuca','Syncarpia'))
		
		assign('inpDF', inpDF, envir=globalenv())
		
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
				
		inpDF[,`Tallest tree`:=ifelse(all(is.na(`Tallest tree`)),as.numeric(NA),last(na.omit(`Tallest tree`))),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[['Shrub size']][which(nchar(inpDF[['Shrub size']])==0)]=NA
		inpDF[,`Shrub size`:=ifelse(all(is.na(`Shrub size`)),as.character(NA),last(na.omit(`Shrub size`))),by=list(`Group`, `Quadrat size`, `Vegetation type`)]
		inpDF[['Quadrat #']][which(inpDF[['data entry']]=='DBH when trunk >10cms')]=0
		
		inpDF %>% 
		filter(`data entry` %in% c('Vegetation Assessment','DBH when trunk >10cms')) %>%
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
			`Tree Species Name`,
			`Tree Species DBH`,
		
			`Acacia Dominant`,
			`Angophora Dominant`,
			`Araucaria Dominant`,
			`Banksia Dominant`,
			`Callitris Dominant`,
			`Casuarina Dominant`,
			`Eucalyptus Dominant`,
			`Lophostemon Dominant`,
			`Melaleuca Dominant`,
			`Syncarpia Dominant`

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
	
		ERROR_TEMPLATE.TRUNCATED('Tree Species DBH'),
		ERROR_TEMPLATE.OUTLIER('Tree Species DBH')
	),
	process=function(inpDF, ...) {
		# remove rows to omit
		if (exists('omitRows'))
			inpDF=inpDF[!omitRows,]
			
		assign('inpDF', inpDF, envir=globalenv())
		
		## calculate metrics
		# initial calcs
		inpDF$area=as.numeric(sapply(strsplit(inpDF[['Quadrat size']], 'x'), '[[', 1)) * as.numeric(gsub('m', '', sapply(strsplit(inpDF[['Quadrat size']], 'x'), '[[', 2)))
		summaryDF=filter(inpDF, `Quadrat #`==0)[,
			list(
					`Basal Area`=sum(`Tree Species DBH`/12.56),
					`Frequency`=length(`Tree Species DBH`),
					`Density`=length(`Tree Species DBH`)/first(area)
			),
			by=list(`Group`, `Tree Species Name`, `Vegetation type`)
		]
		
		# relative calcs
		summaryDF %<>% mutate(
			`Relative Basal Area`=round(`Basal Area`/sum(`Basal Area`),2),
			`Relative Frequency`=round(`Frequency`/sum(`Frequency`),2),
			`Relative Density`=round(`Density`/sum(`Density`),2),
			`Relative Importance`=round((`Relative Basal Area`+`Relative Frequency`+`Relative Density`)/3,2)
		)
		
		# pad in missing species
		allnames=c("Acacia","Angophora","Araucaria","Banksia","Callitris","Casuarina","Eucalyptus","Lophostemon","Melaleuca","Syncarpia","Unknown")
		missDF = summaryDF[,.(`Tree Species Name`=allnames[which(!allnames %in% `Tree Species Name`)]), by=list(`Group`, `Vegetation type`)] 
		missDF %<>% mutate(
			`Basal Area`=0,
			`Frequency`=0,
			`Density`=0,
			`Relative Basal Area`=0,
			`Relative Frequency`=0,
			`Relative Density`=0,
			`Relative Importance`=0
		)
		
		# compile summaryDF
		summaryDF %<>% rbind(missDF) %>% mutate(
			Date=first(inpDF$Date),
			Time=first(inpDF$Time),
			Longitude=first(inpDF$Longitude),
			Latitude=first(inpDF$Latitude),
			`Quadrat size`=first(inpDF[['Quadrat size']])
			`Quadrat #`=0
		)
		
		# compile and return final data.table
		inpDF %<>% filter(`Quadrat #` > 0) %>% rbind(summaryDF, fill=TRUE)
		return(inpDF)
	}
)

