Mangroves_Herbivory=DATA_PREP$new(
	column_classes=list(
		"Date"="character",
		"Time"="character",
		"group"="character",
		"Latitude"="numeric",
		"Longitude"="numeric",
		"location notes"="character",
		"zonal point"="character",
		"TapePoint"="numeric",
		"quarter"="character",
		"Distance from point"="numeric",
		"Height"="numeric",
		"Circumference"="numeric",
		"stems"="character",
		"Bud 1 Live"="logical",
		"Bud 1 Dead"="logical",
		"Bud 2 Live"="logical",
		"Bud 2 Dead"="logical",
		"Bud 3 Live"="logical",
		"Bud 3 Dead"="logical",
		"leaf distorted ?"="logical",
		"% of leaf missing"="numeric",
		"number of Mines"="numeric",
		"number of Galls"="numeric",
		"Leaf"="character"
	),
	format=function(inpDF) {
		inpDF %>% setnames(c("group","zonal point","TapePoint","quarter","stems","% of leaf missing","leaf distorted ?","number of Mines","number of Galls", "location notes") ,c("Group","Intertidal zone","Tape point","Quarter","Stem","Percent of leaf missing","Leaf distorted","Number of Mines","Number of Galls", "Location notes"))
		inpDF %>% mutate(
				Longitude=na.locf(Longitude),
				Latitude=na.locf(Latitude),
				`Bud 1 Live`=convert2bool(`Bud 1 Live`),
				`Bud 1 Dead`=convert2bool(`Bud 1 Dead`),
				`Bud 2 Live`=convert2bool(`Bud 2 Live`),
				`Bud 2 Dead`=convert2bool(`Bud 2 Dead`),
				`Bud 3 Live`=convert2bool(`Bud 3 Live`),
				`Bud 3 Dead`=convert2bool(`Bud 3 Dead`),
				`Quarter`=gsub('quarter','Quarter',Quarter),
				`Stem`=gsub('stem','Stem',Stem),
				`Percent of leaf missing`={`Percent of leaf missing`/100},
				`Leaf distorted`=convert2bool(`Leaf distorted`)
			) %>% 
			filter(grepl("Quarter",Quarter)) %>% 
			select( `Date`,
				`Time`,
				`Group`,
				`Intertidal zone`,
				`Tape point`,
				`Quarter`,
				`Distance from point`,
				`Height`,
				`Circumference`,
				`Stem`,
				`Leaf`, 
				`Bud 1 Live`,
				`Bud 2 Live`,
				`Bud 3 Live`,
				`Leaf distorted`,
				`Percent of leaf missing` ,
				`Number of Mines`,
				`Number of Galls`,
				`Longitude`,
				`Latitude`,
				`Location notes`
			) %>% return()
	},
	errors=list(
		ERROR_TEMPLATE.FACTOR("Intertidal zone", c('Seaward','Landward - Creek')),
		ERROR_TEMPLATE.SEQUENCE.REPEATS("Quarter", c('Quarter 1', 'Quarter 2', 'Quarter 3', 'Quarter 4'), by="Intertidal zone"),
		ERROR_TEMPLATE.SEQUENCE.REPEATS("Stem", c("Stem 1", "Stem 2", "Stem 3", "Stem 4", "Stem 5", "Stem 6", "Stem 7", "Stem 8"), by=c("Intertidal zone","Quarter")),
		ERROR_TEMPLATE.SEQUENCE.NO_REPEATS("Leaf", c("Leaf 1", "Leaf 2", "Leaf 3", "Leaf 4", "Leaf 5", "Leaf 6"), by=c("Intertidal zone","Quarter","Stem")),
		ERROR_TEMPLATE.TRUNCATED("Tape point"),
		ERROR_TEMPLATE.OUTLIER("Tape point"),
		ERROR_TEMPLATE.TRUNCATED("Distance from point"),
		ERROR_TEMPLATE.OUTLIER("Distance from point"),
		ERROR_TEMPLATE.TRUNCATED("Height"),
		ERROR_TEMPLATE.OUTLIER("Height"),
		ERROR_TEMPLATE.TRUNCATED("Circumference"),
		ERROR_TEMPLATE.OUTLIER("Circumference"),
		ERROR_TEMPLATE.PERCENT("Percent of leaf missing"),
		ERROR_TEMPLATE.POISSON("Number of Mines"),
		ERROR_TEMPLATE.OUTLIER("Number of Mines"),
		ERROR_TEMPLATE.POISSON("Number of Galls"),
		ERROR_TEMPLATE.OUTLIER("Number of Galls")
	),	
	process=function(inpDF) {
		stemDF=inpDF[,
			list(
				Date=na.omit(first(Date)),
				Time=na.omit(first(Time)),
				Longitude=mean(Longitude,na.rm=TRUE),
				Latitude=mean(Latitude,na.rm=TRUE),
				`Distance from point`=mean(`Distance from point`,na.rm=TRUE),
				Height=mean(Height,na.rm=TRUE),
				Circumference=mean(Circumference,na.rm=TRUE),
				`Number of buds alive`=sum(c(`Bud 1 Live`,`Bud 2 Live`,`Bud 3 Live`)),
				`Number of leaves distorted`=sum(`Leaf distorted`),
				`Number of Mines`=mean(`Number of Mines`,na.rm=TRUE),
				`Number of Galls`=mean(`Number of Galls`,na.rm=TRUE),
				`Average percent of leaf missing`=mean(`Percent of leaf missing`,na.rm=TRUE),
				`Number of leaves`=length(`Number of Galls`)
			),
			by=list(Group, `Intertidal zone`, `Tape point`, Quarter, Stems)
		]
		quarterDF=stemDF[,
			list(
				`Date`=na.omit(first(Date)),
				`Time`=na.omit(first(Time)),
				Longitude=mean(Longitude,na.rm=TRUE),
				Latitude=mean(Latitude,na.rm=TRUE),
				`Distance from point`=mean(`Distance from point`,na.rm=TRUE),
				Height=mean(Height,na.rm=TRUE),
				Circumference=mean(Circumference,na.rm=TRUE),
				`Average number of buds alive`=mean(`Number of buds alive`),
				`Average number of leaves distorted`=sum(`Number of leaves distorted`),
				`Average number of Mines`=mean(`Number of Mines`,na.rm=TRUE),
				`Average number of Galls`=mean(`Number of Galls`,na.rm=TRUE),
				`Average percent of leaf missing`=mean(`Average percent of leaf missing`,na.rm=TRUE),
				`Average number of leaves per stem`=mean(`Number of leaves`,na.rm=TRUE),
				`Number of stems`=length(Longitude)
			),
			by=list(Group, `Intertidal zone`, `Tape point`, Quarter)
		]
		return(quarterDF)
	}
)