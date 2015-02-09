Mangroves_Herbivory=DATA_PREP$new(
	column_classes=list(
		"Date"="character",
		"Time"="character",
		"group"="character",
		"Latitude"="numeric",
		"Longitude"="numeric",
		"location notes"="character",
		"zonal point"="character",
		"TapePoint"="character",
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
		inpDF %>% setnames(c("group","zonal point","TapePoint","quarter","stems","% of leaf missing","leaf distorted ?","number of Mines","number of Galls") ,c("Group","Intertidal zone","Tape point","Quarter","Stems","Percent of leaf missing","Leaf distorted","Number of Mines","Number of Galls"))
		inpDF %>% mutate(
				Longitude=na.locf(Longitude),
				Latitude=na.locf(Latitude),
				`Bud 1 Live`=convert2bool(`Bud 1 Live`),
				`Bud 1 Dead`=convert2bool(`Bud 1 Dead`),
				`Bud 2 Live`=convert2bool(`Bud 2 Live`),
				`Bud 2 Dead`=convert2bool(`Bud 2 Dead`),
				`Bud 3 Live`=convert2bool(`Bud 3 Live`),
				`Bud 3 Dead`=convert2bool(`Bud 3 Dead`),
				`Percent of leaf missing`={`Percent of leaf missing`/100},
				`Leaf distorted`=convert2bool(`Leaf distorted`)
		) %>% filter(grepl("quarter",Quarter)) %>% return()
	},
	errors=list(
		ERROR_TEMPLATE.FACTOR("Intertidal zone", c('Seaward','Landward - Creek')),
		ERROR_TEMPLATE.SEQUENCE.REPEATS("Quarter", c('quarter 1', 'quarter 2', 'quarter 3', 'quarter 4'), by="Intertidal zone"),
		ERROR_TEMPLATE.SEQUENCE.REPEATS("Stems", c("stem 1", "stem 2", "stem 3", "stem 4", "stem 5", "stem 6", "stem 7", "stem 8"), by=c("Intertidal zone","Quarter")),
		ERROR_TEMPLATE.SEQUENCE.NO_REPEATS("Leaf", c("Leaf 1", "Leaf 2", "Leaf 3", "Leaf 4", "Leaf 5", "Leaf 6"), by=c("Intertidal zone","Quarter","Stems")),
		ERROR_TEMPLATE.TRUNCATED("Tape point"),
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
				DateTime=min(DateTime, n.rm=TRUE),
				Longitude=mean(Longitude,na.rm=TRUE),
				Latitude=mean(Latitude,na.rm=TRUE),
				Distance_from_point=mean(Distance_from_point,na.rm=TRUE),
				Height=mean(Height,na.rm=TRUE),
				Circumference=mean(Circumference,na.rm=TRUE),
				Number_of_buds_alive=sum(c(Bud_1_Live,Bud_2_Live,Bud_3_Live)),
				Number_of_leaves_distorted=sum(Leaf_distorted),
				Number_of_Mines=mean(Number_of_Mines,na.rm=TRUE),
				Number_of_Galls=mean(Number_of_Galls,na.rm=TRUE),
				Average_percent_of_leaf_missing=mean(Percent_of_leaf_missing,na.rm=TRUE),
				Number_of_leaves=length(Number_of_Galls)
			),
			by=list(Group, Intertidal_zone, Tape_point, Quarter, Stems)
		]
		tempDF=stemDF[,
			list(
				DateTime=min(DateTime, n.rm=TRUE),
				Longitude=mean(Longitude,na.rm=TRUE),
				Latitude=mean(Latitude,na.rm=TRUE),
				Distance_from_point=mean(Distance_from_point,na.rm=TRUE),
				Height=mean(Height,na.rm=TRUE),
				Circumference=mean(Circumference,na.rm=TRUE),
				Average_mumber_of_buds_alive=mean(Number_of_buds_alive),
				Average_number_of_leaves_distorted=sum(Number_of_leaves_distorted),
				Average_number_of_Mines=mean(Number_of_Mines,na.rm=TRUE),
				Average_number_of_Galls=mean(Number_of_Galls,na.rm=TRUE),
				Average_percent_of_leaf_missing=mean(Average_percent_of_leaf_missing,na.rm=TRUE),
				Average_number_of_leaves_per_stem=mean(Number_of_leaves,na.rm=TRUE),
				Number_of_stems=length(Longitude)
			),
			by=list(Group, Intertidal_zone, Tape_point, Quarter)
		]
		return(tempDF)
	}
)