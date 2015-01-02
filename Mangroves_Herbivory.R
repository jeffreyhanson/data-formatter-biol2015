Mangrove_Herbivory=DATA_PREP$new(
	column_classes=list(
		"Date"="character",
		"Time"="character",
		"group"="character",
		"zonal_point"="character",
		"quarter"="character",
		"stems"="character",
		"Intertidal_zone"="character",
		
		"Latitude"="numeric",
		"Longitude"="numeric",
		"Distance_from_point"="numeric",
		"Height"="numeric",
		"Circumference"="numeric",
		"X__of_leaf_missing"="numeric",
		"number_of_Mines"="numeric",
		"number_of_Galls"="numeric",
		
		"Bud_1_Live"="logical",
		"Bud_1_Dead"="logical",
		"Bud_2_Live"="logical",
		"Bud_2_Dead"="logical",
		"Bud_3_Live"="logical",
		"Bud_3_Dead"="logical",
		"leaf_distorted__"="logical"
	),
	format=function(inpDF) {
		inpDF %>% setnames(c("group","zonal_point","TapePoint","quarter","stems","X__of_leaf_missing","leaf_distorted__","number_of_Mines","number_of_Galls") ,c("Group","Intertidal_zone","Tape_point","Quarter","Stems","Percent_of_leaf_missing","Leaf_distorted","Number_of_Mines","Number_of_Galls"))
		inpDF %>% transform(
				Longitude=na.locf(Longitude),
				Latitude=na.locf(Latitude),
				Bud_1_Live=convert2bool(Bud_1_Live),
				Bud_1_Dead=convert2bool(Bud_1_Dead),
				Bud_2_Live=convert2bool(Bud_2_Live),
				Bud_2_Dead=convert2bool(Bud_2_Dead),
				Bud_3_Live=convert2bool(Bud_3_Live),
				Bud_3_Dead=convert2bool(Bud_3_Dead),
				Percent_of_leaf_missing={Percent_of_leaf_missing/100},
				Leaf_distorted=convert2bool(Leaf_distorted)
		) %>% filter(grepl("Quarter",Quarter)) %>% return()
	},
	errors=list(
		ERROR_TEMPLATE.FACTOR("Intertidal_zone", c('Seaward','Landward - Creek')),
		ERROR_TEMPLATE.SEQUENCE.REPEATS("Quarter", c('quarter 1', 'quarter 2', 'quarter 3', 'quarter 4'), by="Intertidal_zone"),
		ERROR_TEMPLATE.SEQUENCE.REPEATS("Stems", c("stem 1", "stem 2", "stem 3", "stem 4", "stem 5", "stem 6", "stem 7", "stem 8"), by=c("Intertidal_zone","Quarter")),
		ERROR_TEMPLATE.SEQUENCE.NO_REPEATS("Leaf", c("Leaf 1", "Leaf 2", "Leaf 3", "Leaf 4", "Leaf 5", "Leaf 6"), by=c("Intertidal_zone","Quarter","Stems")),
		ERROR_TEMPLATE.TRUNCATED("Tape_point"),
		ERROR_TEMPLATE.TRUNCATED("Distance_from_point"),
		ERROR_TEMPLATE.OUTLIER("Distance_from_point"),
		ERROR_TEMPLATE.TRUNCATED("Height"),
		ERROR_TEMPLATE.OUTLIER("Height"),
		ERROR_TEMPLATE.TRUNCATED("Circumference"),
		ERROR_TEMPLATE.OUTLIER("Circumference"),
		ERROR_TEMPLATE.PERCENT("Percent_of_leaf_missing"),
		ERROR_TEMPLATE.POISSON("Number_of_Mines"),
		ERROR_TEMPLATE.OUTLIER("Number_of_Mines"),
		ERROR_TEMPLATE.POISSON("Number_of_Galls"),
		ERROR_TEMPLATE.OUTLIER("Number_of_Galls")
	),	
	process=function(inpDF) {
		stemDF=inpDF[,
			list(
				DateTime=min(DateTime, n.rm=TRUE),
				Longitude=mean(Longitude,na.rm=TRUE),
				Latitude=mean(Latitude,na.rm=TRUE),
				Distance_from_point=mean(Distance_from_point,na.rm=TRUE)
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
		])
		tempDF=stemDF[,
			list(
				DateTime=min(DateTime, n.rm=TRUE),
				Longitude=mean(Longitude,na.rm=TRUE),
				Latitude=mean(Latitude,na.rm=TRUE),
				Distance_from_point=mean(Distance_from_point,na.rm=TRUE)
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