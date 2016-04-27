Crabs=DATA_PREP$new(
	column_classes=list(
		"Date"="character",
		"Time"="character",
		"group name"="character",
		"Distance along tape"="numeric",
		"Distance along tape1"="numeric",
		"Plant type?"="character",
		"Litter"="numeric",
		"Groundcover Identification"="character",
		"Ground Cover"="numeric",
		"Trees/shrubs identification"="character",
		"Active Burrows"="numeric",
		"Inactive Burrows"="numeric",
		">2cm"="numeric",
		"DunePostition"="character",
		"Latitude"="numeric",
		"Longitude"="numeric",
		"Accuracy"="numeric"
	),
	format=function(inpDF) {
		inpDF %T>% setnames(
			c("Date", "Time", "Group", "Distance along tape", "Distance along tape1", 
				"Plant type", "Litter", "Groundcover Identification", "Ground Cover", 
				"Trees/shrubs identification", "Active Burrows", "Inactive Burrows", 
				"Holes >2cm", "DunePostition", "Latitude", "Longitude", "Accuracy")
		) %>% mutate(
			`Distance along tape`= replace(`Distance along tape`, which(is.na(`Distance along tape`)), `Distance along tape1`[which(is.na(`Distance along tape`))]),
			`Ground Cover`=`Ground Cover`/100,
			`Litter cover`=Litter/100,
			`Distance along tape (factor)`=paste('Point',`Distance along tape`)
		) %>% filter(
			!is.na(`Distance along tape`)
		) %>% `[`(,
			list(
				`Distance along tape`=first(`Distance along tape`),
				Date=first(na.omit(Date), default=as(NA, class(Date))),
				Time=first(na.omit(Time), default=as(NA, class(Time))),
				Longitude=first(na.omit(Longitude), default=as(NA, class(Longitude))),
				Latitude=first(na.omit(Latitude), default=as(NA, class(Latitude))),
				`Dune position`=last(na.omit(DunePostition), default=as(NA, class(DunePostition))),
				`Litter cover`=last(na.omit(`Litter cover`), default=as(NA, class(`Litter cover`))),
				`Number active burrows`=last(na.omit(`Active Burrows`), default=as(NA, class(`Active Burrows`))),
				`Number inactive burrows`=last(na.omit(`Inactive Burrows`), default=as(NA, class(`Inactive Burrows`))),
				`Number burrows >2cm across`=last(na.omit(`Holes >2cm`), default=as(NA, class(`Holes >2cm`))),
				
				`Angular Pigface`=extractValue(.SD, 'Groundcover Identification', 'Angular pigface ', 'Ground Cover', default=0),
				`Coast Headland Pea`=extractValue(.SD, 'Groundcover Identification', 'Coast Headland Pea', 'Ground Cover', default=0),
				`Beach Morning Glory`=extractValue(.SD, 'Groundcover Identification', 'Beach morning glory', 'Ground Cover', default=0),
				`Beach Primrose`=extractValue(.SD, 'Groundcover Identification', 'Beach primrose ', 'Ground Cover', default=0),
				`Beach Sawthistle`=extractValue(.SD, 'Groundcover Identification', 'Beach Sawthistle', 'Ground Cover', default=0),
				`Beach Couch`=extractValue(.SD, 'Groundcover Identification', 'Beach Couch', 'Ground Cover', default=0),
				`Beach Spinifex`=extractValue(.SD, 'Groundcover Identification', 'Beach Spinifex', 'Ground Cover', default=0),
				`Blady Grass`=extractValue(.SD, 'Groundcover Identification', 'Blady Grass', 'Ground Cover', default=0),
				`Blue Flax Lily`=extractValue(.SD, 'Groundcover Identification', 'Blue Flax Lily', 'Ground Cover', default=0),
				`Coastal Jack Bean`=extractValue(.SD, 'Groundcover Identification', 'Coastal Jack bean', 'Ground Cover', default=0),
				`Coastal Lovegrass`=extractValue(.SD, 'Groundcover Identification', 'Coastal Lovegrass', 'Ground Cover', default=0),
				`Devils Twine`=extractValue(.SD, 'Groundcover Identification', 'Devil\'s Twine', 'Ground Cover', default=0),
				`Dune Oxalis`=extractValue(.SD, 'Groundcover Identification', 'Dune Oxalis', 'Ground Cover', default=0),
				`Emilia`=extractValue(.SD, 'Groundcover Identification', 'Emilia', 'Ground Cover', default=0),
				`Everlasting Daisy`=extractValue(.SD, 'Groundcover Identification', 'Everlasting Daisy', 'Ground Cover', default=0),
				`Hillside Burrgrass`=extractValue(.SD, 'Groundcover Identification', 'Hillside Burrgrass ', 'Ground Cover', default=0),
				`Marine Couch`=extractValue(.SD, 'Groundcover Identification', 'Marine Couch', 'Ground Cover', default=0),
				`Mossman River Grass`=extractValue(.SD, 'Groundcover Identification', 'Mossman River grass', 'Ground Cover', default=0),
				`Native Cobblers Pegs`=extractValue(.SD, 'Groundcover Identification', 'Native Cobbler\'s Pegs', 'Ground Cover', default=0),
				`Scented Fan Flower`=extractValue(.SD, 'Groundcover Identification', 'Scented Fan Flower', 'Ground Cover', default=0),
				`Snake Vine`=extractValue(.SD, 'Groundcover Identification', 'Snake vine ', 'Ground Cover', default=0),
				`Tape Vine`=extractValue(.SD, 'Groundcover Identification', 'Tape Vine', 'Ground Cover', default=0),
				`Yellow Beach Bean`=extractValue(.SD, 'Groundcover Identification', 'Yellow Beach Bean', 'Ground Cover', default=0),
				
				`Coastal Banksia`=sum(`Trees/shrubs identification`=='Coastal Banksia'),
				`Red Ash`=sum(`Trees/shrubs identification`=='Red Ash'),
				`Coastal Canthium`=sum(`Trees/shrubs identification`=='Coastal Canthium'),
				`Native Cherry`=sum(`Trees/shrubs identification`=='Native Cherry'),
				`Coastal Screw Pine`=sum(`Trees/shrubs identification`=='Coastal Screw-pine'),
				`Coastal Wattle`=sum(`Trees/shrubs identification`=='Coastal Wattle'),
				`Coastal She oak`=sum(`Trees/shrubs identification`=='Coastal She-oak'),
				`Black Wattle`=sum(`Trees/shrubs identification`=='Black Wattle'),
				`Twiggy Homoranthus`=sum(`Trees/shrubs identification`=='Twiggy Homoranthus'),
				`Mangrove Boobialla`=sum(`Trees/shrubs identification`=='Mangrove Boobialla')
			),
			by=list(Group, `Distance along tape (factor)`)
		) %>% return()
	},
	errors=list(
		ERROR_TEMPLATE.FACTOR('Dune position', c('Upslope','Crest','Downslope','Trough')),
		ERROR_TEMPLATE.PERCENT('Litter cover'),
		ERROR_TEMPLATE.TRUNCATED('Number active burrows'),
		ERROR_TEMPLATE.OUTLIER('Number active burrows'),
		ERROR_TEMPLATE.TRUNCATED('Number inactive burrows'),
		ERROR_TEMPLATE.OUTLIER('Number inactive burrows'),
		ERROR_TEMPLATE.TRUNCATED('Number burrows >2cm across'),
		ERROR_TEMPLATE.OUTLIER('Number burrows >2cm across'),
		
		ERROR_TEMPLATE.PERCENT('Angular Pigface', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Coast Headland Pea', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Beach Morning Glory', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Beach Primrose', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Beach Sawthistle', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Beach Couch', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Beach Spinifex', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Blady Grass', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Blue Flax Lily', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Coastal Jack Bean', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Coastal Lovegrass', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Devils Twine', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Dune Oxalis', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Emilia', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Everlasting Daisy', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Hillside Burrgrass', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Marine Couch', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Mossman River Grass', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Native Cobblers Pegs', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Scented Fan Flower', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Snake Vine', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Tape Vine', allowNA=TRUE),
		ERROR_TEMPLATE.PERCENT('Yellow Beach Bean', allowNA=TRUE),
		
		ERROR_TEMPLATE.TRUNCATED('Coastal Banksia'),
		ERROR_TEMPLATE.TRUNCATED('Red Ash'),
		ERROR_TEMPLATE.TRUNCATED('Coastal Canthium'),
		ERROR_TEMPLATE.TRUNCATED('Native Cherry'),
		ERROR_TEMPLATE.TRUNCATED('Coastal Screw Pine'),
		ERROR_TEMPLATE.TRUNCATED('Coastal Wattle'),
		ERROR_TEMPLATE.TRUNCATED('Coastal She oak'),
		ERROR_TEMPLATE.TRUNCATED('Black Wattle'),
		ERROR_TEMPLATE.TRUNCATED('Twiggy Homoranthus'),
		ERROR_TEMPLATE.TRUNCATED('Mangrove Boobialla')
	),
	process=function(inpDF, ...) {
		if (exists('omitRows'))
			inpDF=inpDF[!omitRows,]
		inpDF %>% 
			mutate(
				`Zone` = cut(
					`Distance along tape`,
					c(-1, 20, 40, 60, 80, 100, 120),
					labels=c('0-20m','20-40m','40-60m','60-80m','80m-100m','100m-120m')
				),
				`Ratio active burrows`=(`Number active burrows`)/(`Number active burrows`+`Number inactive burrows`),
				`Ratio >2cm across burrows`=(`Number burrows >2cm across`)/(`Number active burrows`+`Number inactive burrows`),
				`Number of trees`=
					`Coastal Banksia` +
					`Red Ash` +
					`Coastal Canthium` +
					`Native Cherry` +
					`Coastal Screw Pine` +
					`Coastal Wattle` +
					`Coastal She oak` +
					`Black Wattle` +
					`Twiggy Homoranthus` +
					`Mangrove Boobialla`
				,
				`Number of ground plants`=
					(`Angular Pigface`>0) +
					(`Coast Headland Pea`>0) +
					(`Beach Morning Glory`>0) +
					(`Beach Primrose`>0) +
					(`Beach Sawthistle`>0) +
					(`Beach Couch`>0) +
					(`Beach Spinifex`>0) +
					(`Blady Grass`>0) +
					(`Blue Flax Lily`>0) +
					(`Coastal Jack Bean`>0) +
					(`Coastal Lovegrass`>0) +
					(`Devils Twine`>0) +
					(`Dune Oxalis`>0) +
					(`Emilia`>0) +
					(`Everlasting Daisy`>0) +
					(`Hillside Burrgrass`>0) +
					(`Marine Couch`>0) +
					(`Mossman River Grass`>0) +
					(`Native Cobblers Pegs`>0) +
					(`Scented Fan Flower`>0) +
					(`Snake Vine`>0) +
					(`Tape Vine`>0) +
					(`Yellow Beach Bean`>0)
				,
				`Living ground cover`=
					`Angular Pigface` +
					`Coast Headland Pea` +
					`Beach Morning Glory` +
					`Beach Primrose` +
					`Beach Sawthistle` +
					`Beach Couch` +
					`Beach Spinifex` +
					`Blady Grass` +
					`Blue Flax Lily` +
					`Coastal Jack Bean` +
					`Coastal Lovegrass` +
					`Devils Twine` +
					`Dune Oxalis` +
					`Emilia` +
					`Everlasting Daisy` +
					`Hillside Burrgrass` +
					`Marine Couch` +
					`Mossman River Grass` +
					`Native Cobblers Pegs` +
					`Scented Fan Flower` +
					`Snake Vine` +
					`Tape Vine` +
					`Yellow Beach Bean`	,
				`Total number burrows` = `Number active burrows`+`Number inactive burrows`
			) %>% select(
				Date,
				Time,
				Group,
				`Distance along tape`,
				`Zone`,
				Longitude:`Number burrows >2cm across`,
				`Total number burrows`,
				`Ratio active burrows`:`Living ground cover`,
				`Angular Pigface`:`Mangrove Boobialla`
			) %>% return()
	}
)

