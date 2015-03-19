Spiders=DATA_PREP$new(
	column_classes=list(
		"Date"="character",
		"Time"="character",
		"GroupName"="character",
		"Type_of_Area"="character",
		"Main_Tree_Genus"="character",
		"Hole_size_in_mm"="numeric",
		"NN_Distance__cm_"="numeric",
		"NN_Hole_Size__mm_"="numeric",
		"Hole_Temp"="numeric",
		"Hole_Humidity"="numeric",
		"Light_Reading"="numeric",
		"Light_Reading_1"="numeric",
		"Hole_Temp_Neighbour"="numeric",
		"Hole_Humidity_Neighbour"="numeric",
		"Light_Reading_Neighbour"="numeric",
		"To_Nearest_Tree__m_"="numeric",
		"Tree_Height__m_"="numeric",
		"CBH__cm_"="numeric",
		"Litter_Cover"="numeric",
		"Grass_"="numeric",
		"Herb_"="numeric",
		"Creeper_"="numeric",
		"Shrub_"="numeric",
		"Dead_Log_"="numeric",
		"Litter_Depth"="numeric",
		"foliage_cover"="numeric",
		"Air_Temp"="numeric",
		"Air_Humidity"="numeric",
		"Hole_Distance__cm_"="numeric",
		"Hole_Size__mm_"="numeric",
		"Latitude"="numeric",
		"Longitude"="numeric"
	),
	format=function(inpDF) {
		# change column names
		inpDF %>% stop %>% setnames(...)
	
		# average neighbor statistics
		inpDF[which(is.na(inpDF[["Add Extra Holes"]])),"Add Extra Holes",with=FALSE]="Absent"
		currSize=c()
		currDist=c()
		for (currRow in seq_len(nrow(inpDF))) {
			if (inpDF$Add_Extra_Holes[currRow] == "Yes") {
				currSize=c(currSize, inpDF[["Hole Size mm"]][currRow])
				currDist=c(currSize, inpDF[["Hole Distance cm"]][currRow])
			} else if (inpDF[["Add Extra Holes"]][currRow] == "No") {		
				inpDF[["Hole Size mm"]][currRow]<<-suppressWarnings(mean(currSize,na.rm=TRUE))
				inpDF[["Hole Distance cm"]][currRow]<<-suppressWarnings(mean(currDist,na.rm=TRUE))
				currSize=c()
				currDist=c()
			} else {
				currSize=c()
				currDist=c()
			}
		}

		# convert data to percentages
		inpDF %>% 
		filter(`Add Extra Holes`!="Yes") %>%
		mutate(
			`Hole Humidity`=`Hole Humidity`/100
			`Hole Humidity Neighbour`=`Hole Humidity Neighbour`/100
			`Litter Cover`=`Litter Cover`/100
			`Grass`=`Grass`/100
			`Herb`=`Herb`/100
			`Creeper`=`Creeper`/100
			`Shrub`=`Shrub`/100
			`Dead Log`=`Dead Log`/100
			`foliage cover`=`foliage cover`/100
			`Air Humidity`=`Air Humidity`/100
		) %>% return()
	},
	errors=list(
		checkCol.factor("Type_of_Area", c("Spider Infested","Spiders absent")),
		checkCol.truncated("Hole_size_in_mm", data=bioData[["data"]][which(bioData[["data"]][["Type_of_Area"]] == "Absence"),]),
		checkCol.truncated("NN_Distance__cm_", data=bioData[["data"]][which(bioData[["data"]][["Type_of_Area"]] == "Absence"),]),
		checkCol.truncated("NN_Hole_Size__mm_", data=bioData[["data"]][which(bioData[["data"]][["Type_of_Area"]] == "Absence"),]),
		checkCol.normal("Hole_Temp"),
		checkCol.percent("Hole_Humidity"),
		checkCol.truncated("Light_Reading"),
		checkCol.normal("Hole_Temp_Neighbour", data=bioData[["data"]][which(bioData[["data"]][["Type_of_Area"]] == "Absence"),]),
		checkCol.percent("Hole_Humidity_Neighbour", data=bioData[["data"]][which(bioData[["data"]][["Type_of_Area"]] == "Absence"),]),
		checkCol.truncated("Light_Reading_Neighbour", data=bioData[["data"]][which(bioData[["data"]][["Type_of_Area"]] == "Absence"),]),
		checkCol.truncated("To_Nearest_Tree__m_"),
		checkCol.truncated("Tree_Height__m_"),
		checkCol.truncated("CBH__cm_"),
		checkCol.percent("Litter_Cover"),
		checkCol.percent("Grass_"),
		checkCol.percent("Herb_"),
		checkCol.percent("Herb_"),
		checkCol.percent("Creeper_"),
		checkCol.percent("Shrub_"),
		checkCol.percent("Dead_Log_"),
		checkCol.truncated("Litter_Depth"),
		checkCol.percent("foliage_cover"),
		checkCol.normal("Air_Temp"),
		checkCol.percent("Air_Humidity"),
		checkCol.truncated("Hole_Distance__cm_", data=bioData[["data"]][which(bioData[["data"]][["Type_of_Area"]] == "Absence"),]),
		checkCol.truncated("Hole_Size__mm_", data=bioData[["data"]][which(bioData[["data"]][["Type_of_Area"]] == "Absence"),])
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	),
	process=function(inpDF) {
		return(inpDF)
	}
)

