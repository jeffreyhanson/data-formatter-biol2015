ID = setRefClass("ID", 
	fields=list(Id="numeric"),
	methods=list(
		initialize=function() {
			Id<<-0
		},
		new=function(n=1) {
			ret=as.character(seq(Id, Id+n-1))
			Id<<-Id+n
			return(ret)
		}
	)
)

MANAGER=setRefClass("MANAGER",
	fields=list(
		.activeWeekNumber_CHR="character",
		.activeProjectName_CHR="character",
		.activeGroupColor_CHR="character",
		.activeGroupNames_CHR="character",
		
		.fullProjectData_DF="data.table",
		.activeGroupData_DF="data.table",
		.dataPrep="DATA_PREP",
		.activeView="character",
		.activeViewData_DF="data.table"
	),
	methods=list(
		#### initialize methods
		initialize=function() {
			.activeView="all"
			.activeWeekNumber_CHR<<-character(0)
			.activeProjectName_CHR<<-character(0)
			.activeGroupColor_CH<<-character(0)
			.activeGroupNames_CHR<<-character(0)
			.fullProjectData_DF<<-data.table(0)
			.activeGroupData_DF<<-data.table(0)
			.activeViewData_DF<<-data.table(0)
		},
		#### field validating methods
		isDirFieldsValid=function() {
			return(!is.empty(.activeWeekNumber_CHR) & !is.empty(.activeProjectName_CHR) & !is.empty(.activeGroupColor_CH))
		},
		isAllFieldsValid=function() {
			return(!is.empty(.activeWeekNumber_CHR) & !is.empty(.activeProjectName_CHR) & !is.empty(.activeGroupColor_CH) & !is.empty(.activeGroupNames_CHR))
		},
		
		#### disk interface methods
		loadProjectDataFromFile=function() {
			.fullProjectData_DF<<-.dataPrep$prepareData(rbind.fill(llply(dir(file.path(.activeWeekNumber_CHR, .activeProjectName_CHR, .activeGroupColor_CHR, "raw"), full.names=TRUE),fread)))
		},
		saveDataToFile=function() {
			# save cleaned data
			write.table(.activeGroupData_DF, file.path(.activeWeekNumber_CHR, .activeProjectName_CHR, .activeGroupColor_CHR, "cleaned"), sep=",", row.names=FALSE)
			# save formatted data
			write.table(.dataPrep$processData(.activeGroupData_DF), file.path(.activeWeekNumber_CHR, .activeProjectName_CHR, .activeGroupColor_CHR, "formatted"), sep=",", row.names=FALSE)
			# save compiled data
			write.table(rbind.fill(sapply(dir(file.path(.activeWeekNumber_CHR, .activeProjectName_CHR, .activeGroupColor_CHR, "formatted"),full.names=TRUE),fread)), file.path(.activeWeekNumber_CHR, .activeProjectName_CHR, .activeGroupColor_CHR, "compiled", "final.csv"), sep=",", row.names=FALSE)
		},
		
		### data manipulation methods
		getProjectGroupNames=function() {
			return(unique(fullProjectData_DF$Group_name))
		},
		setActiveWeekNumber_CHR=function(week_number) {
			.activeWeekNumber_CHR<<-week_number
		},		
		setActiveProjectName=function(project_name) {
			project_name=sub(" ", "_", project_name)
			.activeProjectName<<-project_name
			.dataPrep<<-get(project_name)
		},
		setActiveGroupColor=function(group_color) {
			.activeGroupColor_CHR<<-group_color
		},
		setActiveGroupNames=function(group_names) {
			.activeGroupNames_CHR<<-group_names
		},
		setActiveData=function() {
			.activeGroupData_DF<<-.fullProjectData_DF %>% filter(Group_names %in% group_names)
		},
		
		#### error handling methods
		scanDataForErrors=function() {
			tempErrors=.dataPrep$scanForErrors(.activeGroupData_DF)
			.errors[[sapply(tempErrors, function(x){return(x$.id)})]]<<-tempErrors
		},
		setErrorStatus=function(id, status) {
			.errors[[id]]$.status<<-status
		},
		setErrorIgnore=function(id, ignore) {
			.errors[[id]]$.ignore<<-ignore
		},
		
		#### render data table methods
		getFullProjectData=function() {
			return(data=.fullProjectData_DF, row=numeric(0), col=numeric(0))
		},
		getActiveGroupsData=function() {
			.activeView<<-"all"
			.activeViewData_DF<<-activeGroupData_DF
			errorMTX=error_list_to_matrix(.errors)
			return(data=.activeGroupData_DF, row=errorMTX[,1], col=errorMTX[,2], color=errorMTX[,3])
		},
		getAllDataWithFixedErrorsInActiveGroups=function() {
			.activeView<<-"fixed"
			errorMTX=error_list_to_matrix(.errors)
			errorMTX=errorMTX[which(errorMTX[,4]=="fixed"),]
			.activeGroupData_DF<<-.activeGroupData_DF[errorMTX[,1],drop=FALSE]
			return(data=.activeGroupData_DF, row=match(errorMTX[,1], rownames(.activeGroupData_DF)), col=errorMTX[,2], color=errorMTX[,3])
		},
		getAllDataWithIgnoredErrorsInActiveGroups=function() {
			.activeView<<-"ignored"
			errorMTX=error_list_to_matrix(.errors)
			errorMTX=errorMTX[which(errorMTX[,5]),]
			.activeGroupData_DF<<-.activeGroupData_DF[errorMTX[,1],drop=FALSE]
			return(data=.activeGroupData_DF, row=match(errorMTX[,1], rownames(.activeGroupData_DF)), col=errorMTX[,2], color=errorMTX[,3])
		},
		getAllDataWithErrorsInActiveGroups=function() {
			.activeView<<-"error"
			errorMTX=error_list_to_matrix(.errors)
			errorMTX=errorMTX[which(errorMTX[,4]=="error"),]
			.activeGroupData_DF<<-.activeGroupData_DF[errorMTX[,1],drop=FALSE]
			return(data=.activeGroupData_DF, row=match(errorMTX[,1], rownames(.activeGroupData_DF)), col=errorMTX[,2], color=errorMTX[,3])
		},
		getDataWithSpecificError=function(id) {
			return(list(row=match(.errors[[id]]$.row, rownames(.activeGroupData_DF)), color=highlightColor))
		}
	)
)

ERROR=setRefClass("ERROR",
	fields=list(
		.id="character",
		.status="character",
		.ignore="logical"
		.row="character",
		.col="character",
		.name="character",
		.description="character",
		.test="function"
	),
	methods=list(
		initialize=function(id, name, i, j, description, test) {
			.id<<-id
			.name<<-name
			.status<<-"error"
			.ignore<<-FALSE
			.row<<-i
			.col<<-j
			.description<<-description
			.test<<-test
		},
		test=function(inpDF) {
			if(nrow(.test(inpDF[i,j,drop=FALSE,with=FALSE]))==0) {
				.status<<-"fixed"
			} else {
				.status<<-"error"
			}
		},
		isValid=function() {
			return(.status=="fixed")
		},
		swapIgnore=function() {
			.ignore=!.ignore
		},
		repr=function() {
			return(paste0('
			<div class="feature well status-',.status,'-ignore-',ignore,'">
				<h5 class="toc-element-label">',.name,'(',.id,')</h5>
				<button class="btn sbs-action-button toc-element-zoom" id="',.id,'_zoom_btn" name="',.id,'" type="button" onclick="zoomItem(this.name)">
					<i class="fa fa-search-plus"></i>
				</button>
				<button class="btn sbs-action-button toc-element-swap" id="',.id,'_swap_btn" name="',.id,'" type="button" onclick="swapIgnoreItem(this.name)">
					<i class="fa fa-square-o"></i>
				</button>
			</div>
			')
		},
		color=function() {
			if (ignore) {
				return(ignoreColor)
			} else {
				if (.status=="fixed") {
					return(fixedColor)
				} else {
					return(errorColor)
				}
			}
		}
	)
)

ERROR_GENERATOR=setRefClass("ERROR_GENERATOR",
	fields=list(
		.id="ID",
		.name="character",
		.description="character",
		.errors="list",
		.test="function",
	),
	methods=list(
		initialize=function(id, name, description, test) {
			.id<<-id
			.description<<-description
			.test<<-test
		},
		testForErrors=function(inpDF) {
			.errors=alply(.test(inpDF),1, function(x) {
				ERROR$new(.id$new(), .name, x[1], x[2], .description, .test)
			})
		},
		isValid=function() {
			return(length(.errors)==0)
		},
		getErrors=function() {
			return(.errors)
		}
	)
)

DATA_PREP=setRefClass("DATA_PREP",
	fields=list(
		.column_classes_LST="list"
		.format_FUN="function"
		.errors_LST="list",
		.process_FUN="function"
	),
	methods=list(
		initialize=function(column_classes, format, errors, process) {
			.column_classes_LST<<-column_classes
			.format_FUN<<-format
			.errors_LST<<-errors
			.process_FUN<<-process
		},
		prepareData=function(inpDF) {			
			return(.format_FUN(data.table(as.data.frame(Map(as, inpDF[,names(colmn_classes),with=FALSE], .colmn_classes)))))
		},
		scanForErrors=function(inpDF) {
			return(llply(.errors_LST, inPDF))
		},
		processData=function(inpDF) {
			return(.process_FUN(inpDF))
		}
	)
)

ERROR_TEMPLATE.FACTOR=function(column_name, values) {
	return(ERROR_GENERATOR$new(id, "Invalid factor value.", paste0("Cell value should be:",phrase_FUN(values)), function(inpDF) {
		rows=which(!inpDF[[column_name]] %in% values)
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERRROR_TEMPLATE.SEQUENCE.REPEATS=function(column_name, values, by) {
	return(ERROR_GENERATOR$new(id, "Invalid value in sequence (repeats).", paste0("Cell values should be in the sequence:",phrase_FUN(values, "then")), function(inpDF) {
		vals=inpDF[[column_name]]
		names(vals)=seq_along(inpDF[[column_name]])
		rows=aggregate(vals, by=llply(by, function(x){return(inpDF[[by]])}), FUN=function(x) {
			ord=rle(match(x, values))$values
			return(names(x)[ord==cummax(ord)])
		})
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERRROR_TEMPLATE.SEQUENCE.NO_REPEATS=function(column_name, values, by) {
	return(ERROR_GENERATOR$new(id, "Invalid value in sequence (no repeats).", paste0("Cell values should be in the sequence:",phrase_FUN(values, "then")), function(inpDF) {
		vals=inpDF[[column_name]]
		names(vals)=seq_along(inpDF[[column_name]])
		rows=tapply(vals, INDEX=llply(by, function(x){return(inpDF[[by]])}), simplify=FALSE, FUN=function(x) {
			diffs=match(x, values) %>% diff
			return(names(x)[which(diffs!=1)+1])
		}) %>% unlist(recursive=FALSE, use.names=FALSE) %>% as.numeric()
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}


ERROR_TEMPLATE.TRUNCATED=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid truncated value.", paste0("Cell value should be:",phrase_FUN(values),"."), function(inpDF) {
		rows=which(!inpDF[[column_name]] %in% values)
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.PERCENT=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid percent value.", "Cell value should be between 0-1.", function(inpDF) {
		rows=which(inpDF[[column_name]]<0 || inpDF[[column_name]]>1)
		rows[is.na(rows)]=TRUE
		rows=which(rows)		
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.POISSON=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid Poisson value.", "Cell value should be integer and not less than 0.", function(inpDF) {
		rows=which(inpDF[[column_name]]<0 || round(inpDF[[column_name]])!=inpDF[[column_name]])
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.NORMAL=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid normal value.", "Cell value should not be a missing value.", function(inpDF) {
		rows=which(is.na(inpDF[[column_name]]))
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.OUTLIER=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Potential outlier value.", "Cell value might be an outlier.", function(inpDF) {
		rows=which(2<abs((inpDF[[column_name]]-mean(inpDF[[column_name]],na.rm=TRUE))/sd(x,na.rm=TRUE)))
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}


error_list_to_matrix=function(errorLST) {
	return(laply(errorLST, function(x) {
		return(c(x$.row, x$.col, x$color(), x$.status))
	}))
}