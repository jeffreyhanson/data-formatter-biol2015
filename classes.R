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

DATA_PREP=setRefClass("DATA_PREP",
	fields=list(
		.column_classes_LST="list",
		.format_FUN="function",
		.errors_LST="list",
		.process_FUN="function"
	),
	methods=list(
		initialize=function(column_classes=list(), format=function(){return(NULL)}, errors=list(), process=function(){return(NULL)}) {
			.column_classes_LST<<-column_classes
			.format_FUN<<-format
			.errors_LST<<-errors
			.process_FUN<<-process
		},
		prepareData=function(inpDF) {
			return(.format_FUN(as.data.table(Map(as, inpDF[,names(.column_classes_LST),with=FALSE], .column_classes_LST))))
		},
		scanForErrors=function(inpDF) {
			return(llply(.errors_LST, function(x){
				return(x$testForErrors(inpDF))
			}))
		},
		processData=function(inpDF) {
			return(.process_FUN(inpDF))
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
		.activeViewData_DF="data.table",
		
		.errors="list"
	),
	methods=list(
		#### initialize methods
		initialize=function() {
			.activeView<<-"all"
			.activeWeekNumber_CHR<<-character(0)
			.activeProjectName_CHR<<-character(0)
			.activeGroupColor_CHR<<-character(0)
			.activeGroupNames_CHR<<-character(0)
			.fullProjectData_DF<<-data.table(0)
			.activeGroupData_DF<<-data.table(0)
			.activeViewData_DF<<-data.table(0)
		},
		#### field validating methods
		isDirFieldsValid=function() {
			return(!is.empty(.activeWeekNumber_CHR) & !is.empty(.activeProjectName_CHR) & !is.empty(.activeGroupColor_CHR))
		},
		isAllFieldsValid=function() {
			return(!is.empty(.activeWeekNumber_CHR) & !is.empty(.activeProjectName_CHR) & !is.empty(.activeGroupColor_CHR) & !is.empty(.activeGroupNames_CHR))
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
			return(unique(.fullProjectData_DF$Group))
		},
		setActiveWeekNumber_CHR=function(week_number) {
			.activeWeekNumber_CHR<<-week_number
		},		
		setActiveProjectName=function(project_name) {
			project_name=sub(" ", "_", project_name)
			.activeProjectName_CHR<<-project_name
			.dataPrep<<-get(project_name)
		},
		setActiveGroupColor=function(group_color) {
			.activeGroupColor_CHR<<-group_color
		},
		setActiveGroupNames=function(group_names) {
			.activeGroupNames_CHR<<-group_names
		},
		setActiveData=function() {
			.activeGroupData_DF<<-.fullProjectData_DF %>% filter(Group %in% .activeGroupNames_CHR)
		},
		
		#### error handling methods
		scanDataForErrors=function() {
			tempErrors=.dataPrep$scanForErrors(.activeGroupData_DF) %>% unlist(recursive=TRUE, use.names=FALSE)
			if (length(tempErrors)>0) {
				.errors[laply(tempErrors, function(x){return(x$.id)})]<<-tempErrors
			}
		},
		scanCellForErrors=function(row, col) {
			# init
			retLST=list()
			# get all errrors in column
			currColErrors=llply(.dataPrep$.errors_LST, function(x) {
				if (x$.column==names(.activeGroupData_DF)[col]) {
					cat('testing for',x$.name,'errors in',x$.column,'\n')
					return(x$testForErrors(.activeGroupData_DF))
				} else {
					return(NULL)
				}
			}) %>% unlist(recursive=FALSE, use.names=FALSE)
			print("currColErrors")
			str(currColErrors)
			# extract existing errors in cell
			cellErrors=.errors[
				laply(.errors, function(x) {return(x$.row)})==row &
				laply(.errors, function(x) {return(x$.col)})==col
			]
			## check to see get errors that have been updated
			# get fixed errors
			fixedErrors=cellErrors[laply(cellErrors, function(x) {
				return(
					all(
						laply(currColErrors, function(z){return(z$.row)})!=x$.row &
						laply(currColErrors, function(z){return(z$.col)})!=x$.col &
						laply(currColErrors, function(z){return(z$.name)})!=x$.name
					)
				)
			})]
			for (i in seq_along(fixedErrors))
				.errors[fixedErrors[[i]]$.id]$.status<<-'fixed'
			# errors that were fixed but are now errors again
			unfixedErrors=cellErrors[laply(cellErrors, function(x) {
				return(
					x$.status=="fixed" & any(
						laply(currColErrors, function(z){return(z$.row)})==x$.row &
						laply(currColErrors, function(z){return(z$.col)})==x$.col &
						laply(currColErrors, function(z){return(z$.name)})==x$.name
					)
				)
			})]
			retLST$updatedErrors=append(fixedErrors, unfixedErrors)
			# check to see if new errors
			if (length(cellErrors)==0) {
				retLST$newErrors=currColErrors
			} else {
				retLST$newErrors=currColErrors[
					laply(currColErrors, function(x) {
						laply(cellErrors, function(z){return(z$.row)})!=x$.name
					})
				]
			}
			if (length(retLST$newErrors)>0)
				.errors<<-append(.errors,retLST$newErrors)
			# return error objects
			return(retLST)
		},
		setErrorStatus=function(id, status) {
			.errors[[id]]$.status<<-status
		},


		#### render data table methods
		getFullProjectData=function() {
			return(list(data=.fullProjectData_DF, row=numeric(0), col=numeric(0)))
		},
		getActiveGroupsData=function(status="all") {
			.activeView<<-status
			if (status=='all') {
				ind=seq_along(.errors)
				.activeViewData_DF<<-.activeGroupData_DF
			} else {
				ind=which(laply(.errors, "[[", ".status")==status)
				.activeViewData_DF<<-.activeGroupData_DF[laply(.errors[ind], "[[", ".row"),]
			}
			return(list(
				data=data.frame(.activeViewData_DF),
				row=laply(.errors[ind], "[[", ".row"),
				col=laply(.errors[ind], "[[", ".col"),
				color=laply(.errors[ind], function(x) x$color())
			))
		},
		getDataWithSpecificError=function(id) {
			.activeView<<-'all'
			.activeViewData_DF<<-.activeGroupData_DF[.errors[[id]]$.row,]
			return(list(
				data=data.frame(.activeViewData_DF),
				row=.errors[[id]]$.row,
				col=.errors[[id]]$.col,
				color=.errors[[id]]$color()
			))
		}
	)
)

ERROR=setRefClass("ERROR",
	fields=list(
		.id="character",
		.status="character",
		.row="integer",
		.col="integer",
		.name="character",
		.description="character",
		.test="function"
	),
	methods=list(
		initialize=function(id, name, i, j, description, test) {
			.id<<-id
			.name<<-name
			.status<<-"error"
			.row<<-i
			.col<<-j
			.description<<-description
			.test<<-test
		},
		test=function(inpDF) {
			if(nrow(.test(inpDF))==0) {
				.status<<-"fixed"
			} else {
				.status<<-"error"
			}
		},
		isValid=function() {
			return(.status=="fixed")
		},
		swapIgnore=function() {
			if (.status=='error') {
				.status<<-'ignored'
			} else if (.status=='ignored') {
				.status<<-'error'
			}
		},
		repr=function() {
			return(paste0('
			<div class="list-container status-',.status,'">
				<div class="row">
					<h5 class="list-element-label">(',.id,')  ',.name,'</h5>
					<button class="btn btn-default action-button list-element-zoom" id="',.id,'_zoom_btn" name="',.id,'" type="button" onclick="zoomItem(this.name)">
						<i class="fa fa-search-plus"></i>
					</button>
					<button class="btn btn-default action-button list-element-swapignore" id="',.id,'_swap_btn" name="',.id,'" type="button" onclick="swapIgnoreItem(this.name)">
						<i class="',ifelse(.status=='ignored' || .status=='fixed','fa fa-check-square-o','fa fa-square-o'),'"></i>
					</button>
				</div>
			</div>
			'))
		},
		color=function() {
			return(c(ignored=ignoreColor, fixed=fixedColor, error=errorColor)[.status])
		}
	)
)

ERROR_GENERATOR=setRefClass("ERROR_GENERATOR",
	fields=list(
		.id="ID",
		.name="character",
		.description="character",
		.column="character",
		.test="function"
	),
	methods=list(
		initialize=function(id, name, column, description, test) {
			.id<<-id
			.name<<-name
			.column<<-column
			.description<<-description
			.test<<-test
		},
		testForErrors=function(inpDF) {
			return(alply(.test(inpDF),1, function(x) {
				return(ERROR$new(.id$new(), .name, x[[1]], x[[2]], .description, .test))
			}))
		}
	)
)

ERROR_TEMPLATE.FACTOR=function(column_name, values) {
	return( ERROR_GENERATOR$new(id, "Invalid factor value.", column_name, paste0("Cell value should be: ",phrase_FUN(values),"."), function(inpDF) {
		rows=which(!inpDF[[column_name]] %in% values)
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.SEQUENCE.REPEATS=function(column_name, values, by) {
	return(ERROR_GENERATOR$new(id, "Invalid value in sequence (repeats).", column_name, paste0("Cell value should be in the sequence: ",phrase_FUN(values, "then"),"."), function(inpDF) {
		vals=inpDF[[column_name]]
		names(vals)=seq_along(inpDF[[column_name]])
		rows=tapply(vals, INDEX=llply(by, function(x){return(inpDF[[x]])}), simplify=FALSE, FUN=function(x) {
			ord=rle(match(x, values))$values
			return(names(x)[ord==cummax(ord)])
		})$x %>% unlist(recursive=TRUE, use.names=FALSE) %>% as.numeric()
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.SEQUENCE.NO_REPEATS=function(column_name, values, by) {
	return(ERROR_GENERATOR$new(id, "Invalid value in sequence (no repeats).", column_name, paste0("Cell value should be in the sequence: ",phrase_FUN(values, "then"),"."), function(inpDF) {
		vals=inpDF[[column_name]]
		names(vals)=seq_along(inpDF[[column_name]])
		rows=tapply(vals, INDEX=llply(by, function(x){return(inpDF[[x]])}), simplify=FALSE, FUN=function(x) {
			diffs=match(x, values) %>% diff
			return(names(x)[which(diffs!=1)+1])
		})$x %>% unlist(recursive=TRUE, use.names=FALSE) %>% as.numeric()
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}


ERROR_TEMPLATE.TRUNCATED=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid truncated value", column_name, paste0("Cell value should be greater than 0"), function(inpDF) {
		rows=which(inpDF[[column_name]] < 0)
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.PERCENT=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid percent value", column_name, "Cell value should be between 0-1", function(inpDF) {
		rows=inpDF[[column_name]]<0 || inpDF[[column_name]]>1
		rows[is.na(rows)]=TRUE
		rows=which(rows)
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.POISSON=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid Poisson value", column_name, "Cell value should be integer and not less than 0", function(inpDF) {
		rows=which(inpDF[[column_name]]<0 || round(inpDF[[column_name]])!=inpDF[[column_name]])
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.NORMAL=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid normal value", column_name, "Cell value should not be a missing value", function(inpDF) {
		rows=which(is.na(inpDF[[column_name]]))
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.OUTLIER=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Potential outlier value", column_name, "Cell value might be an outlier", function(inpDF) {
		rows=which(is.outlier(inpDF[[column_name]]))
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}


error_list_to_matrix=function(errorLST) {
	return(laply(errorLST, function(x) {
		return(c(x$.row, x$.col, x$color(), x$.status))
	}))
}
