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
		.omittedRows_BOL="logical",
		
		.fullProjectData_DF="data.table",
		.activeGroupData_DF="data.table",
		.dataPrep="DATA_PREP",
		.activeView_CHR="character",
		.activeViewData_DF="data.table",
		
		.errors_LST="list"
		
	),
	methods=list(
		#### initialize methods
		initialize=function() {
			.activeView_CHR<<-"all"
			.activeWeekNumber_CHR<<-character(0)
			.activeProjectName_CHR<<-character(0)
			.activeGroupColor_CHR<<-character(0)
			.activeGroupNames_CHR<<-character(0)
			.fullProjectData_DF<<-data.table(0)
			.activeGroupData_DF<<-data.table(0)
			.activeViewData_DF<<-data.table(0)
			.omittedRows_BOL<<-logical(0)
		},
		#### field validating methods
		isDirFieldsValid=function() {
			return(!is.empty(.activeWeekNumber_CHR) & !is.empty(.activeProjectName_CHR) & !is.empty(.activeGroupColor_CHR))
		},
		isRawDataAvailable=function() {
			currFILES=dir(file.path("raw", .activeProjectName_CHR, .activeWeekNumber_CHR, .activeGroupColor_CHR))
			if (length(currFILES)>0) {
				if (grepl('^.*.csv$', currFILES, ignore.case=TRUE)) {
					return(TRUE)
				}
			}
			return(FALSE)
		},
		isAllFieldsValid=function() {
			return(!is.empty(.activeWeekNumber_CHR) & !is.empty(.activeProjectName_CHR) & !is.empty(.activeGroupColor_CHR) & !is.empty(.activeGroupNames_CHR))
		},
		
		#### disk interface methods
		loadProjectDataFromFile=function() {
			currFILES=dir(file.path("raw", .activeProjectName_CHR, .activeWeekNumber_CHR, .activeGroupColor_CHR), '^.*.csv', ignore.case=TRUE, full.names=TRUE)
			if (length(currFILES)>0) {
				.dataPrep<<-get(.activeProjectName_CHR)
				.fullProjectData_DF<<-.dataPrep$prepareData(rbind.fill(llply(currFILES,fread)))
				return(TRUE)
			} else {
				.fullProjectData_DF<<-data.table(0)
				return(FALSE)
			}
		},
		saveDataToFile=function() {
			# init
			currFileName=paste0(paste(.activeGroupNames_CHR, collapse='_'), '.csv')
			# save cleaned data
			write.table(
				.activeGroupData_DF,
				file.path("cleaned", .activeProjectName_CHR, .activeWeekNumber_CHR, .activeGroupColor_CHR, currFileName),
				sep=",",
				row.names=FALSE
			)
			# save formatted data for group
			write.table(
				.dataPrep$processData(.activeGroupData_DF, week=.activeWeekNumber_CHR, omitRows=.omittedRows_BOL),
				file.path("formatted", .activeProjectName_CHR, .activeWeekNumber_CHR, .activeGroupColor_CHR, currFileName),
				sep=",",
				row.names=FALSE
			)
			# save compiled data for group
			write.table(
				rbind.fill(
					sapply(
						dir(file.path("formatted", .activeProjectName_CHR, .activeWeekNumber_CHR, .activeGroupColor_CHR), '^.*.csv$', ignore.case=TRUE, full.names=TRUE),
						fread
					)
				), 
				file.path("compiled", activeProjectName_CHR, .activeWeekNumber_CHR, paste0('group_', .activeGroupColor_CHR, "_data.csv")),
				sep=",",
				row.names=FALSE
			)
			# save master data set
			write.table(
				rbind.fill(
					sapply(
						dir(file.path("compiled", .activeProjectName_CHR, '^.*.csv$', ignore.case=TRUE, full.names=TRUE, recursive=TRUE)),
						fread
					)
				), 
				file.path("master", paste0("masterdata_",format(System.time(), '_%Y-%m-%d-%H-%M-%S'),".csv")),
				sep=",",
				row.names=FALSE
			)
		},
		
		### data manipulation methods
		getProjectGroupNames=function() {
			return(unique(.fullProjectData_DF$Group))
		},
		setActiveWeekNumber_CHR=function(week_number) {
			.activeWeekNumber_CHR<<-week_number
		},		
		setActiveProjectName=function(project_name) {
			.activeProjectName_CHR<<-sub(" ", "_", project_name)
		},
		setActiveGroupColor=function(group_color) {
			.activeGroupColor_CHR<<-group_color
		},
		setActiveGroupNames=function(group_names) {
			.activeGroupNames_CHR<<-group_names
		},
		setActiveData=function() {
			.dataPrep<<-get(.activeProjectName_CHR)
			.activeGroupData_DF<<-.fullProjectData_DF %>% filter(Group %in% .activeGroupNames_CHR)
			.activeGroupData_DF$DeleteButton<<-paste0('
				<button class="btn btn-default action-button btn-xs" id="delteBtn_', seq_len(nrow(.activeGroupData_DF)), '" name="', seq_len(nrow(.activeGroupData_DF)), '" type="button" onclick="swapOmission(name)" action="show">
					<i class="fa fa-minus-circle" style="color:red"></i>
				</button>')
			.activeGroupData_DF$Row<<-seq_len(nrow(.activeGroupData_DF))
			.activeGroupData_DF<<-.activeGroupData_DF[,c(ncol(.activeGroupData_DF)-1, ncol(.activeGroupData_DF), seq_len(ncol(.activeGroupData_DF)-2)),with=FALSE]
			.omittedRows_BOL<<-rep(FALSE, nrow(.activeGroupData_DF))
		},
		swapOmission=function(row) {
			# init
			ret_LST=list(updatedErrors=list() ,newErrors=list())
			if (!.omittedRows_BOL[row]) {
				# actions to omit row
				.omittedRows_BOL[row]<<-TRUE
				# set all issues in row to be ignored
				for (i in seq_along(.errors_LST)) {
					if (.errors_LST[[i]]$.row_INT==row) {
						.errors_LST[[i]]$setStatus('ignored')
						ret_LST$updatedErrors=append(ret_LST$updatedErrors, .errors_LST[[i]])
					}
				}
			} else {
				# actions to de-omit row
				.omittedRows_BOL[row]<<-FALSE
				# check all cells in row for issues
				for (i in seq_len(ncol(.activeGroupData_DF))[c(-1,-2)]) {
					currErrors_LST=scanCellForErrors(row, i, includeIgnored=TRUE)
					ret_LST$updatedErrors=append(ret_LST$updatedErrors, currErrors_LST$updatedErrors)
					ret_LST$newErrors=append(ret_LST$newErrors, currErrors_LST$newErrors)
				
				}
			}
			# post
			return(ret_LST)
		},
		
		#### error handling methods
		scanDataForErrors=function() {
			tempErrors=.dataPrep$scanForErrors(.activeGroupData_DF) %>% unlist(recursive=TRUE, use.names=FALSE)
			if (length(tempErrors)>0) {
				.errors_LST[laply(tempErrors, function(x){return(x$.id_CHR)})]<<-tempErrors
			}
		},
		scanCellForErrors=function(row, col, includeIgnored=FALSE) {
			# init
			retLST=list()
			
			# get all errrors in column
			currColErrors=llply(.dataPrep$.errors_LST, function(x) {
				if (x$.column_CHR==names(.activeGroupData_DF)[col]) {
					return(x$testForErrors(.activeGroupData_DF))
				} else {
					return(NULL)
				}
			}) %>% unlist(recursive=FALSE, use.names=FALSE)
			
			# extract existing errors in cell
			cellErrors=.errors_LST[
				laply(.errors_LST, function(x) {return(x$.col_INT)})==col
			]
			
			## check to see get errors that have been updated
			# get fixed errors
			fixedErrors=cellErrors[laply(cellErrors, function(x) {
				return(
					all(
						!laply(currColErrors, function(z){return(z$.row_INT)})==x$.row_INT &
						laply(currColErrors, function(z){return(z$.name_CHR)})==x$.name_CHR
					)
				)
			})]
			for (i in seq_along(fixedErrors))
				.errors_LST[[fixedErrors[[i]]$.id_CHR]]$setStatus('fixed')
			
			# errors that were fixed but are now errors again
			unfixedErrors=cellErrors[laply(cellErrors, function(x) {
				return(
					(
						x$.status_CHR=="fixed" ||
						(x$.status_CHR=="ignored" & includeIgnored)
						
						
					) & any(
						laply(currColErrors, function(z){return(z$.row_INT)})==x$.row_INT &
						laply(currColErrors, function(z){return(z$.name_CHR)})==x$.name_CHR
					)
				)
			})]
			for (i in seq_along(unfixedErrors))
				.errors_LST[[unfixedErrors[[i]]$.id_CHR]]$setStatus('error')
			
			# store updates on existing errors
			retLST$updatedErrors=append(fixedErrors, unfixedErrors)
			
			# check to see if new errors
			if (length(cellErrors)==0) {
				retLST$newErrors=currColErrors
			} else {
				retLST$newErrors=currColErrors[
					laply(currColErrors, function(x) {
						all(
							laply(cellErrors, function(z){return(z$.row_INT)})!=x$.row_INT &
							laply(cellErrors, function(z){return(z$.col_INT)})!=x$.col_INT &
							laply(cellErrors, function(z){return(z$.name_CHR)})!=x$.name_CHR
						)
					})
				]
			}
			
			# store new errors
			if (length(retLST$newErrors)>0) {
				names(retLST$newErrors)=laply(retLST$newErrors, function(z){return(z$.id_CHR)})
				.errors_LST<<-append(.errors_LST,retLST$newErrors)
			}
			
			# return error objects
			return(retLST)
		},
		setErrorStatus=function(id, status) {
			.errors_LST[[id]]$.status_CHR<<-status
		},
		#### render data table methods
		getFullProjectData=function() {
			return(list(data=.fullProjectData_DF, row=numeric(0), col=numeric(0)))
		},
		getActiveGroupsData=function(status=.activeView_CHR) {
			.activeView_CHR<<-status
			if (status=='all') {
				ind=seq_along(.errors_LST)
				.activeViewData_DF<<-.activeGroupData_DF
				return(list(
					data=data.frame(.activeViewData_DF),
					row=seq_len(nrow(.activeViewData_DF)),
					highlight_row=unname(laply(.errors_LST[ind], "[[", ".row_INT")),
					highlight_col=unname(laply(.errors_LST[ind], "[[", ".col_INT")),
					highlight_color=unname(laply(.errors_LST[ind], function(x) x$color()))
				))

			} else {
				ind=which(laply(.errors_LST, "[[", ".status_CHR")==status)
				rows=laply(.errors_LST[ind], "[[", ".row_INT")
				ind2=which(laply(.errors_LST, "[[", ".row_INT") %in% rows)
				.activeViewData_DF<<-.activeGroupData_DF[rows,]
				return(list(
					data=.activeViewData_DF,
					row=unname(laply(.errors_LST[ind2], "[[", ".row_INT")),
					highlight_row=unname(laply(.errors_LST[ind2], "[[", ".row_INT")),
					highlight_col=unname(laply(.errors_LST[ind2], "[[", ".col_INT")),
					highlight_color=unname(laply(.errors_LST[ind2], function(x) x$color()))
				))
			}
		},
		getDataWithSpecificError=function(id) {
			.activeView_CHR<<-'all'
			.activeViewData_DF<<-.activeGroupData_DF[.errors_LST[[id]]$.row_INT,]
			return(list(
				row=.errors_LST[[id]]$.row_INT,
				highlight_row=.errors_LST[[id]]$.row_INT,
				highlight_col=.errors_LST[[id]]$.col_INT,
				highlight_color=.errors_LST[[id]]$color()
			))
		}
	)
)

ERROR=setRefClass("ERROR",
	fields=list(
		.id_CHR="character",
		.status_CHR="character",
		.row_INT="integer",
		.col_INT="integer",
		.col_CHR="character",
		.name_CHR="character",
		.description_CHR="character",
		.test_FUN="function"
	),
	methods=list(
		initialize=function(id, name, i, j, k, description, test) {
			.id_CHR<<-id
			.name_CHR<<-name
			.status_CHR<<-"error"
			.row_INT<<-i
			.col_INT<<-j
			.col_CHR<<-k
			.description_CHR<<-description
			.test_FUN<<-test
		},
		test=function(inpDF) {
			if(nrow(.test_FUN(inpDF))==0) {
				.status_CHR<<-"fixed"
			} else {
				.status_CHR<<-"error"
			}
		},
		key=function() {
# 			return(.row_INT)
			return(1/(as.numeric(.row_INT)+(as.numeric(.col_INT)*0.1)))
		},
		isValid=function() {
			return(.status_CHR=="fixed")
		},
		setStatus=function(x) {
			stopifnot(x %in% c('error','fixed','ignored'))
			.status_CHR<<-x
		},
		swapIgnore=function() {
			if (.status_CHR=='error') {
				.status_CHR<<-'ignored'
			} else if (.status_CHR=='ignored') {
				.status_CHR<<-'error'
			}
		},
		repr=function() {
			return(paste0('
			<div class="list-container status-',.status_CHR,'-primary" title="',.description_CHR,'\nRow = ',.row_INT,', Column = ',.col_CHR,'.">
				<div class="row">
					<h5 class="list-element-label">(',.id_CHR,')  ',.name_CHR,'</h5>
					<button class="btn btn-default action-button list-element-zoom" id="',.id_CHR,'_zoom_btn" name="',.id_CHR,'" type="button" onclick="zoomItem(this.name)">
						<i class="fa fa-search-plus"></i>
					</button>
					<button class="btn btn-default action-button list-element-swapignore" id="',.id_CHR,'_swap_btn" name="',.id_CHR,'" type="button" onclick="swapIgnoreItem(this.name)">
						<i class="',ifelse(.status_CHR=='ignored' || .status_CHR=='fixed','fa fa-check-square-o','fa fa-square-o'),'"></i>
					</button>
				</div>
			</div>
			'))
		},
		color=function() {
			return(paste0('status-',.status_CHR))
		}
	)
)

ERROR_GENERATOR=setRefClass("ERROR_GENERATOR",
	fields=list(
		.id_CHR="ID",
		.name_CHR="character",
		.description_CHR="character",
		.column_CHR="character",
		.test_FUN="function"
	),
	methods=list(
		initialize=function(id, name, column, description, test) {
			.id_CHR<<-id
			.name_CHR<<-name
			.column_CHR<<-column
			.description_CHR<<-description
			.test_FUN<<-test
		},
		testForErrors=function(inpDF) {
			return(alply(.test_FUN(inpDF),1, function(x) {
				return(ERROR$new(.id_CHR$new(), .name_CHR, x[[1]], x[[2]], names(inpDF)[x[[2]]], .description_CHR, .test_FUN))
			}))
		}
	)
)

ERROR_TEMPLATE.FACTOR=function(column_name, values) {
	return( ERROR_GENERATOR$new(id, "Invalid factor value", column_name, paste0("Cell value should be: ",phrase_FUN(values),"."), function(inpDF) {
		rows=which(!inpDF[[column_name]] %in% values)
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.SEQUENCE.REPEATS=function(column_name, values, by) {
	return(ERROR_GENERATOR$new(id, "Invalid value in sequence", column_name, paste0("Cell value should be in the sequence: ",phrase_FUN(values, "then"),".\nThe same value can be repeated multiple times in a row."), function(inpDF) {
		vals=inpDF[[column_name]]
		names(vals)=seq_along(inpDF[[column_name]])
		rows=tapply(vals, INDEX=llply(by, function(x){return(inpDF[[x]])}), simplify=FALSE, FUN=function(x) {
			rows=names(x)[!x %in% values]
			ind=which(x %in% values)
			if (length(ind) > 0) {
				ids=match(x[ind], values)
				ind2=ids[which(
					!(
						ids[1:(length(ids)-1)] == ids[2:length(ids)]-1 |
						ids[1:(length(ids)-1)] == ids[2:length(ids)] |
						(
							ids[1:(length(ids)-1)]==max(ids) &
							ids[2:length(ids)]==min(ids)
						)
					)
				)]
				rows=c(rows, names(x[ind[ind2]]))
			}
			return(rows)
		}) %>% unlist(recursive=TRUE, use.names=FALSE) %>% as.integer()
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.SEQUENCE.NO_REPEATS=function(column_name, values, by) {
	return(ERROR_GENERATOR$new(id, "Invalid value in sequence", column_name, paste0("Cell value should be in the sequence: ",phrase_FUN(values, "then"),".\nThe same value cannot be repeated multiple times in a row."), function(inpDF) {
		vals=inpDF[[column_name]]
		names(vals)=seq_along(inpDF[[column_name]])
		rows=tapply(vals, INDEX=llply(by, function(x){return(inpDF[[x]])}), simplify=FALSE, FUN=function(x) {
			rows=names(x)[!x %in% values]
			ind=which(x %in% values)
			if (length(ind) > 0) {
				ids=match(x[ind], values)
				ind2=ids[which(
					!(
						ids[1:(length(ids)-1)] == ids[2:length(ids)]-1 |
						(
							ids[1:(length(ids)-1)]==max(ids) &
							ids[2:length(ids)]==min(ids)
						)
					)
				)]
				rows=c(rows, names(x[ind[ind2]]))
			}
			return(rows)
		}) %>% unlist(recursive=TRUE, use.names=FALSE) %>% as.integer()
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}


ERROR_TEMPLATE.TRUNCATED=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid truncated value", column_name, "Cell value should be greater than 0.\nChange the value in this cell.", function(inpDF) {
		rows=which(inpDF[[column_name]] < 0)
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.PERCENT=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid percent value", column_name, "Cell value should be between 0-1.\nChange the value in this cell.", function(inpDF) {
		rows=which(is.na(inpDF[[column_name]]) || inpDF[[column_name]]<0 || inpDF[[column_name]]>1)
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.POISSON=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid Poisson value", column_name, "Cell value should be integer and not less than 0.\nChange the value in this cell.", function(inpDF) {
		rows=which(is.na(inpDF[[column_name]]) || inpDF[[column_name]]<0 || round(inpDF[[column_name]])!=inpDF[[column_name]])
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.NORMAL=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid normal value", column_name, "Cell value should not be a missing value.\nChange the value in this cell.", function(inpDF) {
		rows=which(is.na(inpDF[[column_name]]))
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.OUTLIER=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Potential outlier value", column_name, "Cell value might be an outlier.\nCheck to see if it's an error and if so change it.", function(inpDF) {
		rows=which(is.outlier(inpDF[[column_name]]))
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

ERROR_TEMPLATE.BINARY=function(column_name) {
	return(ERROR_GENERATOR$new(id, "Invalid binary value", column_name, "Cell value should be either 0 or 1.\nChange the value in this celll.", function(inpDF) {
		rows=which(!inpDF[[column_name]] %in% c(0,1))
		return(data.frame(row=rows, col=rep(match(column_name, names(inpDF)), length(rows))))
	}))
}

error_list_to_matrix=function(errorLST) {
	return(laply(errorLST, function(x) {
		return(c(x$.row_INT, x$.col_INT, x$color(), x$.status_CHR))
	}))
}
