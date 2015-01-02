shinyServer(function(input,output,server) {
	#### initialise
	# set working directory
	init_dir_FUN()
	manager=MANAGER$new()
	toc=createToc(session, "toc")

	# set initial values
	output$is_data_loaded=FALSE
	updateSelectInput("week_number_CHR", choices=week_numbers_VCHR)
	updateSelectInput("project_name_CHR", choices=project_names_VCHR)
	updateSelectInput("group_color_CHR", choices=group_colours_VCHR)
	session$sendCustomMessage("setWidgetProperty",list(id="group_name_VCHR",prop="disabled", status=TRUE))
	
	#### reactive handlers
	## set manager fields
	observe({
		if(is.empty(input$week_number_CHR))
			return()
		isolate({
			manager$setActiveWeekNumber_CHR(input$week_number_CHR)
			output$is_data_loaded=FALSE
			if (manager$isDirFieldsValid()) {
				manager$loadProjectDataFromFile()
				updateSelectInput("group_name_VCHR", choices=manager$getProjectGroupNames())
				session$sendCustomMessage("setWidgetProperty",list(id="group_name_VCHR",prop="disabled", status=FALSE))
			} else {
				updateSelectInput("group_name_VCHR", choices=c(""))
				session$sendCustomMessage("setWidgetProperty",list(id="group_name_VCHR",prop="disabled", status=TRUE))
			}
		})
	})
	observe({
		if(is.empty(input$project_name_CHR))
			return()			
		isolate({
			manager$setActiveProjectName(input$project_name_CHR)
			output$is_data_loaded=FALSE
			if (manager$isDirFieldsValid()) {
				manager$loadProjectDataFromFile()
				updateSelectInput("group_name_VCHR", choices=manager$getProjectGroupNames())
				session$sendCustomMessage("setWidgetProperty",list(id="group_name_VCHR",prop="disabled", status=FALSE))
			} else {
				updateSelectInput("group_name_VCHR", choices=c(""))
				session$sendCustomMessage("setWidgetProperty",list(id="group_name_VCHR",prop="disabled", status=TRUE))
			}
		})
	})
	observe({
		if(is.empty(input$group_color_CHR))
			return()
		isolate({
			manager$setActiveGroupColor(input$group_color_CHR)
			output$is_data_loaded=FALSE
			if (manager$isDirFieldsValid()) {
				manager$loadProjectDataFromFile()
				updateSelectInput("group_name_VCHR", choices=manager$getProjectGroupNames())
				session$sendCustomMessage("setWidgetProperty",list(id="group_name_VCHR",prop="disabled", status=FALSE))
			} else {
				updateSelectInput("group_name_VCHR", choices=c(""))
				session$sendCustomMessage("setWidgetProperty",list(id="group_name_VCHR",prop="disabled", status=TRUE))
			}
		})
	})
	
	## load data
	observe({
		if(is.empty(input$group_names_VCHR))
			return()
		isolate({
			manager$setActiveGroupNames(input$group_names_VCHR)
			output$is_data_loaded=FALSE
			if (manager$isAllFieldsValid()) {
				output$is_data_loaded=TRUE
				# set active data
				manager$setActiveData()
				# scan data for errors
				manager$scanDataForErrors()
				# add errors to widget
				for (i in seq_along(manager$.errors)) {
					toc$addItem(manager$.errors[[i]]$.id, manager$.errors[[i]]$repr(), manager$.errors[[i]]$.status, FALSE)
				}
				toc$reloadView()
			}
		})
	})
		
	## zoom item
	observe({
		if(is.null(toc$zoomItem))
			return()
		isolate({
			tmp=toc$getRowWithSpecificError(toc$zoomItem$id)
			session$sendCustomMessage("highlightRow",list(row=tmp$row,color=tmp$color))
		})
	})
	
	## set view
	observe({
		if(is.null(toc$viewBtnsGroup))
			return()
		isolate({
			switch(viewBtnsGroup,
				"all"={tmp=toc$getActiveGroupsData()},
				"fixed"={tmp=toc$getAllDataWithFixedErrorsInActiveGroups()},
				"ignored"={tmp=toc$getAllDataWithIgnoredErrorsInActiveGroups()},
				"errors"={tmp=toc$getAllDataWithErrorsInActiveGroups()}
			)
			output$renderDataTable({tmp$data})
			session$sendCustomMessage("colorCells",list(row=tmp$row,col=tmp$col,color=tmp$color))
		})
	})
	
	## swap ignore status
	observe({
		if(is.null(toc$swapIgnoreItem))
			return()
		isolate({
			manager$.errors[[toc$swapIgnoreItem$id]].swapIgnore()
			toc$updateItem(toc$swapIgnoreItem$id, item=manager$.errors[[toc$swapIgnoreItem$id]].repr())
		})
	})

	## save data
	observe({
		if(submit_data_BTN==0)
			return()
		isolate({
			manager$saveDataToFile()
		})
	})

})

