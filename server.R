bindEvent <- function(eventExpr, callback, env=parent.frame(), quoted=FALSE) {
  eventFunc <- exprToFunction(eventExpr, env, quoted)
  initialized <- FALSE
  invisible(observe({
    eventVal <- eventFunc()
    if (!initialized)
      initialized <<- TRUE
    else
      isolate(callback())
  }))
}

shinyServer(function(input,output,session) {
	#### initialise
	# set working directory
	setwd(main_DIR)
	init_dir_FUN()
	manager=MANAGER$new()
	listwidget=createList(session, "widget")

	# set initial values
	blankPageUI=div()
	dataUI=div(dataTableOutput("data"))
	output$mainpanelUI=renderUI({blankPageUI})
	
	output$sidebartype=renderText({'load_data_panel'})
	session$sendCustomMessage('evalText', list(text="var sidebartype='load_data_panel';"))
	session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=TRUE))
	session$sendCustomMessage("setWidgetProperty",list(id="load_data_BTN",prop="disabled", status=TRUE))
	
	#### reactive handlers
	## set manager fields
	observe({
		if(is.empty(input$week_number_CHR))
			return()
		isolate({
			manager$setActiveWeekNumber_CHR(input$week_number_CHR)
			if (manager$isDirFieldsValid()) {
				manager$loadProjectDataFromFile()
				updateSelectInput("group_names_VCHR", choices=manager$getProjectGroupNames())
				session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=FALSE))
			} else {
				session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=TRUE))
			}
		})
	})
	observe({
		if(is.empty(input$project_name_CHR))
			return()			
		isolate({
			manager$setActiveProjectName(input$project_name_CHR)
			if (manager$isDirFieldsValid()) {
				manager$loadProjectDataFromFile()
				updateSelectInput("group_names_VCHR", choices=manager$getProjectGroupNames())
				session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=FALSE))
			} else {
				session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=TRUE))
			}
		})
	})
	observe({
		if(is.empty(input$group_color_CHR))
			return()
		isolate({
			manager$setActiveGroupColor(input$group_color_CHR)
			if (manager$isDirFieldsValid()) {
				manager$loadProjectDataFromFile()
				updateSelectInput(session,"group_names_VCHR", choices=manager$getProjectGroupNames())
				session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=FALSE))
			} else {
				session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=TRUE))
			}
		})
	})
	
	
	## make load button active/inactive
	observe({
		if(is.empty(input$group_names_VCHR)) {
			isolate({
				session$sendCustomMessage("setWidgetProperty",list(id="load_data_BTN",prop="disabled", status=TRUE))
			})
		}
	})
	observe({
		if(!is.empty(input$group_names_VCHR)) {
			isolate({
				session$sendCustomMessage("setWidgetProperty",list(id="load_data_BTN",prop="disabled", status=FALSE))
			})
		}
	})
			
	## load data
	observe({
		if(is.null(input$load_data_BTN) || input$load_data_BTN==0)
			return()
		isolate({
			manager$setActiveGroupNames(input$group_names_VCHR)
			if (manager$isAllFieldsValid()) {
				# set active data
				manager$setActiveData()
				# scan data for errors
				manager$scanDataForErrors()
				# add errors to widget
				for (i in seq_along(manager$.errors)) {
					listwidget$addItem(manager$.errors[[i]]$.id, manager$.errors[[i]]$repr(), manager$.errors[[i]]$.status, FALSE)
				}
				listwidget$reloadView()
				# show data
				tmp=manager$getActiveGroupsData()
				output$data=renderDataTable({tmp$data})
				session$sendCustomMessage("colorCells",list(row=tmp$row,col=tmp$col,color=tmp$color))
				output$mainpanelUI=renderUI({dataUI})
				# change sidebar
				output$sidebartype=renderText({'error_list_panel'})
			}
		})
		
	})
		
	## zoom item
	observe({
		if (is.null(input$zoomItem))
			return()
		isolate({
			tmp=manager$getDataWithSpecificError(input$zoomItem$id)
			output$data=renderDataTable({tmp$data})			
			session$sendCustomMessage("highlightRow",list(row=tmp$row,col=tmp$col,color=tmp$color))
		})
	})
	
	## set view
	observe({
		if (is.null(input$listStatus))
			return()
		isolate({
			tmp=manager$getActiveGroupsData(input$listStatus$view)
			output$data=renderDataTable({tmp$data})
			session$sendCustomMessage("colorCells",list(row=tmp$row,col=tmp$col,color=tmp$color))
			listwidget$setView(input$listStatus$view,TRUE)
		})
	})
	
	## swap ignore status
	observe({
		if (is.null(input$swapIgnoreItem))
			return()
		isolate({
			if (manager$.errors[[input$swapIgnoreItem$id]]$.status=='error' || manager$.errors[[input$swapIgnoreItem$id]]$.status=='ignored') {
				manager$.errors[[input$swapIgnoreItem$id]]$swapIgnore()
				listwidget$updateItem(input$swapIgnoreItem$id, manager$.errors[[input$swapIgnoreItem$id]]$repr(), manager$.errors[[input$swapIgnoreItem$id]]$.status, TRUE)
				if (manager$.activeView=="ignored" || manager$.activeView=="error")
					listwidget$reloadView()
			}
		})
	})

	## save data
	observe({
		if (is.null(input$submit_data_BTN) || input$submit_data_BTN==0)
			return()
		isolate({
			manager$saveDataToFile()
		})
	})
})

