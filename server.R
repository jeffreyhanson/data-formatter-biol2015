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
				print(1)
				manager$setActiveData()
				# scan data for verrors
				print(2)
				manager$scanDataForErrors()
				# add errors to widget
				print(3)
				for (i in seq_along(manager$.errors_LST)) {
					listwidget$addItem(manager$.errors_LST[[i]]$.id_CHR, manager$.errors_LST[[i]]$repr(), manager$.errors_LST[[i]]$.status_CHR, FALSE)
				}
				listwidget$reloadView()
				# show data
				print(4)
				tmp=manager$getActiveGroupsData()
				print(4.1)
				output$data=renderDataTable({tmp$data})
				print(4.2)
				session$sendCustomMessage("colorCells",list(row=tmp$row,col=tmp$col,color=tmp$color))
				print(4.3)
				output$mainpanelUI=renderUI({dataUI})
				print(5)
				# change sidebar
				output$sidebartype=renderText({'error_list_panel'})
				print(6)
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
			print(11)
			tmp=manager$getActiveGroupsData(input$listStatus$view)
			print(12)
			output$data=renderDataTable({tmp$data})
			print(13)
			session$sendCustomMessage("colorCells",list(row=tmp$row,col=tmp$col,color=tmp$color))
			print(14)
			listwidget$setView(input$listStatus$view,TRUE)
			print(15)
		})
	})
	
	## swap ignore status
	observe({
		if (is.null(input$swapIgnoreItem))
			return()
		isolate({
			if (manager$.errors_LST[[input$swapIgnoreItem$id]]$.status_CHR=='error' || manager$.errors_LST[[input$swapIgnoreItem$id]]$.status_CHR=='ignored') {
				manager$.errors_LST[[input$swapIgnoreItem$id]]$swapIgnore()
				listwidget$updateItem(input$swapIgnoreItem$id, manager$.errors_LST[[input$swapIgnoreItem$id]]$repr(), manager$.errors_LST[[input$swapIgnoreItem$id]]$.status_CHR, TRUE)
				if (manager$.activeView_CHR=="ignored" || manager$.activeView_CHR=="error")
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

