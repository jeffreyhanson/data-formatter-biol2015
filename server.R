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
	listwidget=createList(session, "list_widget")
	dtwidget=createDataTable(session, "dt_widget")

	# set initial values
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
				dtwidget$render(tmp$data)
				for (i in seq_along(tmp$row))
					dtwidget$highlight(row=tmp$row[i],col=tmp$col[i],color=tmp$color[i])
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
			# init
			tmp=manager$getDataWithSpecificError(input$zoomItem$id)
			# data table widget
			dtwidget$filter(tmp$row)
			dtwidget$highlight(row=tmp$row,col=tmp$col,color=tmp$color)
		})
	})
	
	## set view
	observe({
		if (is.null(input$listStatus))
			return()
		isolate({
			# init
			tmp=manager$getActiveGroupsData(input$listStatus$view)
			# data table widget
			dtwidget$filter(tmp$row)
				dtwidget$highlight(row=tmp$row[i],col=tmp$col[i],color=tmp$color[i])
			# list widget
			listwidget$setView(input$listStatus$view,TRUE)
		})
	})
	
	## update cell value
	observe({
		if(is.null(input$dt_widget_update))
			return()
		isolate({
			# update value
			str(input$dt_widget_update)
			manager$.activeViewData_DF[[input$dt_widget_update$col]][input$dt_widget_update$row]<<-as(input$dt_widget_update$value, class(manager$.activeViewData_DF[[input$dt_widget_update$col]]))
			manager$.activeGroupData_DF[[input$dt_widget_update$col]][as.numeric(rownames(manager$.activeViewData_DF)[input$dt_widget_update$row])]<<-as(input$dt_widget_update$value, class(manager$.activeViewData_DF[[input$dt_widget_update$col]]))
			print(summary(manager$.activeGroupData_DF))
			# rescan for errors
			retErrors=manager$scanCellForErrors(as.numeric(rownames(manager$.activeViewData_DF))[input$dt_widget_update$row],input$dt_widget_update$col)
			# update list widget with existing errors that have changed statuses
			print('retErrors$updatedErrors')
			str(retErrors$updatedErrors)
			for (i in seq_along(retErrors$updatedErrors)) {
				listwidget$updateItem(retErrors$updatedErrors[[i]]$.id, retErrors$updatedErrors[[i]]$repr(), retErrors$updatedErrors[[i]]$.status, FALSE)
			}
			# update list widget with new errors
			print('retErrors$newErrors')
			str(retErrors$newErrors)
			for (i in seq_along(retErrors$newErrors)) {
				listwidget$addItem(retErrors$newErrors[[i]]$.id, retErrors$newErrors[[i]]$repr(), retErrors$newErrors[[i]]$.status, FALSE)
			}
			# reload list widget
			listwidget$reloadView()				
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

