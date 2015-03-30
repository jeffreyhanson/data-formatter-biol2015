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
				for (i in seq_along(manager$.errors_LST)) {
					listwidget$addItem(manager$.errors_LST[[i]]$.id_CHR, manager$.errors_LST[[i]]$repr(), manager$.errors_LST[[i]]$.status_CHR, manager$.errors_LST[[i]]$key(), FALSE)
				}
				listwidget$reloadView()
				# show data
				tmp=manager$getActiveGroupsData()
				dtwidget$render(tmp$data)
				dtwidget$highlight(row=tmp$highlight_row,col=tmp$highlight_col,color=tmp$highlight_color)
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
			dtwidget$highlight(row=tmp$highlight_row,col=tmp$highlight_col,color=tmp$highlight_color)
			# filter list widget
			listwidget$filterItems(input$zoomItem$id, TRUE)
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
			dtwidget$highlight(row=tmp$highlight_row,col=tmp$highlight_col,color=tmp$highlight_color)
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
			manager$.activeViewData_DF[[input$dt_widget_update$col]][match(input$dt_widget_update$row,manager$.activeViewData_DF$Row)]<<-as(input$dt_widget_update$value, class(manager$.activeViewData_DF[[input$dt_widget_update$col]]))
			manager$.activeGroupData_DF[[input$dt_widget_update$col]][input$dt_widget_update$row]<<-as(input$dt_widget_update$value, class(manager$.activeViewData_DF[[input$dt_widget_update$col]]))
			
			# rescan for errors
			retErrors=manager$scanCellForErrors(input$dt_widget_update$row,input$dt_widget_update$col)
			
			# update widgets with updated errors
			for (i in seq_along(retErrors$updatedErrors))
				listwidget$updateItem(retErrors$updatedErrors[[i]]$.id_CHR, retErrors$updatedErrors[[i]]$repr(), retErrors$updatedErrors[[i]]$.status_CHR, retErrors$updatedErrors[[i]]$key(), FALSE)

			# update widgets with new errors
			for (i in seq_along(retErrors$newErrors))
				listwidget$addItem(retErrors$newErrors[[i]]$.id_CHR, retErrors$newErrors[[i]]$repr(), retErrors$newErrors[[i]]$.status_CHR, retErrors$newErrors[[i]]$key(), FALSE)
			
					
			# highlight cells
			tmp=manager$getActiveGroupsData()			
			dtwidget$highlight(row=tmp$highlight_row,col=tmp$highlight_col,color=tmp$highlight_color)
			
			# reload list widget
			listwidget$reloadView()
			
		})
	})
	
	## swap ignore status
	observe({
		if (is.null(input$swapIgnoreItem))
			return()
		isolate({
			# update list widget
			manager$.errors_LST[[input$swapIgnoreItem$id]]$swapIgnore()
			listwidget$updateItem(input$swapIgnoreItem$id, manager$.errors_LST[[input$swapIgnoreItem$id]]$repr(), manager$.errors_LST[[input$swapIgnoreItem$id]]$.status_CHR, manager$.errors_LST[[input$swapIgnoreItem$id]]$key(), TRUE)
			# update datatable widget
			tmp=manager$getActiveGroupsData()
			dtwidget$filter(tmp$row)
			dtwidget$highlight(row=tmp$highlight_row,col=tmp$highlight_col,color=tmp$highlight_color)
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

