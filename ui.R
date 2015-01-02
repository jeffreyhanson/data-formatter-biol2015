shinyUI(fluidPage(
	tags$head(
		tags$script(
			HTML('
				Shiny.addCustomMessageHandler("setWidgetProperty",
					function(id, prop, status) {
						$(#id).prop(prop, status);
					}
				);

				Shiny.addCustomMessageHandler("colorCells",
					function(row, col, color) {
						
						
						
					}
				);
				
				function zoomItem(id) {
					Shiny.onInputChange("zoomItem", {
						"id":id,
						".nonce":Math.random()
					})
				}

				function swapIgnoreItem(id) {
					Shiny.onInputChange("swapIgnoreItem", {
						"id":id,
						".nonce":Math.random()
					})
				}
				
			')
		)
	),

	sidebarLayout(
		titlePanel("Data Error Checker & Formatter"),
		sidebarPanel(
			selectInput("week_number_CHR", "Week Number:", choices=week_numbers_VINT, selectize=FALSE),
			selectInput("project_name_CHR", "Project:", choices=project_names_VCHR, selectize=FALSE),
			selectInput("group_color_CHR", "Group Colour:", choices=group_colors_VCHR, selectize=FALSE)
			selectInput("group_name_VCHR", "Group Name:", choices=c(""), mutliple=TRUE, selectize=TRUE)
			bsActionButton("load_data_BTN", "Load Data:", style="primary")
			br(),
			conditionalPanel(condition="is_data_loaded",
				toc("toc"),
				bsActionButton("scan_data_BTN", icon("search"), style="primary"),
				bsActionButton("submit_data_BTN", icon("cloud-upload"), style="primary")
			)
		),
		mainPanel(
			conditionalPanel(condition="is_data_loaded",
				dataTableOutput("data")
			)
		)
	)
))