shinyUI(fluidPage(
	tags$head(
		tags$link(rel="stylesheet", type="text/css", href="deps/list/style.css"),
		tags$link(rel="stylesheet", type="text/css", href="https://netdna.bootstrapcdn.com/font-awesome/4.0.3/css/font-awesome.css"),
		tags$link(rel="stylesheet", type="text/css", href="deps/datatable/DataTables-1.10.5/media/css/jquery.dataTables.css"),
		tags$style("

			.list-container {
				float:left;
				width: 100%;
				cursor: pointer;
				margin: 2%;

				border-style: solid;
				border-width: 0px;
				-moz-border-radius: 15px;
				-webkit-border-radius: 15px;
				border-radius: 15px;
			}

			.status-ignored {
				background-color: #f0ad4e;
			}

			.status-error {
				background-color: #d9534f;
			}

			.status-fixed {
				background-color: #5cb85c;
			}
					
			.list-element-label {
				display:inline-block;
				float:left;
				color: #FFFFFF;
				margin-left: 10%;
				margin-top: 4%;
				margin-bottom: 2%;				
			}

			.list-element-zoom {
				display:inline-block;
				float:right;
				margin-right: 10%;
				margin-top: 2%;
				margin-bottom: 2%;
			}
			
			.list-element-swapignore {
				display:inline-block;
				float:right;
				margin-right: 3%;
				margin-top: 2%;
				margin-bottom: 2%;
			}
		
		")
	),
	tags$body(
		sidebarLayout(
			sidebarPanel(
				h4("Data Error Checker"),
				div(style="visibility:hidden",h3(textOutput('sidebartype'))),
				conditionalPanel(
					condition="output.sidebartype == 'load_data_panel'",
					div(
						selectInput("week_number_CHR", "Week Number:", choices=week_numbers_VCHR, selected="week 1"),
						selectInput("project_name_CHR", "Project:", choices=project_names_VCHR, selected="mangrove herbivory"),
						selectInput("group_color_CHR", "Group Colour:", choices=group_colors_VCHR, selected="blue"),
						selectInput("group_names_VCHR", "Group Name:", choices=c(""), multiple=TRUE, selectize=TRUE),
						br(),
						bsButton("load_data_BTN", "Load Data", style="primary")
					)
				),
				conditionalPanel(
					condition="output.sidebartype == 'error_list_panel'",
					div(
						ListHtmlRepr("list_widget"),
						br(),
						bsButton("scan_data_BTN", icon("search"), style="primary"),
						bsButton("submit_data_BTN", icon("cloud-upload"), style="primary")
					)
				)
			),
			mainPanel(
				conditionalPanel(
					condition="output.sidebartype == 'load_data_panel'",
					div(
						br()
					)
				),
				conditionalPanel(
					condition="output.sidebartype == 'error_list_panel'",
					div(
						DataTableHtmlRepr("dt_widget")
					)
				)
			)
		)
	),
	tags$foot(
		tags$script(src="https://rubaxa.github.io/Sortable/Sortable.js"),
		tags$script(src="deps/datatable/DataTables-1.10.5/media/js/jquery.js"),
		tags$script(src="deps/datatable/DataTables-1.10.5/media/js/jquery.dataTables.js"),
		tags$script(src="deps/list/bindings.js"),
		tags$script(src="deps/datatable/bindings.js"),
		tags$script(
			HTML('
				Shiny.addCustomMessageHandler("setWidgetProperty",
					function(message) {
						$("#"+message.id).prop(message.prop, message.status);
					}
				);

				Shiny.addCustomMessageHandler("colorCells",
					function(message) {
						console.log(message.row);
						console.log(message.col);
						console.log(message.color);
					}
				);

				Shiny.addCustomMessageHandler("evalText",
					function(message) {
						console.log("evaluating: "+message.text);
						eval(message.text);
						console.log("sidebar = "+sidebartype);
					}
				);
				
				function swapIgnoreItem(id) {
					Shiny.onInputChange("swapIgnoreItem", {
						"id":id,
						".nonce":Math.random()
					})
				}

				function zoomItem(id) {
					Shiny.onInputChange("zoomItem", {
						"id":id,
						".nonce":Math.random()
					})
				}
				
			')
		)
	)
))