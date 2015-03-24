shinyUI(fluidPage(
	tags$head(
		tags$link(rel="stylesheet", type="text/css", href="deps/list/style.css"),
		tags$link(rel="stylesheet", type="text/css", href="css/font-awesome.min.css"),
		tags$link(rel="stylesheet", type="text/css", href="deps/datatable/DataTables-1.10.5/media/css/jquery.dataTables.css"),
		tags$link(rel="stylesheet", type="text/css", href="deps/datatable/DataTables-1.10.5/extensions/FixedHeader/css/dataTables.fixedHeader.min.css"),
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
			
			.datatable-scroll {
				overflow-x: auto;
				overflow-y: visible;
				float: left;
				width: 100%;
				position: relative;
			}
			
			.status-ignored {
				background-color: #F0AD4E !important;
			}
			
			.status-ignored-secondary {
				background-color: #FFD8A0 !important;
			}
			
			.status-error {
				background-color: #D9534F !important;
			}
			
			.status-error-secondary {
				background-color: #FFADAB !important;
			}
			
			.status-fixed {
				background-color: #5CB85C !important;
			}
			
			.status-fixed-secondary {
				background-color: #BAE8BA !important;
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
			
			.center1 {
				margin-left: auto;
				margin-right: auto;
				width: 80%;
			}

			.center2 {
				margin-left: auto;
				margin-right: auto;
				width: 20%;
			}
						
			
			h2 {
				text-transform: uppercase;
				text-shadow: 1px 1px 0 #FFFFFF, 2px 2px 0 #000000;
				text-align:center;
				display: inline-block;
			}
			
			.center {
				width: 100%;
				text-align:center;
			}
		
		")
	),
	tags$body(
		sidebarLayout(
			sidebarPanel(
				div(class='center', tags$h2(class='header', "Data Error Checker")),
				div(style="visibility:hidden",h3(textOutput('sidebartype'))),
				conditionalPanel(
					condition="output.sidebartype == 'load_data_panel'",
					div(	
						selectInput("week_number_CHR", "Week Number:", choices=week_numbers_VCHR, selected="week 1"),
						selectInput("project_name_CHR", "Project:", choices=project_names_VCHR, selected="mangrove herbivory"),
						selectInput("group_color_CHR", "Group Colour:", choices=group_colors_VCHR, selected="blue"),
						selectInput("group_names_VCHR", "Group Name:", choices=c(""), multiple=TRUE, selectize=TRUE),
						br(),
						bsActionButton("load_data_BTN", "Load Data", style="primary")
					)
				),
				conditionalPanel(
					condition="output.sidebartype == 'error_list_panel'",
					div(
						ListHtmlRepr("list_widget"),
						br(),
						div(
							class='center',
							tags$button(
								id="scan_data_BTN",
								type="button",
								style="primary",
								class="btn btn-primary sbs-action-button",
								tags$i(class="fa fa-search"),
								title="Rescan data for errors."
							),
							tags$button(
								id="submit_data_BTN",
								type="button",
								style="primary",
								class="btn btn-primary sbs-action-button",
								tags$i(class="fa fa-cloud-upload"),
								title="Submit data to the cloud?"
							)
						)

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
		tags$script(src="deps/datatable/DataTables-1.10.5/media/js/jquery.js"),
		tags$script(src="deps/datatable/DataTables-1.10.5/media/js/jquery.dataTables.min.js"),
		tags$script(src="deps/datatable/DataTables-1.10.5/extensions/TableTools/js/dataTables.tableTools.min.js"),
		tags$script(src="deps/datatable/jquery-Datatables-editable-2.3.3/jquery.dataTables.editable.js"),
		tags$script(src="deps/datatable/DataTables-1.10.5/extensions/FixedHeader/js/dataTables.fixedHeader.min.js"	),
		tags$script(src="deps/datatable/jquery_jeditable-1.7.3/jquery.jeditable.js"),
		tags$script(src="deps/list/Sortable.min.js"),
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
				
				//tooltip
				$(function() {
					var tooltips = $("[title]").tooltip();
					$(document)(function() {
					tooltips.tooltip( "open" );
					});
				});

			')
		)
	)
))