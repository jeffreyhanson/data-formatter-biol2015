createDataTable<-function(session, outputId) {
	session$output[[outputId]]=renderText("")
	send <- function(method, func, msg) {
		msg <- msg[names(formals(func))]
		names(msg) <- NULL
		origDigits <- getOption('digits')
		options(digits=22)
		on.exit(options(digits=origDigits))
		session$sendCustomMessage('list', list(
			Id = outputId,
			method = method,
			args = msg
		))		
	}
	baseimpl <- function() {
		send(`__name__`, sys.function(), as.list(environment()))
	}
	stub <- function(prototype) {
		# Get the un-evaluated expression
		p <- substitute(prototype)
		# The function name is the first element
		name <- as.character(p[[1]])
		# Get textual representation of the expression; change name to "function"
		# and add a NULL function body
		txt <- paste(deparse(p), collapse = "\n")
		txt <- sub(name, "function", txt, fixed = TRUE)
		txt <- paste0(txt, "NULL")
		# Create the function
		func <- eval(parse(text = txt))
		# Replace the function body, using baseimpl's body as a template
		body(func) <- substituteDirect(
			body(baseimpl),
			as.environment(list("__name__"=name))
		)
		environment(func) <- environment(baseimpl)
		# Return as list
		structure(list(func), names = name)
	}
  structure(c(
	stub(highlight(row, col, color)),
	stub(render(data)),
  ), class = "datatable_widget")
}

DataTableHtmlRepr=function(outputId, options=NULL) {
	return(
		tagList(
			singleton(
				tags$head(tags$script(
					HTML(
					'
						function setCellValue(row, col, value) {
							Shiny.onInputChange("cellValue", {
								"row":row,
								"col":col,
								"value":value,
								".nonce":Math.random()
							})
						}
					'
					)
				))
			),
			
			tags$div(id=outputId, class="row-border hover order-column", cellspacing="0", width="100%",
				tags$thead(paste0(outputId,'_head')),
				tags$tfoot(paste0(outputId,'_foot')),
				tags$tbody(paste0(outputId,'_body')),
				tags$script(
					type="application/json", class="list-options",
					ifelse(is.null(options), "{}", RJSONIO::toJSON(options))			
				)
			)
		)
	)
}

