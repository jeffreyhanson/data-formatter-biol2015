createList<-function(session, outputId) {
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
	stub(reloadView()),
	stub(setView(status,force_reset)),
	stub(addItem(id, item, status, force_reset)),
	stub(updateItem(id, item, status, force_reset)),
	stub(removeItem(id, force_reset))
  ), class = "list_widget")
}

ListHtmlRepr=function(outputId, options=NULL) {
	return(
		tagList(
			singleton(
				tags$head(tags$script(
					HTML(
					'
						function setListStatus(view) {
							Shiny.onInputChange("listStatus", {
								"view":view,
								".nonce":Math.random()
							})
						}					
					'
					)
				))
			),
			tags$div(id=outputId, class="list-widget-output",
				tags$div(id=paste0(outputId,"_btns_div"), class="list-btns-div center",
					HTML('
						<div class="btn-group" data-toggle="buttons">
							<label class="btn btn-default">
								<input id="status_all_BTN" type="radio" value="all" checked="checked" onchange=setListStatus(this.value) />All
							</label>
							<label class="btn btn-default">
								<input id="status_fixed_BTN" type="radio" value="fixed" onchange=setListStatus(this.value) />Fixed
							</label>
							<label class="btn btn-default">
								<input id="status_ignored_BTN" type="radio" value="ignored" onchange=setListStatus(this.value) />Ignored
							</label>
							<label class="btn btn-default">
								<input id="status_errors_BTN" type="radio" value="error" onchange=setListStatus(this.value) />Errors
							</label>
						</div> 
					')
				),
				tags$div(id=paste0(outputId,"_item_list"), class="list-item-list"),
				tags$script(
					type="application/json", class="list-options",
					ifelse(is.null(options), "{}", RJSONIO::toJSON(options))			
				)
			)
		)
	)
}

