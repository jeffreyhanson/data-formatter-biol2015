createToc<-function(session, outputId) {
	session$output[[outputId]]=renderText("")

	send <- function(method, func, msg) {
		msg <- msg[names(formals(func))]
		names(msg) <- NULL
		origDigits <- getOption('digits')
		options(digits=22)
		on.exit(options(digits=origDigits))
		session$sendCustomMessage('toc', list(
		  Id = outputId,
		  method = method,
		  args = msg
	))}	

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
	stub(setView(status)),
	stub(addItem()),
	stub(removeItem())
  ), class = "toc")

}

toc=function(outputId, options=NULL) {
	if (is.numeric(width))
		width = sprintf("%dpx", width)
	if (is.numeric(height))
		height = sprintf("%dpx", height)
	tagList(
		singleton(
			tags$head(
				tags$link(rel="stylesheet", type="text/css", href="dependencies/toc/toc.css")
				tags$script(src="dependencies/shiny_interface/binding.js")
				tags$script(src="dependencies/toc/binding.js")
			),
			tags$div(id=outputId, class="toc",
				tags$div(id=paste0(outputId,"_btns_div"), class="toc-btns-div",
					HTML('
						<div id="viewBtnsGroup" class="btn-group sbs-button-group" data-toggle="buttons-radio">
							<button id="btn1" type="button" class="btn sbs-btn active bs-active" data-value="all">All</button>
							<button id="btn2" type="button" class="btn sbs-btn" data-value="fixed">Fixed</button>
							<button id="btn3" type="button" class="btn sbs-btn" data-value="ignored">Ignored</button>
							<button id="btn4" type="button" class="btn sbs-btn" data-value="errors">Errors</button>
						</div> 
					')
				),
				tags$div(id=paste0(outputId,"_item_list"), class="toc-item-list")
			),
			tags$script(type="application/json", class="toc-options",ifelse(is.null(options), "{}", RJSONIO::toJSON(options)))
		)
	)
}

