(function() {
	// global vars
	var datatables={};
	
	// create Shiny output binding
	var datatableOutputBinding=new Shiny.OutputBinding();
	$.extend(datatableOutputBinding, {
		find: function(scope) {
			return $(scope).find(".datatable-widget-output");
		},
		renderValue: function(el, data) {
			// initialise
			var $el = $(el);
			var tmp = $el.data('datatable-widget-output');
			if (!tmp) {
				// initialise new datatable
				var datatable = {};
				datatable.id=el.id;
				var datatableOptions=JSON.parse(
					$el.children('script.datatable-options').text()
				);
				
				datatable.table=document.getElementById(datatable.id);
				datatable.head=document.createElement("thead");
				datatable.body=document.createElement("tbody");
				datatable.foot=document.createElement("tfoot");
				
				datatable.table.innerHTML=datatable.head;
				datatable.table.innerHTML+=datatable.body;
				datatable.table.innerHTML+=datatable.foot;
				
				// initialise fields
				datatables[datatable.id]=datatable;
				
				console.log('id');
				console.log(datatable.id);

				console.log('table');
				console.log(datatable.table);

				console.log('head');
				console.log(datatable.head);

				console.log('body');
				console.log(datatable.body);

				console.log('foot');
				console.log(datatable.foot);
				
			}
		}
	});
	Shiny.outputBindings.register(datatableOutputBinding, "datatable-output-binding");
	
	Shiny.addCustomMessageHandler('datatable', function(data) {
		var datatable=datatables[data.Id];
		if (!datatable)
			return;
		if (methods[data.method]) {
			methods[data.method].apply(datatable, data.args);
		} else {
			throw new Error("Unknown method " + data.method);
		}
	});
	
	var methods = {};
	
	// define methods
	methods.render=function(data) {
		// reset
		this.head.innerHTML='';
		this.foot.innerHTML='';
		this.body.innerHTML='';
		
		// load data
		var tmp_html;
		var tmp2_html;
		for (i in data) {
			// init
			tmp_html=document.createElement("tr");
			tmp_html2=document.createElement("th");
			tmp_html2.innerHTML=i;
			tmp_html.innerHTML+=tmp2_html;
			// head
			this.head.innerHTML+=tmp_html;
			// foot
			this.foot.innerHTML+=tmp_html;
		}
		for (j=0; j<data[Object.keys(data)[0]].length; j++) {
			// init
			tmp_html=document.createElement("tr");
			// add row
			for (i in data) {
				tmp2_html=document.createElement("td");
				tmp2_html.innerHTML=data[i][j];
				tmp_html.innerHTML+=tmp2_html;
			}
			this.body.innerHTML+=tmp_html;
		}
		
		// initialise datatable
		console.log('start init datable jquery');
		$(this.id).dataable();
		console.log('end init datable jquery');
	};

	methods.filter=function(row) {
		console.log(2);
	

	
	};

	
	methods.highlight=function(row, col, color) {
		console.log(3);
	
	
	
	};
		
	methods.update=function(row, col, value) {
		console.log(4);
	
	

	};
	



	
})();
	
	




