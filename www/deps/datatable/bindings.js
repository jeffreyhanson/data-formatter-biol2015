(function() {
	// global vars
	var datatables={};
	
	// create Shiny output binding
	var datatableOutputBinding=new Shiny.OutputBinding();
	$.extend(listOutputBinding, {
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
				var listOptions=JSON.parse(
					$el.children('script.datatable-options').text()
				);
				datatable.head=document.getElementById(datatable.id+"_head");
				datatable.foot=document.getElementById(datatable.id+"_foot");
				datatable.body=document.getElementById(datatable.id+"_body");
				// initialise fields
				datatables[datatable.id]=datatable;
			}
		}
	});
	Shiny.outputBindings.register(listOutputBinding, "datatable-output-binding");
	
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
	methods.renderDataTable=function(data) {
		console.log(1);
	

	
	};
	
	methods.highlightCell=function(row, col, color) {
		console.log(2);
	
	
	
	};
		
	methods.updateCell=function(row, col, value) {
		console.log(3);
	
	

	};
	



	
})();
	
	




