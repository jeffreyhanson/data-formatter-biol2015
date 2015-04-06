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
				
				// initialise fields
				datatables[datatable.id]=datatable;
				
				console.log('id');
				console.log(datatable.id);

				console.log('table');
				console.log(datatable.table);
				
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
		// prepare datatable objects
		// cell values
		var dataSet=[];
		var tmp=[];
		for (i=0; i<data[Object.keys(data)[0]].length; i++) {
			// init
			tmp=[];
			// add column data
			for (j in data)
				tmp.push(data[j][i]);
			// store array
			dataSet.push(tmp);
		}
		// column names
		var colnames=[];
		for (j in data) {
			colnames.push({'title': j.replace(/\./g, ' ')});
		}
		
		// initialise datatable
		var currId=this.id;
		var currDataTable = $('#'+this.id).dataTable({
			"data": dataSet,
			"columns": colnames
		}).makeEditable({
			sUpdateURL: function(value, settings) {
				Shiny.onInputChange(currId + '_update', {
					row: currDataTable.fnGetPosition(this)[0],
					col: currDataTable.fnGetPosition(this)[2]+1,
					value: value,
					'.nonce': Math.random() // Force reactivity
				});
				return(value);
			}
		});
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
	
	




