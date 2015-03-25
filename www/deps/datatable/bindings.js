(function() {
	// global vars
	var datatables={};
	var ids=[];
	
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
				ids.push(datatable.id);
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
		colnames[0]='';
		
		// set datatable filters
		this.activeRows=[];
		for (var i=0; i<data[Object.keys(data)[0]].length; i++) {
			this.activeRows.push(i);
		}
				
		$.fn.dataTableExt.afnFiltering.push(
		function (oSettings, aData, iDataIndex) {
// 			console.log('id = ' + ids[0]+'; iDataIndex = '+iDataIndex+'; activeRows[0] = '+datatables[ids[0]].activeRows+'; '+$.inArray(iDataIndex,datatables[ids[0]].activeRows)>-1);
			return $.inArray(iDataIndex,datatables[ids[0]].activeRows) > -1;
		});
		
		// set highlight fields
		this.highlightRow=[];
		this.highlightCol=[];
		this.highlightColor=[];

		// initialise datatable
		var currId=this.id;
		var currDataTable = $('#'+this.id).dataTable({
			"data": dataSet,
			"columns": colnames,
			"sDom": 'r<"H"lf><"datatable-scroll"t><"F"ip>',
			"columnDefs": [{
				"searchable": false,
				"orderable": false,
				"targets": 0
			}],
			"order": [[ 1, 'asc' ]],
			"fnRowCallback": function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
				// get columns numbers for items
				console.log('highlightRow = '+datatables[ids[0]].highlightRow);
				var idxs=[];
				var idx=datatables[ids[0]].highlightRow.indexOf(aData[0]-1);
				while (idx>-1) {
					console.log('pushing idx '+idx);
					idxs.push(idx);
					idx=datatables[ids[0]].highlightRow.indexOf(aData[0]-1, idx+1);
				}
				
				// add color classes
				var secondaryColor;
				if (idxs.length>0) {
					
					console.log('row = '+aData[0]);
					console.log('idx = '+idxs);
					
					// remove all colors from cells
					$(nRow).removeClass('status-error-secondary status-ignored-secondary status-fixed-secondary');
					
					// create subset arrays
					var curr_highlightCol=[];
					var curr_highlightColor=[];
					for (var i=0; i<idxs.length; ++i) {
						curr_highlightCol.push(datatables[ids[0]].highlightCol[idxs[i]]);
						curr_highlightColor.push(datatables[ids[0]].highlightColor[idxs[i]]);
					}
					console.log('curr_highlightCol = '+curr_highlightCol);
					console.log('curr_highlightColor = '+curr_highlightColor);
					
					// apply secondary colors to cells in row without items
					var secondaryColor;
					if (curr_highlightColor.indexOf('status-error')>-1) {
						secondaryColor='status-error-secondary';
					} else if (curr_highlightColor.indexOf('status-ignored')>-1) {
						secondaryColor='status-ignored-secondary';
					} else if (curr_highlightColor.indexOf('status-fixed')>-1) {
						secondaryColor='status-fixed-secondary';
					} else {
						console.log('unknown status = '+curr_highlightColor);
					}
					console.log('secondaryColor = '+secondaryColor);
					$(nRow).addClass(secondaryColor);
					
					// apply primary colors to cell in row with items
					$(nRow).children().each(function (index, td) {
						var j=curr_highlightCol.indexOf(index);
						if (j>-1) {
							console.log('primary color index = '+j+'; column = '+index);
							$(this).removeClass('status-error-primary status-error-secondary status-ignored-primary status-ignored-secondary status-fixed-primary status-fixed-secondary');
							$(this).addClass(curr_highlightColor[j]+'-primary');
						} else {
							console.log('secondary color index = '+j+'; column = '+index);
							if (!(
								$(this).hasClass('status-error-primary') ||
								$(this).hasClass('status-error-primary') ||
								$(this).hasClass('status-fixed-primary')
							)) {
								$(this).removeClass('status-error-secondary status-ignored-secondary status-fixed-secondary');
								$(this).addClass(secondaryColor);
							}
						}
					});
				}
			}
		}).makeEditable({
			sUpdateURL: function(value, settings) {
				Shiny.onInputChange(currId + '_update', {
					row: currDataTable.fnGetPosition(this)[0]+1,
					col: currDataTable.fnGetPosition(this)[2]+1,
					value: value,
					'.nonce': Math.random() // Force reactivity
				});
				return(value);
			}
		});
 		this.jtable=currDataTable;
	};

	methods.filter=function(row) {
		// convert row to array if integer
		if (typeof(row) === 'number')
			row=[row];
		// substract one from array to acheive base-0 indexing
		for (var i=0; i<row.length; ++i) {
			row[i]=row[i]-1;
		}
		// set activeRows as new row
		this.activeRows=row;
		// force table to redraw
		this.jtable.fnDraw();
	};
	
	methods.highlight=function(row, col, color) {
		// set array with highlighting info
		if (typeof(row) === 'number') {
			row=[row];
			col=[col];
			color=[color];
		}
		for (var i=0; i<row.length; ++i) {
			row[i]=row[i]-1;
			col[i]=col[i]-1;
		}
		this.highlightRow=row;
		this.highlightCol=col;
		this.highlightColor=color;
		// force table to redraw
		this.jtable.fnDraw();
	};
		
	methods.update=function(row, col, value) {
		console.log(4);
	};
	
})();
	
	




