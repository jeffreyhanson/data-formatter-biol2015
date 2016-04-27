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
		this.omitRows=[];
		for (var i=0; i<data[Object.keys(data)[0]].length; i++) {
			this.activeRows.push(i);
			this.omitRows.push(false);
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
		this.jtable = $('#'+this.id).dataTable({
			"data": dataSet,
			"columns": colnames,
// 			"sDom": 'r<"H"lf><"datatable-scroll"t><"F"ip>', // horizontal scroll bar
			"columnDefs": [{
				"searchable": false,
				"orderable": false,
				"targets": [0,1]
			}],
			"bAutoWidth": false, 
			"fnRowCallback": function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
				if (!datatables[ids[0]].omitRows[aData[1]]) {
					//// if row is not set to be omitted
					// get columns numbers for items
					var idxs=[];
					var idx=datatables[ids[0]].highlightRow.indexOf(aData[1]-1);
					while (idx>-1) {
						idxs.push(idx);
						idx=datatables[ids[0]].highlightRow.indexOf(aData[1]-1, idx+1);
					}
					
					// add color classes
					var secondaryColor;
					if (idxs.length>0) {
						
						// remove all colors from cells
						$(nRow).removeClass('status-error-secondary status-ignored-secondary status-fixed-secondary status-omit read_only');
						
						// create subset arrays
						var curr_highlightCol=[];
						var curr_highlightColor=[];
						for (var i=0; i<idxs.length; ++i) {
							curr_highlightCol.push(datatables[ids[0]].highlightCol[idxs[i]]);
							curr_highlightColor.push(datatables[ids[0]].highlightColor[idxs[i]]);
						}
						
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
						$(nRow).addClass(secondaryColor);
						
						// apply primary colors to cell in row with items
						$(nRow).children().each(function (index, td) {
							var j=curr_highlightCol.indexOf(index);
							if (j>-1) {
								$(this).removeClass('status-error-primary status-error-secondary status-ignored-primary status-ignored-secondary status-fixed-primary status-fixed-secondary status-omit read_only');
								$(this).addClass(curr_highlightColor[j]+'-primary');
							} else {
								if (!(
									$(this).hasClass('status-error-primary') ||
									$(this).hasClass('status-ignored-primary') ||
									$(this).hasClass('status-fixed-primary')
								)) {
									$(this).removeClass('status-error-secondary status-ignored-secondary status-fixed-secondary status-omit read_only');
									$(this).addClass(secondaryColor);
								}
							}
						});
					} else {
						$(nRow).removeClass('status-error-primary status-error-secondary status-ignored-primary status-ignored-secondary status-fixed-primary status-fixed-secondary status-omit read_only');
						$(nRow).children().each(function (index, td) {
							$(this).removeClass('status-error-primary status-error-secondary status-ignored-primary status-ignored-secondary status-fixed-primary status-fixed-secondary status-omit read_only');
						});
					}
				} else {
					//// if row is set to be omitted
					$(nRow).removeClass('status-error-primary status-error-secondary status-ignored-primary status-ignored-secondary status-fixed-primary status-fixed-secondary status-omit read_only');
					$(nRow).addClass('status-omit read_only');

					$(nRow).children().each(function (index, td) {
						$(this).removeClass('status-error-primary status-error-secondary status-ignored-primary status-ignored-secondary status-fixed-primary status-fixed-secondary status-omit read_only');
						$(this).addClass('status-omit read_only');
					});
				}
			}
		}).makeEditable({
			sReadOnlyCellClass: "read_only",
			fnOnEditing: function(jInput, oEditableSettings, sOriginalText, id) {
				if (jInput.parents('tr').hasClass('read_only') || (jInput.parents('td').index()<2)) {
					jInput["0"].value=sOriginalText;
				}
				return(true);
			},
			fnShowError: function(errorText, action) {},
			sUpdateURL: function(value, settings) {
				Shiny.onInputChange(currId + '_update', {
					row: datatables[ids[0]].jtable.fnGetPosition(this)[0]+1,
					col: datatables[ids[0]].jtable.fnGetPosition(this)[2]+1,
					value: value,
					'.nonce': Math.random() // Force reactivity
				});
				return(value);
			}
		}).stickyTableHeaders();
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
	
	methods.omitRow=function(row, status) {
		// set omit row
		this.omitRows[row]=status;
		// redraw table
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
	
	




