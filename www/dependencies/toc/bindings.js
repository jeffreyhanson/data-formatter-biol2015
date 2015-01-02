(function() {
	// global vars
	var tocs={};
	
	// create Shiny output binding
	var leafletOutputBinding=new Shiny.OutputBinding();
	$.extend(leafletOutputBinding, {
		find: function(scope) {
			return $(scope).find(".toc-div-output");
		},
		renderValue: function(el, data) {
			var $el = $(el);
			var toc = $el.data('toc-div');
			if (!toc) {
				// initialise new toc
				var id = this.getId(el);
				var tocOptions=JSON.parse(
					$el.children('script.toc-options').text()
				);
				toc.id=id;
				tocs[id]=toc;
				toc.item_list=document.getElementById(id+"_item_list");
				toc.sortable_list=Sortable.create(toc.item_list);
			
				// initialise fields
				toc.status="all";
				toc.items={};
				
				// tell shiny to start adding items 
				Shiny.onInputChange(id+'_load_data'. {
					'.nonce':Math.random()
				});
			}
		}
	});
	Shiny.outputBindings.register(tocOutputBinding, "toc-output-binding");
	
	Shiny.addCustomMessageHandler("toc", function(data) {
		var tocId=data.tocId;
		var toc=tocs[tocId];
		if (!toc)
			return;
		if (methods[data.method]) {
			methods[data.method].apply(toc, data.args);
		} else {
			throw new Error("Unknown method " + data.method);
		}
	});
	
	var methods = {};
	
	// define methods
	methods.reloadView=function() {
		// remove all items from div
		this.item_list.innerHTML='';
		// add items to div
		if (this.status!="all") {
			for (i=0; i<this.items.length; i++) {
				if (this.items[i].status==this.status) {
					this.item_list.appendChild(this.items[i].html);
				}
			}
		} else {
			for (i=0; i<this.items.length; i++) {
				this.item_list.appendChild(this.items[i].html);
			}
		}
	};
	
	methods.setView=function(status, force_reset=true) {
		// change status
		this.status=status;
		// refresh div
		if (force_reset && this.status!=status)
			this.methods.reloadView();
	};
		
	methods.addItem=function(id, item, status, force_reset=true) {
		// create item object
		this.items[id]={
			id: id,
			item: item,
			status: status,
			html: '<div id="'+id+'_item" class="'+status+'-item">'+item+'</div>'
		};
		// refresh div
		if (force_reset && this.items[id].status==this.status) {
			this.methods.reloadView();
		}
	};
	
	methods.removeItem=function(id, force_reset=true) {
		// remove item object
		if (this.items[id])
			delete this.items[id];
		// refresh div
		if (force_reset && this.items[id].status==this.status) {
			this.methods.reloadView(this.status);
		}
	};
	
	methods.updateItem(id, item=null, status=null, force_reset=true) {
		if (this.items[id]) {
			// update item
			var old_status=this.items[id].status
			if (item!=null) {
				this.items[id].item=item;
			}
			if (status!=null) {
				this.items[id].status=status;
			}
			this.items[id].html='<div id="'+id+'_item" class="'+status+'-item">'+item+'</div>';
			// update rendered object
			if (force_reset && (this.items[id].status==this.status || old_status==this.status)) {
				this.methods.reloadView(this.status);
			}
		} else {
			console.log("item with id "+id+" does not exist.");
		}
	};
	
	methods.clearItems=function(force_reset=true) {
		// remove all items
		this.items.clear();
		// update div
		if (force_reset) {
			this.div.innerHTML="";
		}
	};
	
}
	
	




