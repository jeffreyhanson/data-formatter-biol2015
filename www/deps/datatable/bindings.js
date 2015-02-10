(function() {
	// global vars
	var lists={};
	
	// create Shiny output binding
	var listOutputBinding=new Shiny.OutputBinding();
	$.extend(listOutputBinding, {
		find: function(scope) {
			return $(scope).find(".list-widget-output");
		},
		renderValue: function(el, data) {
			// initialise
			var $el = $(el);
			var tmp = $el.data('list-widget-output');
			if (!tmp) {
				// initialise new list
				var list = {};
				list.id=el.id;
				var listOptions=JSON.parse(
					$el.children('script.list-options').text()
				);
				list.item_list=document.getElementById(list.id+"_item_list");
				list.sortable_list=Sortable.create(list.item_list, {});
				// initialise fields
				list.status="all";
				list.items=[];
				lists[list.id]=list;
			}
		}
	});
	Shiny.outputBindings.register(listOutputBinding, "list-output-binding");
	
	Shiny.addCustomMessageHandler('list', function(data) {
		var list=lists[data.Id];
		if (!list)
			return;
		if (methods[data.method]) {
			methods[data.method].apply(list, data.args);
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
			for (i in this.items) {
				if (this.items[i].status==this.status) {
					this.item_list.insertBefore(this.items[i].html, this.item_list.firstChild);
				}
			}
		} else {
			for (i in this.items) {
				this.item_list.insertBefore(this.items[i].html, this.item_list.firstChild);
			}
		}
	};
	
	methods.setView=function(status, force_reset) {
		// set defaults 
		force_reset = typeof force_reset !== 'undefined' ? force_reset : true;
		// change status
		this.status=status;
		// refresh div
		if (force_reset) {
			methods['reloadView'].apply(lists[this.id]);
		}
	};
		
	methods.addItem=function(id, item, status, force_reset) {
		// set defaults
		force_reset = typeof force_reset !== 'undefined' ? force_reset : true;	
		// create item object
		var item_html=document.createElement("div");
		item_html.setAttribute("class", 'class="'+status+'-item-'+item+'"');
		item_html.setAttribute("item-id", id);
		item_html.innerHTML=item;
		this.items[id]={
			id: id,
			status: status,
			html: item_html
		};
		// refresh div
		if (force_reset && (this.items[id].status==this.status || this.status=='all'))
			methods['reloadView'].apply(lists[this.id]);
	};
	
	methods.removeItem=function(id, force_reset) {
		// set defaults 
		force_reset = typeof force_reset !== 'undefined' ? force_reset : true;	
		// remove item object
		if (this.items[id])
			delete this.items[id];
		// refresh div
		if (force_reset && (this.items[id].status==this.status || this.status=='all'))
			methods['reloadView'].apply(lists[this.id]);
	};
	
	methods.updateItem=function(id, item, status, force_reset) {
		// set defaults 
		force_reset = typeof force_reset !== 'undefined' ? force_reset : true;	
		if (this.items[id]) {
			// update item
			var old_status=this.items[id].status
			if (typeof item !== 'undefined') {
				this.items[id].item=item;
			}
			if (typeof status !== 'undefined') {
				this.items[id].status=status;
			}
			// generate html repr
			var item_html=document.createElement("div");
			item_html.setAttribute("class", 'class="'+status+'-item-'+item+'"');
			item_html.setAttribute("item-id", id);
			item_html.innerHTML=item;
			this.items[id].html=item_html;
			// update rendered object
			if (force_reset && (this.items[id].status==this.status || this.status=='all')) {
				methods['reloadView'].apply(lists[this.id]);
			}
		} else {
			console.log("error: item with id "+id+" does not exist.");
		}
	};
	
	methods.clearItems=function(force_reset) {
		// set defaults 
		force_reset = typeof force_reset !== 'undefined' ? force_reset : true;	
		// remove all items
		this.items.clear();
		// update div
		if (force_reset) {
			this.div.innerHTML="";
		}
	};
	
})();
	
	




