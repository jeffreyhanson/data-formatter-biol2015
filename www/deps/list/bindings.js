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
				list.filterIds=[];
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
		// sort items by key field
		this.items.sort(function(x,y) {
			return(x.key > y.key) ? 1 : ((y.key > x.key) ? -1 : 0);
		});
		
		// remove all items from div
		this.item_list.innerHTML='';

		// add items to div
		if (this.status=='all') {
			for (i in this.items) {
				this.item_list.insertBefore(this.items[i].html, this.item_list.firstChild);
			}
		} else if (this.status=='filter') {
			for (i in this.items) {
				if ($.inArray(this.items[i].id, this.filterIds) > -1) {
					this.item_list.insertBefore(this.items[i].html, this.item_list.firstChild);
				}
			}
		} else {
			for (i in this.items) {
				if (this.items[i].status==this.status) {
					this.item_list.insertBefore(this.items[i].html, this.item_list.firstChild);
				}
			}
		}
	};
	
	methods.setView=function(status, force_reset) {
		// set defaults 
		force_reset = typeof force_reset !== 'undefined' ? force_reset : true;
		// change status
		this.status=status;
		this.filterIds=[];
		// refresh div
		if (force_reset) {
			methods['reloadView'].apply(lists[this.id]);
		}
	};
		
	methods.addItem=function(id, item, status, key, force_reset) {
		// set defaults
		force_reset = typeof force_reset !== 'undefined' ? force_reset : true;	
		// create item object
		var item_html=document.createElement("div");
		item_html.setAttribute("class", 'class="'+status+'-item-'+item+'"');
		item_html.setAttribute("item-id", id);
		item_html.innerHTML=item;
		this.items[id]={
			id: id,
			key: key,
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
		for (i in this.items) {
			if (this.items[i].id==id) {
				delete this.items[id];
			}
		}
 
		// refresh div
		if (force_reset)
			methods['reloadView'].apply(lists[this.id]);
	};
	
	methods.updateItem=function(id, item, status, key, force_reset) {
		// set defaults 
		force_reset = typeof force_reset !== 'undefined' ? force_reset : true;
		for (i in this.items) {
			if (this.items[i].id==id) {
				// update item
				var old_status=this.items[i].status
				if (typeof item !== 'undefined') {
					this.items[i].item=item;
				}
				if (typeof status !== 'undefined') {
					this.items[i].status=status;
				}
				// generate html repr
				var item_html=document.createElement("div");
				item_html.setAttribute("class", 'class="'+status+'-item-'+item+'"');
				item_html.setAttribute("item-id", id);
				item_html.innerHTML=item;
				this.items[i].html=item_html;
			}
		}
		// refresh div
		if (force_reset) {
			methods['reloadView'].apply(lists[this.id]);
		}
	};
	
	methods.filterItems=function(ids, force_reset) {
		// set defaults
		force_reset = typeof force_reset !== 'undefined' ? force_reset : true;
		if (typeof(ids) === 'string') {
			ids=[ids];
		} 
		// init
		this.status='filter';
		this.filterIds=ids;
		// refresh div
		if (force_reset) {
			methods['reloadView'].apply(lists[this.id]);
		}
	};
	
	methods.clearItems=function(force_reset) {
		// set defaults 
		force_reset = typeof force_reset !== 'undefined' ? force_reset : true;	
		// remove all items
		this.items.clear();
		this.filterIds=[];
		// update div
		if (force_reset) {
			this.div.innerHTML="";
		}
	};
	
})();
	
	




