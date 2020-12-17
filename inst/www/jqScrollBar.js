// JAVASCRIPT

//INPUT BINDING
var jqScrollBarBinding = new Shiny.InputBinding();
$.extend(jqScrollBarBinding, {
    find: function(scope) {
        return $(scope).find(".jqScrollBar");
    },
    initialize: function(el){
        //  Initialize element data here:
         let valueIni = $(el).attr(`data-ini`);
         valueIni=JSON.parse(valueIni)[0];
        $(el).data('value', valueIni)
        $(el).scrollTabs();
        let text=valueIni.text
        $(el).find('span').filter(function(){
                       if ($(this).text() === text){
                          $(this).trigger('click')
                      }
        })
        
    },
    getValue: function(el) {
      // Used for returning the value(s) of this input control
      let value=$(el).data('value');
      value=JSON.stringify(value);
      return value ;
    },
    setValue: function(el,  value) { 
      // used for updating input control
          $(el).data('value', value);
          $(el).trigger("change");
    },
    subscribe: function(el, callback) {
        // notify server whenever change
        $(el).on("change.jqScrollBarBinding", function(e) {
            callback();
        });
    },
    unsubscribe: function(el) {
        $(el).off(".jqScrollBarBinding");                              
    },
    receiveMessage: function(el, data) { //called when server sends update message
        if(!!data){
            let cmd = data.cmd;
            let text =data.text
            let id=el.id
            switch(cmd){
              case 'add': 
                  Object.keys(text).forEach(k=>$('#'+el.id).scrollTabs().addTab(
                    "<span rel='"+ text[k]+"'>"+k+"</span>", 0) 
                  )
                  break;
              case 'remove':
                  $('#'+el.id).scrollTabs().removeTabs("span:contains('"+text+"')")
                  break;
              case 'select':
                   $('#'+el.id).find('span').filter(function(){
                      if ($(this).text() === text){
                          $(this).trigger('click')
                      }
                  })
                  break;
              case 'rename':
                  $('#'+el.id).find('span').filter(function(){
                      if ($(this).text() === text.oldName){
                          $(this).text(text.newName)
                      }
                  })
                  break;
              case 'clear':
                  $('#'+el.id).scrollTabs().clearTabs();
                  break;
              default:
            }
        }
    },
    clicked: function(ctrlId, value, evt ){
      let el='#'+ctrlId;
      this.setValue( el, value)
    },
    
    getType: function(el){ 
      return "jqScrollBarBinding";
    }
});



// REGISTER INPUT BINDING
Shiny.inputBindings.register(jqScrollBarBinding);

