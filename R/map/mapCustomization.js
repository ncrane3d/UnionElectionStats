function(el, x){
    var map = this;
    map.eachLayer(function(layer){
        if(layer instanceof L.Polygon){
            layer.on('click', function(e){
                map.setBounds(layer.getBounds())
            })
        }
    })
}

function(el, x){
    var map = this;
    map.eachLayer(function(layer){
        if(layer instanceof L.Polygon){
            layer.on('click', function(e){
                map.setBounds(layer.getBounds());
            })
        }
    });
}