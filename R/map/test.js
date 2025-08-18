function(el, x){
                var map = this;
                map.on("layeradd", function() {
                map.eachLayer(
                    function(layer){
                    if(layer instanceof L.Polygon){
                        layer.on("click", function(e){
                            map.fitBounds(layer.getBounds());
                        })
                    }
            })});
            }