var following = (function(){
    var l = "empty";
    async function load(sid, noncer){
        var tx = ["x", keys.pub(),
                  noncer.check(),
                  sid, 17, noncer.id];
        var stx = keys.sign(tx);
        var r = await rpc.signed(stx);
        r = r.slice(1);
        l = {};
        r.map(function(x){
            l[x]= true;
        });
    };
    function check(id){
        if(l === "empty"){return("error")};
        if(l[id] === undefined){return(false)};
        return(l[id]);
    };
    function remove(id){
        if(l === "empty"){return("error")};
        delete l.id;
    };
    function add(id){
        if(l === "empty"){return("error")};
        l[id] = true;
    };
    function all(){
        var k = Object.keys(l);
        return(k.map(function(x){
            return(parseInt(x))}));
    };
    return({
        load: load,
        check: check,
        add: add,
        remove: remove,
        all: all
    });
})();
