(async function(){
    var div = document.createElement("div");
    document.body.appendChild(div);
    div.appendChild(br());

    var topdiv = document.createElement("div");
    var middiv = document.createElement("div");
    var lowdiv = document.createElement("div");
    div.appendChild(topdiv);
    div.appendChild(middiv);
    div.appendChild(lowdiv);

    var sid = await rpc.apost(["sid"]);
    console.log(sid);

    var accountdiv;
    var my_nonce;
    keys.update_balance_callback(async function(){
        my_nonce = await nonce_builder(keys.pub());
        accountdiv = account_div_maker(keys.pub(), my_nonce.id);
    })
    

    async function account_div_maker(pub, id){
        if(id === undefined){
            id = await rpc.apost(["x", 2, pub]);
            id = id[1];
        }
        var tx;
        var n = my_nonce.check();
        tx = ["balance", keys.pub(), n, sid, id]
        console.log(tx);
        var stx = keys.sign(tx);
        var r = await rpc.signed(stx);
        console.log(r);
        var d = document.createElement("div");
        var data = {
            id: id,
            username: atob(r[2]),
            description: atob(r[3]),
            pubkey: atob(r[1]),
            recent_posts: [],
            coins: r[5],
            coin_hours: r[7]
        };
        console.log(data);
        function load() {
            //use rpc to load data.
        };
        
/*
* account
  - if it is your account, make it editable.
  - username
  - description
  - pubkey
  - recent posts
  - balances
  - a way to spend
*/

        return(data);
    };
    
    //const urlParams = new URLSearchParams(window.location.search);
    //var pubkey = urlParams.get('pubkey');
    //pubkey = pubkey.replace(/\ /g, "+");


})();
