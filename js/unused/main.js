(function(){
    var div = document.createElement("div");
    document.body.appendChild(div);
    div.appendChild(br());
    var selected_div = document.createElement("div");
    div.appendChild(selected_div);
    var selected_mid = "";

    var amount_label = document.createElement("span");
    amount_label.innerHTML = "amount: ";
    var amount = document.createElement("input");
    div.appendChild(amount_label);
    div.appendChild(amount);
    div.appendChild(br());

    var buy_shares = button_maker2(
        "buy shares",
        function(){rpc.post(
            ["nonce", keys.pub()],
            function(nonce){
                var tx = [
                    "buy_shares", keys.pub(),
                    nonce+1,
                    parseInt(amount.value, 10),
                    selected_mid];
                var stx = keys.sign(tx);
                rpc.post(
                    ["buy_shares", stx],
                    function(x){
                        reload_balance();
                    });
            });
                  });
    div.appendChild(buy_shares);
    var combine_shares = button_maker2(
        "combine shares",
        function(){rpc.post(
            ["nonce", keys.pub()],
            function(nonce){
                var tx = [
                    "combine_shares", keys.pub(),
                    nonce+1, selected_mid];
                var stx = keys.sign(tx);
                rpc.post(
                    ["combine_shares", stx],
                    function(x){
                        console.log(x);
                        reload_balance();
                    });
            })});
    div.appendChild(combine_shares);
    var bet_true = button_maker2(
        "bet true", bet(1));
    div.appendChild(bet_true);
    var bet_false = button_maker2(
        "bet false", bet(0));
    div.appendChild(bet_false);
    function bet(type){
        return(function(){rpc.post(
            ["nonce", keys.pub()],
            function(nonce){
                var tx = [
                    "bet", keys.pub(), nonce+1,
                    selected_mid, type, 
                    parseInt(amount.value, 10)];
                var stx = keys.sign(tx);
                rpc.post(
                    ["bet", stx],
                    function(x){
                        reload_balance();
                    });
            })});
    };

    var market_div = document.createElement("div");
    div.appendChild(market_div);
    
    function get_btc_balance(){
        rpc.post(["balance", keys.pub()], function(bal){
            var span = document.createElement("span");
            span.innerHTML = "your balance: "
                .concat(bal)
                .concat("");
            market_div.appendChild(span);
            market_div.appendChild(br());
        });
    };
    function display_markets(){
        rpc.post(["markets"], function(markets){
            markets = markets.slice(1);
            display_markets2(markets);
        });
    };
    get_btc_balance();
    display_markets();
    function display_markets2(markets){
        if(markets.length == 0){
            return(0);
        };
        rpc.post(
            ["market", markets[0]],
            function(market){
                var mid = market[1];
                rpc.post(["balance", keys.pub(), mid],
                         function(bal){
                             var text = atob(market[2]);
                             var trues = market[3];
                             var falses = market[4];
                             var price = (trues / (falses + trues)).toFixed(2).toString();
                             var span = document.createElement("span");
                             span.innerHTML = "market: \""
                                 .concat(text)
                                 .concat("\" price: ")
                                 .concat(price)
                                 .concat("; liquidity: ")
                                 .concat(Math.round(Math.sqrt(trues * falses)))
                                 .concat("; your shares of true: ")
                                 .concat(bal[1])
                                 .concat("; your shares of false: ")
                                 .concat(bal[2])
                                 .concat("");
                             market_div.appendChild(span);
                             var button = button_maker2("select", function(){
                                 selected_mid = market[1];
                                 selected_div.innerHTML = "selected market: "
                                     .concat(text);
                             });
                             market_div.appendChild(button);
                             market_div.appendChild(br());
                             display_markets2(markets.slice(1));
                         });
            });
    };
    function reload_balance(){
        market_div.innerHTML = "";
        get_btc_balance();
        display_markets();
    };
    keys.update_balance_callback(function(){
        reload_balance();
    });
    //return({display_markets: display_markets});
})();
