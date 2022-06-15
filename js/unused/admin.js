(function(){
    var div = document.createElement("div");
    document.body.appendChild(div);
    div.appendChild(br());

    var inflate_label = document.createElement("span");
    inflate_label.innerHTML = "address: ";
    var address = document.createElement("input");
    var amount_label = document.createElement("span");
    amount_label.innerHTML = "amount: ";
    var amount = document.createElement("input");
    var inflate_button = button_maker2(
        "inflate money supply",
        function(){rpc.post(
            ["nonce", keys.pub()],
            function(nonce){
                var tx = [
                    "inflate", keys.pub(), nonce+1,
                    address.value,
                    parseInt(amount.value, 10)];
                var stx = keys.sign(tx);
                rpc.post(
                    ["inflate", stx],
                    function(x){
                        console.log(x);
                    });
            });
                  });
    div.appendChild(inflate_label);
    div.appendChild(address);
    div.appendChild(br());
    div.appendChild(amount_label);
    div.appendChild(amount);
    div.appendChild(br());
    div.appendChild(inflate_button);
    div.appendChild(br());
    div.appendChild(br());
    

    var oracle_text_label = document.createElement("span");
    oracle_text_label.innerHTML = "new market text:";
    var oracle_text = document.createElement("input");
    div.appendChild(oracle_text_label);
    div.appendChild(oracle_text);
    div.appendChild(br());

    var trues_label = document.createElement("span");
    trues_label.innerHTML = "shares of true:";
    var trues = document.createElement("input");
    div.appendChild(trues_label);
    div.appendChild(trues);
    div.appendChild(br());
    var falses_label = document.createElement("span");
    falses_label.innerHTML = "shares of false:";
    var falses = document.createElement("input");
    div.appendChild(falses_label);
    div.appendChild(falses);
    div.appendChild(br());
    var new_oracle_button = button_maker2(
        "make new oracle",
        function(){rpc.post(
            ["nonce", keys.pub()],
            function(nonce){
                var tx = [
                    "new_market", keys.pub(), nonce+1,
                    btoa(oracle_text.value),
                    parseInt(trues.value, 10),
                    parseInt(falses.value, 10)];
                var stx = keys.sign(tx);
                rpc.post(
                    ["new_market", stx],
                    function(x){
                        console.log(x);
                    });
            });
                  });
    div.appendChild(new_oracle_button);
    div.appendChild(br());
    div.appendChild(br());


    
    var result_label = document.createElement("span");
    result_label.innerHTML = "resolve oracle (0 means false, 1 means true, 0.3 means 30% for true, and 70% for false): ";
    var result = document.createElement("input");
    div.appendChild(result_label);
    div.appendChild(br());
    div.appendChild(result);
    div.appendChild(br());

    var market_div = document.createElement("div");
    div.appendChild(market_div);


    function display_markets(){
        rpc.post(["markets"], function(markets){
            markets = markets.slice(1);
            display_markets2(markets);
        });
    };
    display_markets();
    function display_markets2(markets){
        if(markets.length == 0){
            return(0);
        };
        rpc.post(
            ["market", markets[0]],
            function(market){
                var mid = market[1];
                var text = atob(market[2]);
                var span = document.createElement("span");
                span.innerHTML = "market: \""
                    .concat(text)
                    .concat("\"");
                market_div.appendChild(span);
                var button = button_maker2(
                    "settle this market",
                    function(){rpc.post(
                        ["nonce", keys.pub()],
                        function(nonce){
                            var r = Math.round(10000 * parseFloat(result.value, 10));
                            var tx = [
                                "oracle", keys.pub(),
                                nonce+1, mid, r
                            ];
                            console.log(tx);
                            var stx = keys.sign(tx);
                            rpc.post(
                                ["oracle", stx],
                                function(x){
                                    console.log(x);
                                });
                        });
                              });
                market_div.appendChild(button);
                market_div.appendChild(br());
            });
        display_markets2(markets.slice(1));
    };

})();
