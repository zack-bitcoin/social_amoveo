async function account_div_maker(
    pub, id, noncer, sid, d)
    {
        if(id === undefined){
            id = await rpc.apost(["x", 2, pub]);
            id = id[1];
        }
        var them =
            await account_loader(noncer, sid, id);

        var title = document.createElement("h1");
        d.appendChild(title);
        var user_title = document.createElement("h3");
        user_title.innerHTML = them.username;
        d.appendChild(user_title);
        var description_p = document.createElement("p");
        description_p.innerHTML = them.description;
        d.appendChild(description_p);
        var pub = document.createElement("p");
        pub.innerHTML = them.pubkey;
        d.appendChild(pub);
        var coins_balance =
            document.createElement("p");
        coins_balance.innerHTML =
            "veo: ".concat(s2c(them.coins));
        d.appendChild(coins_balance);
        var coin_hours = document.createElement("p");
        coin_hours.innerHTML = "coin hours: ".concat(s2c(them.coin_hours));
        d.appendChild(coin_hours);

        //todo. this account is followed by these people who you follow.
        function is_in(a, b){
            if(b.length === 0) {return false;}
            if(a === b[0]){return true;}
            return(is_in(a, b.slice(1)));
        };
        var you_follow = following.all();
        var followers = [];
        await Promise.all(you_follow.map(async function(aid){
            var tx = ["x", keys.pub(),
                      noncer.check(),
                      sid, 17, aid];
            var stx = keys.sign(tx);
            var r = await rpc.signed(stx);
            r = r.slice(1);
            var b = is_in(id, r);
            if(b) { followers = followers
                    .concat([aid])};
        }));

        
        if(noncer && (!(id === noncer.id))){
            if(following.check(them.id)){
                var unfollow_button = header_button(
                    "unfollow "
                        .concat(them.username),
                    async function(){
                        var tx =
                            ["x", keys.pub(),
                             noncer.check(),
                             sid,19, them.id];
                        var stx = keys.sign(tx);
                        await rpc.signed(stx);
                        unfollow_button.innerHTML =
                            "successfully unfollowed "
                            .concat(them.username);
                        unfollow_button.onclick =
                            function(){};
                        following.remove(them.id);
                    });
                d.appendChild(unfollow_button);
            } else {
                
                var follow_button = header_button(
                    "follow "
                        .concat(them.username),
                    async function(){
                        var tx = ["x", keys.pub(),
                                  noncer.check(),
                                  sid, 3, them.id];
                        var stx = keys.sign(tx);
                        await rpc.signed(stx);
                        follow_button.innerHTML =
                            "successfully followed "
                            .concat(them.username);
                        follow_button.onclick =
                            function(){};
                        following.add(them.id);
                    });
                d.appendChild(follow_button);
            };
            d.appendChild(br());
        }
        if(!(followers.length === 0)){
            //var t = "followed by accounts that you follow: ";
            var s = document.createElement("span");
            s.innerHTML = "followed by accounts that you follow: ";
            d.appendChild(s);
            await Promise.all(followers.map(
                async function(aid){
                    var acc = await account_loader(
                        noncer, sid, aid,
                        "cached");
                    var button = header_button(
                        acc.username,
                        function(){
                            main.load_account_page(
                                acc, noncer, sid)
                        });
                    d.appendChild(button);
                    var t = document.createElement("span");
                    t.innerHTML = ", ";
                    d.appendChild(t);
                }));
            d.appendChild(br());
        }
        
        var spend_amount =
            text_input("amount: ", d);
        var spend_ch_button =
            button_maker2(
                "send them coin hours",
                async function(){
                    var tx = ["x", keys.pub(),
                              noncer.check(),
                              sid, 0, 0,
                              c2s(spend_amount),
                              them.id];
                    var stx = keys.sign(tx);
                    await rpc.signed(stx);
                    delete accounts_memoized[noncer.id];
                    delete accounts_memoized[them.id];
                    d.innerHTML = "";
                    account_div_maker(
                        pub, id, noncer, sid, d);
                });
        var delegate_button =
            button_maker2(
                "delegate coins to them",
                async function(){
                    var tx = ["x", keys.pub(),
                              noncer.check(),
                              sid, 0,
                              c2s(spend_amount),
                              0,
                              them.id];
                    var stx = keys.sign(tx);
                    await rpc.signed(stx);
                    delete accounts_memoized[noncer.id];
                    delete accounts_memoized[them.id];
                    d.innerHTML = "";
                    account_div_maker(
                        pub, id, noncer, sid, d);
                });
        d.appendChild(spend_ch_button);
        d.appendChild(delegate_button);
        
        
        return(d);
    };
