async function account_div_maker(
    pub, id, noncer, sid, d)
    {
        if(id === undefined){
            id = await rpc.apost(["x", 2, pub]);
            id = id[1];
        }
        var data =
            await account_loader(noncer, sid, id);

        //todo follow/unfollow button/indicator.

        //todo a way to see the list of who they follow.

        var title = document.createElement("h1");
        d.appendChild(title);
        var user_title = document.createElement("h3");
        user_title.innerHTML = data.username;
        d.appendChild(user_title);
        var description_p = document.createElement("p");
        description_p.innerHTML = data.description;
        d.appendChild(description_p);
        var pub = document.createElement("p");
        pub.innerHTML = data.pubkey;
        d.appendChild(pub);
        var coins_balance =
            document.createElement("p");
        coins_balance.innerHTML =
            "veo: ".concat(s2c(data.coins));
        d.appendChild(coins_balance);
        var coin_hours = document.createElement("p");
        coin_hours.innerHTML = "coin hours: ".concat(s2c(data.coin_hours));
        d.appendChild(coin_hours);
        if(noncer && (!(id === noncer.id))){
            if(following.check(data.id)){
                var unfollow_button = header_button(
                    "unfollow "
                        .concat(data.username),
                    async function(){
                        var tx =
                            ["x", keys.pub(),
                             noncer.check(),
                             sid,19, data.id];
                        var stx = keys.sign(tx);
                        await rpc.signed(stx);
                        unfollow_button.innerHTML =
                            "successfully unfollowed "
                            .concat(data.username);
                        unfollow_button.onclick =
                            function(){};
                        following.remove(data.id);
                    });
                d.appendChild(unfollow_button);
            } else {
                
                var follow_button = header_button(
                    "follow "
                        .concat(data.username),
                    async function(){
                        var tx = ["x", keys.pub(),
                                  noncer.check(),
                                  sid, 3, data.id];
                        var stx = keys.sign(tx);
                        await rpc.signed(stx);
                        follow_button.innerHTML =
                            "successfully followed "
                            .concat(data.username);
                        follow_button.onclick =
                            function(){};
                        following.add(data.id);
                    });
                d.appendChild(follow_button);
            };
        }
        
        return(d);
    };
