async function account_div_maker(
    pub, id, noncer, sid, d)
    {
        if(id === undefined){
            id = await rpc.apost(["x", 2, pub]);
            id = id[1];
        }
        var data =
            await account_loader(noncer, sid, id);
        //console.log(data);
        //var d = document.createElement("div");
        var title = document.createElement("h1");
        title.innerHTML = "account info";
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
        var coins_balance = document.createElement("p");
        coins_balance.innerHTML = "veo: ".concat(s2c(data.coins));
        d.appendChild(coins_balance);
        var coin_hours = document.createElement("p");
        coin_hours.innerHTML = "coin hours: ".concat(s2c(data.coin_hours));
        d.appendChild(coin_hours);
        return(d);
    };
