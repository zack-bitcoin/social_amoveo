var main;
(async function(){
    var div = document.createElement("div");
    document.body.appendChild(div);

    var show_posts_in_batches_of = 10;

/*
    function header_button(text, f){
        var x = document.createElement("span");
        x.innerHTML = text;
        x.style.color = "blue";
        x.onclick = function(){
            f();
        };
        return(x);
    }
*/
    function clear_page(){
        topdiv.innerHTML = "";
        middiv.innerHTML = "";
        lowdiv.innerHTML = "";
    };

    var source_link = document.createElement("a");
    source_link.innerHTML = "source code for this website";
    source_link.href = "https://github.com/zack-bitcoin/social_amoveo";
    div.appendChild(source_link);
    div.appendChild(br());

    var login_button =
        header_button(
            "login ",
            function(){
                clear_page();
                var title1 = document.createElement("h3");
                title1.innerHTML = "load keys from file, or from a brain wallet.";
                topdiv.appendChild(title1);
                topdiv.appendChild(keys.div);
                var title = document.createElement("h3");
                title.innerHTML = "request funds from your cold storage wallet.";
                middiv.appendChild(title);
                var their_pub =
                    text_input("request funds from pubkey: ",
                               middiv);
                their_pub.value = "BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4=";
                middiv.appendChild(br());
                var coins = text_input("request this many coins: ",
                                       middiv);
                coins.value = "1";
                middiv.appendChild(br());
                var coin_hours =
                    text_input("request this many coin-hours",
                               middiv);
                coin_hours.value = "1000";
                middiv.appendChild(br());
                var make_request_button =
                    button_maker2(
                        "make request",
                        async function(){
                            var tp = their_pub.value;
                            
                            var their_id = await rpc.apost(["x", 2, tp]);
                            their_id = their_id[1];
                            var their_nonce = await rpc.apost(["x", 1, their_id]);
                            //their_nonce =
                            //    their_nonce[1];
                            //console.log(coins.value);
                            //console.log(parseInt(coins.value));
                            //console.log(c2s(coins));
                            var tx = [
                                "x", tp,
                                their_nonce+1,
                                sid, 0, c2s(coins),
                                c2s(coin_hours),
                                keys.pub()
                            ];
                            var s = document.createElement("span");
                            s.innerHTML = JSON.stringify(tx);
                            middiv.appendChild(s);
                            middiv.appendChild(br());
                            var instructions = document.createElement("span");
                            instructions.innerHTML = "use the amoveo light node to sign this request";
                            middiv.appendChild(instructions);
                            //console.log(JSON.stringify(tx));
                        });
                middiv.appendChild(make_request_button);
                middiv.appendChild(br());
                var publish_tx = text_input("signed request: ", lowdiv);
                var publish_button =
                    button_maker2(
                        "publish request",
                        async function(){
                            var r = await rpc.signed(JSON.parse(publish_tx.value));
                            if(r === 0){
                                publish_tx.value = "";
                                keys.update_balance();
                            } else {
                                console.log(JSON.stringify(r));
                            };
                        });
                lowdiv.appendChild(publish_button);
            });
    div.appendChild(login_button);
    div.appendChild(span_dash());

    var your_account_button =
        header_button(
            "your account ",
            function(){
                clear_page();
                topdiv.appendChild(my_account_div);
                topdiv.appendChild(make_post_div);
                topdiv.appendChild(my_posts_div);
            });
    div.appendChild(your_account_button);
    div.appendChild(span_dash());

    var top_posts =
        header_button(
            "top posts",
            function(){
                clear_page();
                reload_top_posts();
                topdiv.appendChild(top_posts_div);
            });
    div.appendChild(top_posts);
    div.appendChild(span_dash());
    //todo. a tab for the most hated posts.

    var following_posts =
        header_button(
            "posts by who you follow",
            async function(){
                clear_page();
                var following_posts_div =
                    document.createElement("div");
                var tx = ["x", keys.pub(),
                          my_nonce.check(),
                          sid, 16,
                          [-6].concat(following.all())];
                var stx = keys.sign(tx);
                var r = await rpc.signed(stx);
                r = r.slice(1);
                //[[pid...],[pid ...]...]
                var posts = [];
                r.map(function(x){
                    x = x.slice(1);
                    posts = posts.concat(x);
                });
                posts = posts.sort(function(a, b){
                    return(b[3] + b[4]
                           - a[3] - a[4]);
                });
                posts_div_maker(
                    posts, my_nonce, sid,
                    following_posts_div,
                    true,
                    show_posts_in_batches_of);
                topdiv.appendChild(
                    following_posts_div);
            });

    div.appendChild(following_posts);
    div.appendChild(span_dash());
    div.appendChild(br());

    var voted_posts =
        header_button(
            "posts voted on by who you follow",
            async function(){
                //list of who you follow.
                var tx =
                    ["x", keys.pub(),
                     my_nonce.check(), sid,
                     17, my_nonce.id];
                var stx = keys.sign(tx);
                var r = await rpc.signed(stx);
                var following = r.slice(1);
                console.log(JSON.stringify(following));
                var votes = await Promise.all(following.map(
                    async function(x){
                        var tx =
                            ["x", keys.pub(),
                             my_nonce.check(), sid,
                             31, x];
                        var stx = keys.sign(tx);
                        var r = await rpc.signed(stx);
                        r = r.slice(1);
                        return(r);
                    }));
                votes = votes.reduce(function(a, b){ return(a.concat(b));}, []);
                //vote = [-7, id, amount, direction, time]
                //list of votes from these accounts.
                //accumulate repeated votes on same post.
                votes = accumulate_repeated_votes(votes);
                //sort post ids based on total voted.
                votes = votes.sort(function(a, b){
                    return(b[2] - a[2]);});

                clear_page();
                posts_div_maker(
                    votes, my_nonce, sid, topdiv,
                    true, show_posts_in_batches_of
                );
                    
            });
    
    div.appendChild(voted_posts);
    div.appendChild(span_dash());

    var notifications_button =
        header_button(
            "comment notifications",
            async function(){
                clear_page();
                var tx = ["x", keys.pub(),
                          my_nonce.check(),
                          sid, 28];
                //console.log(JSON.stringify(tx));
                var stx = keys.sign(tx);
                var r = await rpc.signed(stx);
                r = r.slice(1);
                r = r.map(function(x){
                    return([-1, x[2], 0, 0, 0]);
                });
                //{0, pid}
                posts_div_maker(
                    r, my_nonce, sid, topdiv, true,
                    show_posts_in_batches_of);
                notifications_button.style.color =
                    "blue";
                notifications_button.innerHTML =
                    "comment notifications";
            });
    div.appendChild(notifications_button);
    div.appendChild(br());

    var dms_button =
        header_button(
            "inbox",
            async function(){
                clear_page();
                var tx = ["x", keys.pub(),
                          my_nonce.check(),
                          sid, 32];
                var stx = keys.sign(tx);
                var r = await rpc.signed(stx);
                var [db, db_notifications] =
                    load_dms(r);
                var peer_order = r2peer_order(r);
                console.log(peer_order);
                console.log(my_nonce.id);
                await Promise.all(peer_order.map(
                    async function(aid){
                        var x = await account_loader(
                            my_nonce, sid, aid);
                        var n = db_notifications[aid];
                        var link = header_button(
                            "",
                            async function(){

                                return(conversation_page(
                                    x, db[aid], my_nonce, sid));
                            });
                        if(n){
                            link.innerHTML =
                                "conversation with: "
                                .concat(x.username)
                                .concat(" +")
                                .concat(n.toString());
                            link.style.color = "red";
                        } else {
                            link.innerHTML =
                                "conversation with: "
                                .concat(x.username);
                        };
                        topdiv.appendChild(link);
                        topdiv.appendChild(br());
                //todo. for each peer, make a link to the conversation between us and that peer. include their username in the link.
                        console.log(JSON.stringify(x));
                        return(x);
                    }));
                dms_button.style.color = "blue";
                dms_button.innerHTML = "inbox";
            });
    div.appendChild(dms_button);
    div.appendChild(br());
    

    var topdiv = document.createElement("div");
    var middiv = document.createElement("div");
    var lowdiv = document.createElement("div");
    div.appendChild(topdiv);
    div.appendChild(middiv);
    div.appendChild(lowdiv);

    login_button.click();

    var sid = await rpc.apost(["sid"]);
    //console.log(sid);

    var my_account_div = div_ele();
    my_account_div.innerHTML = "this account does not exist.";

    var make_post_div = div_ele();
    var my_posts_div = div_ele();
    var top_posts_div = div_ele();

    var post_text =
        document.createElement("textarea");
    post_text.rows = 4;
    post_text.cols =
        Math.min(window.innerWidth / 10,
                 45);
    make_post_div.appendChild(post_text);
    
    var post_button =
        button_maker2(
            "make post",
            async function(){
                if(post_text.value.length > 512){
                    post_button.value = "make post - error, you can only have 512 characters. that is ".concat((post_text.value.length)).concat(" characters");
                }
                var tx =
                    ["x", keys.pub(),
                     my_nonce.check(), sid, 4,
                     btoa(post_text.value)];
                var stx = keys.sign(tx);
                var r = await rpc.signed(stx);
                refresh_my_page();
                //console.log(r);
                //post_text.innerHTML = "";
                post_text.value = "";
            });
    make_post_div.appendChild(br());
    make_post_div.appendChild(post_button);

    var my_nonce;


    const urlParams = new URLSearchParams(window.location.search);
    var post_id = urlParams.get('post');

    if(post_id){
        post_id = post_id.replace(/\ /g, "+");
       // console.log(post_id);
        var post =
            await post_loader(
                my_nonce, sid, parseInt(post_id));
        console.log(JSON.stringify(post));
        load_post_page(post, my_nonce, sid);
    };

    async function refresh_my_page(){
        my_account_div.innerHTML = "";
        my_posts_div.innerHTML = "";
        //account_div_maker(
        //    keys.pub(), my_nonce.id,
        //    my_nonce, sid, my_account_div);
        var account_data =
            await account_loader(
                my_nonce, sid, my_nonce.id);
        //var title = document.createElement("h3");
        //title.innerHTML = "your account";
        //my_account_div.appendChild(title);
        var user_title =
            text_input("username: ",
                       my_account_div);
        user_title.value = account_data.username;
        var update_title_button =
            button_maker2(
                "update username",
                async function(){
                    var tx = ["x", keys.pub(),
                     my_nonce.check(),
                     sid, 1,
                     btoa(user_title.value)];
                    var stx = keys.sign(tx);
                    await rpc.signed(stx);
                    delete accounts_memoized[my_nonce.id];
                });
        my_account_div.appendChild(
            update_title_button);
        my_account_div.appendChild(br());
        var description =
            text_input("description: ",
                       my_account_div);
        description.value =
            account_data.description;
        var update_description_button =
            button_maker2(
                "update description",
                async function(){
                    var tx = ["x", keys.pub(),
                              my_nonce.check(),
                              sid, 2,
                              btoa(description.value)];
                    var stx = keys.sign(tx);
                    await rpc.signed(stx);
                });
        my_account_div.appendChild(
            update_description_button);
        my_account_div.appendChild(br());
        var coins_balance =
            document.createElement("div");
        coins_balance.innerHTML =
            "coins: ".concat(
                s2c(account_data.coins));
        my_account_div.appendChild(coins_balance);
        var coin_hours =
            document.createElement("div");
        coin_hours.innerHTML =
            "coin hours: ".concat(
                s2c(account_data.coin_hours));
        my_account_div.appendChild(coin_hours);
        //my_account_div.appendChild(br());
        var following_link =
            header_button(
                "following",
                async function(){
                    var follow_div = await
                    following_div_maker(
                        sid,
                        my_nonce.id,
                        my_nonce);
                    clear_page();
                    topdiv.appendChild(follow_div);
                });
        my_account_div.appendChild(following_link);
        my_account_div.appendChild(br());
        var delegating_link =
            header_button(
                "delegating",
                async function(){
                    var delegating_div = await
                    delegating_div_maker(
                        sid, my_nonce);
                    clear_page();
                    topdiv.appendChild(
                        delegating_div);
                });
        my_account_div.appendChild(
            delegating_link);
        my_account_div.appendChild(br());
        var pids =
            await rpc.apost(["top", my_nonce.id]);
        pids = pids.slice(1);
        posts_div_maker(
            pids,
            my_nonce, sid, my_posts_div,
            false, show_posts_in_batches_of);
    };

    async function dms_notifications_cron(pub){
        console.log("dm notifications cron\n");
        if(!(keys.pub() === pub)){
            return(0);
        };
        var tx = ["x", keys.pub(),
                  my_nonce.check(),
                  sid, 34];
        var stx = keys.sign(tx);
        var r = await rpc.signed(stx);
        if(!(keys.pub() === tx[1])){
            return(0);
        };
        if(r > 0){
            dms_button.style.color =
                "red";
            dms_button.innerHTML =
                "inbox +".concat(r);
        }
    };
    
    async function notifications_cron(pub){
        console.log("notifications cron\n");
        if(!(keys.pub() === pub)){
            return(0);
        };
        var tx = ["x", keys.pub(),
                  my_nonce.check(),
                  sid, 30];
        var stx = keys.sign(tx);
        var r = await rpc.signed(stx);
        if(!(keys.pub() === tx[1])){
            return(0);
        };
        if(r > 0){
            notifications_button.style.color =
                "red";
            notifications_button.innerHTML =
                "comment notifications +".concat(r);
        }
        notifications_cron(pub);
    };

    
    keys.update_balance_callback(async function(){
        //if(my_nonce === undefined){
        my_nonce =
            await nonce_builder(keys.pub());
        //};
        clear_page();
        refresh_my_page();
        await following.load(sid, my_nonce);
        your_account_button.click();
        var tx = ["x", keys.pub(),
                  my_nonce.check(),
                  sid, 29];
        //console.log(JSON.stringify(tx));
        var stx = keys.sign(tx);
        var r = await rpc.signed(stx);
        //console.log(r);
        if(r > 0){
            notifications_button.style.color =
                "red";
            notifications_button.innerHTML =
                "comment notifications +".concat(r);
        }
        var tx = ["x", keys.pub(),
                  my_nonce.check(),
                  sid, 33];
        var stx = keys.sign(tx);
        var r = await rpc.signed(stx);
        if(r > 0){
            dms_button.style.color = "red";
            dms_button.innerHTML =
                "inbox +".concat(r);
        }
        notifications_cron(keys.pub());
        dms_notifications_cron(keys.pub());
    })

    async function reload_top_posts(){
        top_posts_div.innerHTML = "";
        var top_pids =
            await rpc.apost(["top"]);
        top_pids = top_pids.slice(1);
        posts_div_maker(
            top_pids.reverse(),
            my_nonce, sid, top_posts_div,
            true, show_posts_in_batches_of);
        topdiv.appendChild(top_posts_div);
    };

    async function posts_div_maker(
        pids, noncer, sid, d, show_author, n)
    {
        //console.log(JSON.stringify(pids));
        if(pids.length === 0){
            return(0);
        }
        if(n === 0){
            var load_more_button =
                header_button(
                    "load more posts",
                    function(){
                        lowdiv.innerHTML = "";
                        posts_div_maker(
                            pids, noncer, sid, d,
                            show_author, show_posts_in_batches_of)
                    });
            lowdiv.appendChild(load_more_button);
            return(0);
        };
        var id = pids[0][1];
        //console.log(JSON.stringify(pids));
        if(id === "null"){
            return(posts_div_maker(
                pids.slice(1), noncer, sid, d,
                show_author, n-1));
        };
        var post;
        if(pids[0][0] === "post"){
            post = decode_post(pids[0]);
        } else {
            post = await post_loader(
                noncer, sid, id);
        }
        if(post[0] === "error"){
            return(posts_div_maker(
                pids.slice(1), noncer, sid, d,
                show_author, n));
        };
        //console.log(JSON.stringify(post));
        var acc =
            await account_loader(
                noncer, sid, post.author_id);
        //console.log([noncer.id, post.author_id]);
        var author_name = acc.username;
        var s = "";
        console.log(author_name);
        if(show_author &&
           (!(author_name === undefined))){
            var author_link = header_button(
                "author: "
                    .concat(acc.username.slice(0, 30))
                    .concat("<br/>"),
                async function(){
                    load_account_page(
                        acc, noncer, sid)
                })

            d.appendChild(author_link);
            s = s
                .concat(" author balance:")
                .concat(s2c(acc.coins).toFixed(2))
                .concat(" ");
        }
        if(post.text){
            s = ""
                .concat("<span style=\"font-size:130%\">")
                .concat(post.text)
                .concat("</span>")
                .concat("<br/>")
                .concat("comments:")
                .concat(post.comments.length)
                .concat(" upvotes:")
                .concat(s2c(post.upvotes).toFixed(2))
                .concat(" downvotes:")
                .concat(s2c(post.downvotes).toFixed(2))
                .concat(s)
            ;
            var post_p = document.createElement("span");
            post_p.className = "clickable";
            post_p.onclick = function(){
                load_post_page(post, noncer, sid);
                return(0);
            };
            post_p.innerHTML = s;
            d.appendChild(post_p);
        }
        var ts = post.server_timestamp;
        ts = (ts[1]*1000000*1000) + (ts[2]*1000);
        var date = new Date(ts);

        var date_span =
            document.createElement("span");
        date_span.innerHTML = " date: "
            .concat(date.toLocaleString());
        d.appendChild(date_span);

        if(noncer &&
           (noncer.id === post.author_id)){
            var confirm_div =
                document.createElement("span");
            var delete_post_button = header_button(
                "delete",
                async function(){
                    var confirm_button = header_button(
                        "click here to delete",
                        async function(){
                            var tx = ["x", keys.pub(),
                                      noncer.check(),
                                      sid, 25, post.pid];
                            //console.log(JSON.stringify(tx));
                            var stx = keys.sign(tx);
                            await rpc.signed(stx);
                            refresh_my_page();
                            your_account_button
                                .click();
                        });
                    confirm_div.innerHTML = "";
                    confirm_div.appendChild(br());
                    confirm_div.appendChild(confirm_button);
                });
            d.appendChild(br());
            d.appendChild(delete_post_button);
            d.appendChild(confirm_div);
        };
        
        d.appendChild(br());
        d.appendChild(br());

        return(posts_div_maker(
            pids.slice(1), noncer, sid, d,
            show_author, n-1));
    };
    
    async function load_account_page(acc, noncer, sid){
        var this_account_div =
            document.createElement("div");
        var this_account_posts_div =
            document.createElement("div");
        account_div_maker(
            acc.pub, acc.id,
            noncer, sid,
            this_account_div);
        var pids =
            await rpc.apost([
                "top", acc.id]);
        pids = pids.slice(1);
        posts_div_maker(
            pids,noncer, sid,
            this_account_posts_div,
            false, show_posts_in_batches_of);
        clear_page();
        topdiv.appendChild(this_account_div);
        topdiv.appendChild(this_account_posts_div);
    };
    async function load_post_page(post, noncer, sid){
        console.log("load post page");
        var post_div =
            document.createElement("div");
        var make_comment_div =
            document.createElement("div");
        var comments_div =
            document.createElement("div");

        var many_ancestors_to_show = 5;
        
        var parent_ids =
            await comment_parent_chain(
                post, noncer, sid, post_div,
            many_ancestors_to_show);
        parent_ids = parent_ids.map(function(x){
            return([-1, x, 0,0,0])});
        parent_ids = parent_ids.reverse();
        await posts_div_maker(
            parent_ids, noncer, sid, post_div,
            true, many_ancestors_to_show);

        var link = document.createElement("a");
        link.href = "?post=".concat(post.pid);
        link.innerHTML = "shareable link to this post";
        link.target = "_blank";
        post_div.appendChild(link);
        posts_div_maker(post.comments.map(
            function(x){return([-1, x, 0, 0, 0])}),
                        noncer, sid,
                        comments_div, true, show_posts_in_batches_of);


        var upvote_button =
            header_button(
                "upvote",
                async function(){
                    await vote(post.pid, noncer, sid, 1);
                    delete posts_memoized[post.pid];
                    var post2 = await post_loader(
                        noncer, sid, post.pid);
                    load_post_page(post2, noncer, sid);
                });
        make_comment_div.appendChild(
            upvote_button);
        make_comment_div.appendChild(span_dash());
        var downvote_button =
            header_button(
                "downvote",
                async function(){
                    await vote(post.pid, noncer, sid, -1);
                    delete posts_memoized[post.pid];
                    var post2 = await post_loader(
                        noncer, sid, post.pid);
                    load_post_page(post2, noncer, sid);
                });
        make_comment_div.appendChild(
            downvote_button);
        make_comment_div.appendChild(br());
            
        
        var comment_text =
            document.createElement("textarea");
        comment_text.rows = 4;
        comment_text.cols =
            Math.min(window.innerWidth / 10,
                    45);
        make_comment_div.appendChild(comment_text);
        make_comment_div.appendChild(br());

        var comment_button =
            button_maker2(
                "make comment",
                async function(){
                if(comment_text.value.length > 512){
                    comment_button.value = "make comment - error, you can only have 512 characters. that is ".concat((comment_text.value.length)).concat(" characters");
                }
                    var tx = [
                        "x", keys.pub(),
                        my_nonce.check(),
                        sid, 5,
                        btoa(comment_text.value),
                        post.pid];
                    var stx = keys.sign(tx);
                    var r = await rpc.signed(stx);
                    comment_text.value = "";
                    delete posts_memoized[post.pid];
                    var post2 = await post_loader(
                        noncer, sid, post.pid);
                    refresh_my_page();
                    load_post_page(post2, noncer, sid);
                });
        make_comment_div.appendChild(comment_button);
        //todo. time when this was posted.
        clear_page();
        topdiv.appendChild(post_div);
        topdiv.appendChild(make_comment_div);
        topdiv.appendChild(comments_div);
    };
    async function comment_parent_chain(post, noncer, sid, n){
        if(n === 0){
            return([]);
        } else if(!post.pid){
            console.log(JSON.stringify(post));
            return([]);
        } else if(!post.parent ||
                  (post.parent === 0)){
            return([post.pid]);
        } else {
            var parent = await post_loader(
                noncer, sid,
                post.parent);
            var x = await comment_parent_chain(
                parent, noncer, sid, n - 1);
            return([post.pid].concat(x));
        }
    };
    async function vote(
        pid, noncer, sid, direction){
        if(direction === 1){
        } else if(direction === -1){
        } else {
            console.log("invalid vote direction");
            return(0);
        };

        var data =
            await account_loader(
                noncer,sid, noncer.id);
        var coins = data.coins;
        var amount =
            Math.floor(coins/100);
        //vote with 1% of funds
        var tx = ["x", keys.pub(),
                  noncer.check(), sid,
                  6, pid, amount,
                  direction];
        //scale all by 97%
        var tx2 = ["x", keys.pub(),
                   noncer.check(),
                   sid, 8, 970000];
        //remove votes with less than a minimum value of 0.1% of funds
        var min =
            Math.floor(coins/1000);
        var tx3 = ["x", keys.pub(),
                   noncer.check(),
                   sid, 9, min];
        var stx = keys.sign(tx);
        var stx2 = keys.sign(tx2);
        var stx3 = keys.sign(tx3);
        //console.log(JSON.stringify(stx));
        //console.log(JSON.stringify(stx2));
        //console.log(JSON.stringify(stx3));
        await rpc.signed(stx);
        await rpc.signed(stx2);
        await rpc.signed(stx3);
    };
    async function following_div_maker(
        sid, id, noncer){
        var div = document.createElement("div");
        var tx = ["x", keys.pub(),
                  noncer.check(), sid, 17, id];
        var stx = keys.sign(tx);
        var r = await rpc.signed(stx);
        r = r.slice(1);
        if(r.length === 0){
            div.innerHTML =
                "<h3>not following anyone</h3>";
        } else {
            following_div_maker2(
                r, sid, id, noncer, div,
                show_posts_in_batches_of);
        };
        return(div);
    };
    async function following_div_maker2(
        accs, sid, aid, noncer, div, n
    ){
        if(accs.length === 0){
            return(0);
        };
        if(n === 0){
            var load_more_button =
                header_button(
                    "load more accounts",
                    function(){
                        lowdiv.innerHTML = "";
                        following_div_maker2(
                            accs, sid, aid,
                            noncer, div, show_posts_in_batches_of);
                    });
            lowdiv.appendChild(load_more_button);
            return(0);
        };
        var aid = accs[0];
        var acc = await account_loader(
            noncer, sid, aid);
        var link = header_button(
            acc.username.slice(0, 30),
            async function(){
                load_account_page(
                    acc, noncer, sid);
            })
        div.appendChild(link);
        div.appendChild(br());
        return(following_div_maker2(
            accs.slice(1), sid, aid,
            noncer, div, n-1));
    };
    async function delegating_div_maker(sid, noncer){
        var div = document.createElement("div");
        var tx = ["x", keys.pub(),
                  noncer.check(), sid, 21,
                  noncer.id];
        console.log(JSON.stringify(tx));
        var stx = keys.sign(tx);
        var r = await rpc.signed(stx);
        r = r.slice(1);
        r = r.map(function(x)
                  {return(x.slice(1));});
        console.log(JSON.stringify(r));
        if(r.length === 0){
            div.innerHTML =
                "<h3>not delegating to anyone</h3>";
        } else {
            delegating_div_maker2(
                r, sid, noncer.id, noncer, div,
                show_posts_in_batches_of);
        };
        return(div);

    };
    async function delegating_div_maker2(
        accs, sid, aid, noncer, div, n
    ){
        if(accs.length === 0){
            return(0);
        };
        if(n === 0){
            var load_more_button =
                header_button(
                    "load more",
                    function(){
                        lowdiv.innerHTML = "";
                        delegating_div_maker2(
                            accs, sid, aid,
                            noncer, div, show_posts_in_batches_of);
                    });
            lowdiv.appendChild(load_more_button);
            return(0);
        };
        console.log(JSON.stringify(accs));
        var aid = accs[0][0];
        var bal = accs[0][1];
        var acc = await account_loader(
            noncer, sid, aid);

        var s = document.createElement("span");
        s.innerHTML = "user: "
            .concat(acc.username)
            .concat(" amount: ")
            .concat(s2c(bal).toFixed(2))
            .concat(" ")
        ;

        div.appendChild(s);

        var undelegate = header_button(
            "undelegate",
            async function(){
                //div.innerHTML = "";
                var tx = ["x", keys.pub(),
                          noncer.check(), sid, 26,
                          aid];
                var stx = keys.sign(tx);
                await rpc.signed(stx);
                delete accounts_memoized[my_nonce.id];
                var div2 = await delegating_div_maker(sid, noncer);
                clear_page();
                topdiv.appendChild(div2);
                
            });
        div.appendChild(undelegate);
        div.appendChild(br());
        
        console.log(JSON.stringify(acc));
        console.log(JSON.stringify(bal));


        return(delegating_div_maker2(
            accs.slice(1), sid, aid,
            noncer, div, n-1));
    };
    function accumulate_repeated_votes(v){
        if(v.length < 2){
            return(v);
        };
        if(!(v[0][0])){
            console.log("bad type error");
            return(0);
        }
        var a = v[0];
        v = v.slice(1);

        var b = try_arv(a, v, []);
        if(b === "no repeat"){
            return([a].concat(accumulate_repeated_votes(v)));
        } else{
            return(accumulate_repeated_votes(b));
        }
    };
    function try_arv(a, v, q){
        if(v.length === 0){
            return("no repeat");
        } else if((v[0][1] === a[1])
                //  && (v[0][3] === a[3])
                 ){
            return(q.concat([[-7, a[1],
                              a[2] + v[0][2],
                              0, 0]])
                   .concat(v.slice(1)));
        } else {
            return(try_arv(a, v.slice(1),
                           q.concat([v[0]])));
        }
    };
    function r2peer_order(r){
        //make a list of peers, along with the highest mid associated to each peer. Sort the list of peers from high to low mid.
        var r2 = [];
        r2 = r2
            .concat(r[1].slice(1))
            .concat(r[2].slice(1))
            .concat(r[3].slice(1))
            .concat(r[4].slice(1));
        var peers = {};
        r2.map(function(x){
            var from = x[1];
            var mid = x[2];
            if(!(peers[from]) ||
               (peers[from] < mid)){
                   peers[from] = mid;
            };
        });
        var pk = Object.keys(peers);
        var pk2 = pk.sort(function(a, b){
            return(peers[b] - peers[a])});
        pk2 = pk.map(function(x){
            return(parseInt(x))});
        return(pk2);
    };
    function load_dms(r){
        var received_unread =
            r[3].slice(1);
        var received_read =
            r[4].slice(1);
        var sent_unread =
            r[1].slice(1);
        var sent_read =
            r[2].slice(1);
        var db = {};
        var db_notifications = {};
        db = load_dms2(db, 0, received_unread);
        db = load_dms2(db, 1, received_read);
        db = load_dms2(db, 2, sent_unread);
        db = load_dms2(db, 3, sent_read);

        db_notifications = load_notifications(
            db_notifications, received_unread);

        return([db, db_notifications]);
    };
    function load_notifications(db, l){
        if(l.length  === 0){
            return(db);
        };
        var from = l[0][1];
        var mid = l[0][2];
        if(!(db[from])){
            db[from] = 1;
        } else {
            db[from] += 1;
        };
        return(load_notifications(db, l.slice(1)));
    };
    function load_dms2(db, type, l){
        if(l.length === 0){
            return(db);
        }
        var from = l[0][1];
        var mid = l[0][2];
        if(!(db[from])){
            db[from] = [];
        }
        db[from] = merge_post(db[from], mid, type);
        return(load_dms2(db, type, l.slice(1)));
    };
    function merge_post(l, mid, type){
        if(l.length === 0){
            return([[mid, type]]);
        } else {
            mid2 = l[0];
            if(mid2 > mid){
                return(l[0].concat(
                    merge_post(l.slice(1),
                               mid, type)));
            } else {
                return([[mid, type]].concat(l));
            };
        }
    };
    async function conversation_page(acc, dms, noncer, sid){
        clear_page();
        //var dms2 = dms.reverse();
        var dms2 = dms.sort(function(a, b){
            return(b[0] - a[0])});
        var send_div = await send_dm_div_maker(acc, noncer, sid, function(msg){
            var s = middiv.innerHTML;
            s = "<span style=\"color:#007400;\">"
                .concat(msg)
                .concat("</span><br>")
                .concat(s);
            middiv.innerHTML = s;
            unlock_dms(dms2, noncer, sid);
            console.log("todo. sending a message should delete our oldest message, if we have too many existing messages.");
        });
        topdiv.appendChild(send_div);
        var unlock_all = header_button(
            "unlock all",
            function(){
                unlock_dms(dms2, noncer, sid);
                unlock_all.innerHTML = "done";
            });
        topdiv.appendChild(unlock_all);
        await conversation_messages(acc, dms2, noncer, sid, show_posts_in_batches_of);
    };
    async function conversation_messages
    (acc, dms2, noncer, sid, n){
        if(dms2.length === 0){
            return(0);
        };
        if(n === 0){
            //todo show more button.
            var load_more_button =
                header_button(
                    "load more conversation",
                    function(){
                        lowdiv.innerHTML = "";
                        conversation_messages(
                            acc, dms2, noncer, sid,
                            show_posts_in_batches_of);
                    });
            lowdiv.appendChild(load_more_button);
            return(0);
        };
        var x = dms2[0];
        var mid = x[0];
        var type = x[1];
        if(type === 0){
            console.log("received unread");
        } else if(type === 1){
            console.log("received read");
        } else if(type === 2){
            console.log("sent unread");
        } else if(type === 3){
            console.log("sent read");
        };
        var dm = await dm_loader(mid, noncer, sid);
        if(dm === "deleted"){
            console.log("deleted message");
            return(conversation_messages(
                acc, dms2.slice(1), noncer, sid, n));
        }
        var p = document.createElement("span");
        var s = "";
        if(dm.from === noncer.id){
            p.style.color = "#007400";
        } else {
            s = 
                (s2c(dm.lockup)
                 .toFixed(0).toString())
                .concat(" - ");
        }
        if(dm.content.slice(0, 8) ===
           "[\"emsg\",") {
            var x = JSON.parse(dm.content);
            x = keys.multi_decrypt(x);
            p.innerHTML = s.concat(x);
        } else {
            p.innerHTML = s.concat(dm.content);
        }
        middiv.appendChild(p);
        middiv.appendChild(br());
            
        return(conversation_messages(
            acc, dms2.slice(1), noncer, sid, n-1));
        //todo load the 10 most recent messages,
    };
    function decode_dm(r){
            //{x, id, content, read, from, to, lockup, timestamp}
        return({id: r[1],
                content: atob(r[2]),
                read: r[3],
                from: r[4],
                to: r[5],
                lockup: r[6],
                timestamp: r[7]});
    };

    var memoized_dms = {};
    async function dm_loader
    (mid, noncer, sid, type){
        var dm = memoized_dms[mid];
        if(!(dm)){
            return(await dm_loader2(
                mid, noncer, sid));
        };
        return(dm.val);
    };
    async function dm_loader2(mid, noncer, sid){

        var tx = ["x", keys.pub(),
                  noncer.check(), sid,
                  11, mid];
        var stx = keys.sign(tx);
        var r = await rpc.signed(stx);
        if(r[0] === -6){
            return("deleted");
        }
        var dm = decode_dm(r);
        memoized_dms[mid] = {
            timestamp: timestamp(),
            val: dm};
        return(dm);
    };
    async function unlock_dms(dms, noncer, sid){
        if(dms.length === 0){
            return(0);
        };
        var mid = dms[0][0];
        var type = dms[0][1];
        if(type > 1){
            return(unlock_dms(
                dms.slice(1), noncer, sid));
        };
        var dm = await dm_loader(mid, noncer, sid);
        if(dm.lockup > 0){
            var tx = ["x", keys.pub(),
                      noncer.check(), sid,
                      12, mid, 1];
            var stx = keys.sign(tx);
            var r = await rpc.signed(stx);
            delete memoized_dms[mid];
        };
        return(unlock_dms(
            dms.slice(1), noncer, sid));
    };


    main = {
        load_account_page: load_account_page,
        clear_page: clear_page,
        topdiv: function(){return(topdiv)}
    };
})();
