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
    function span_dash(){
        var x = document.createElement("span");
        x.innerHTML = " - ";
        return(x);
    };
    function clear_page(){
        topdiv.innerHTML = "";
        middiv.innerHTML = "";
        lowdiv.innerHTML = "";
    };

    var login_button =
        header_button(
            "login ",
            function(){
                clear_page();
                topdiv.appendChild(keys.div);
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
            "posts from who you follow",
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
                console.log(JSON.stringify(r));
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
                console.log(JSON.stringify(posts));
                
                //r = r.map(function(x){
                //    return([-1, x, 0,0,0])});
                posts_div_maker(
                    posts, my_nonce, sid,
                    following_posts_div,
                    true,
                    show_posts_in_batches_of);
                topdiv.appendChild(
                    following_posts_div);
            });
    
    div.appendChild(following_posts);
    
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

    var post_text = document.createElement("textarea");
    post_text.rows = 4;
    post_text.cols = window.innerWidth / 10;
    make_post_div.appendChild(post_text);
    
    var post_button =
        button_maker2(
            "make post",
            async function(){
                var tx =
                    ["x", keys.pub(),
                     my_nonce.check(), sid, 4,
                     btoa(post_text.value)];
                var stx = keys.sign(tx);
                var r = await rpc.signed(stx);
                refresh_my_page();
                console.log(r);
                post_text.innerHTML = "";
            });
    make_post_div.appendChild(br());
    make_post_div.appendChild(post_button);


    //const urlParams = new URLSearchParams(window.location.search);
    //var pubkey = urlParams.get('pubkey');
    //pubkey = pubkey.replace(/\ /g, "+");


    var my_nonce;

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
                });
        my_account_div.appendChild(
            update_title_button);
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
            "veo: ".concat(
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
        var pids =
            await rpc.apost(["top", my_nonce.id]);
        pids = pids.slice(1);
        posts_div_maker(
            pids,
            my_nonce, sid, my_posts_div,
            false, show_posts_in_batches_of);
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
    })

    async function reload_top_posts(){
        top_posts_div.innerHTML = "";
        var top_pids =
            await rpc.apost(["top"]);
        top_pids = top_pids.slice(1);
        console.log(JSON.stringify(top_pids));
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
        var post;
        if(pids[0][0] === "post"){
            post = decode_post(pids[0]);
        } else {
            post = await post_loader(
                noncer, sid, id);
        }
        var acc =
            await account_loader(
                noncer, sid, post.author_id);
        //console.log([noncer.id, post.author_id]);
        var author_name = acc.username;
        var s = "";
        if(show_author){
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
                .concat(s2c(acc.coins))
                .concat("<br/>");
        }
        s = ""
            .concat(post.text)
            .concat("<br/>")
            .concat(s)
            //.concat("posted at: ")
            //.concat(post.server_timestamp)
            //.concat("<br/>")
            .concat("upvotes: ")
            .concat(s2c(post.upvotes))
            .concat(" downvotes: ")
            .concat(s2c(post.downvotes));
        var post_p = document.createElement("span");
        post_p.onclick = function(){
            load_post_page(post, noncer, sid);
        };
        post_p.innerHTML = s;
        d.appendChild(post_p);

        if(noncer.id === post.author_id){
            //todo. this is your post, so give a button for deleting it.
            console.log("make delete post button");
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
                            console.log(JSON.stringify(tx));
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
    function load_post_page(post, noncer, sid){

        var post_div =
            document.createElement("div");
        var make_comment_div =
            document.createElement("div");
        var comments_div =
            document.createElement("div");

        posts_div_maker([[-1, post.pid, 0, 0, 0]], noncer, sid,
                        post_div, true, 2);
        posts_div_maker(post.comments.map(
            function(x){return([-1, x, 0, 0, 0])}),
                        noncer, sid,
                        comments_div, true, show_posts_in_batches_of);
        if(post.parent === 0){
            //no parent to display
        } else {
            var parent_button =
                header_button(
                    "view comment's parent",
                    async function(){
                        parent_post =
                            await post_loader(
                                noncer, sid,
                                post.parent);
                        load_post_page(
                            parent_post,
                            noncer, sid);
                    });
            make_comment_div.appendChild(
                parent_button);
            make_comment_div.appendChild(
                span_dash());
        };

        var upvote_button =
            header_button(
                "upvote",
                async function(){
                    await vote(post.pid, noncer, sid, 1);
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
        comment_text.cols = window.innerWidth / 10;
        make_comment_div.appendChild(comment_text);

        var comment_button =
            button_maker2(
                "make comment",
                async function(){
                    var tx = [
                        "x", keys.pub(),
                        my_nonce.check(),
                        sid, 5,
                        btoa(comment_text.value),
                        post.pid];
                    var stx = keys.sign(tx);
                    var r = await rpc.signed(stx);
                    //console.log(r);
                    comment_text.value = "";
                    var post2 = await post_loader(
                        noncer, sid, post.pid);
                    load_post_page(post2, noncer, sid);
                });
        make_comment_div.appendChild(comment_button);
        //todo. time when this was posted.
        clear_page();
        topdiv.appendChild(post_div);
        topdiv.appendChild(make_comment_div);
        topdiv.appendChild(comments_div);
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
        console.log(JSON.stringify(stx));
        console.log(JSON.stringify(stx2));
        console.log(JSON.stringify(stx3));
        await rpc.signed(stx);
        await rpc.signed(stx2);
        await rpc.signed(stx3);
    };
    async function following_div_maker(
        sid, id, noncer){
        //todo. who this account follows.
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
            following_div_maker2(r, sid, id, noncer, div, show_posts_in_batches_of);
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
})();
