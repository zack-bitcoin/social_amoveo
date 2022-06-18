(async function(){
    var div = document.createElement("div");
    document.body.appendChild(div);

    var show_posts_in_batches_of = 10;

    function header_button(text, f){
        var x = document.createElement("span");
        x.innerHTML = text;
        x.style.color = "blue";
        x.onclick = function(){
            f();
        };
        return(x);
    }
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
    //todo. a tab for the most hated posts.
    
    
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
                                    



    var my_nonce;

    async function refresh_my_page(){
        my_account_div.innerHTML = "";
        my_posts_div.innerHTML = "";
        account_div_maker(
            keys.pub(), my_nonce.id,
            my_nonce, sid, my_account_div);
        var pids =
            await rpc.apost(["top", my_nonce.id]);
        console.log(pids);
        pids = pids.slice(1);
        posts_div_maker(
            pids,//.map(function(x){return(x[1])}),
            my_nonce, sid, my_posts_div,
            false, show_posts_in_batches_of);
    };

    keys.update_balance_callback(async function(){
        if(my_nonce === undefined){
            my_nonce =
                await nonce_builder(keys.pub());
        };
        refresh_my_page();
        your_account_button.click();
    })

    async function reload_top_posts(){
        top_posts_div.innerHTML = "";
        var top_pids =
            await rpc.apost(["top"]);
        top_pids = top_pids.slice(1);
        console.log(JSON.stringify(top_pids));
        posts_div_maker(
            top_pids,//.map(function(x){return(x[1])}),
            my_nonce, sid, top_posts_div,
            true, show_posts_in_batches_of);
        topdiv.appendChild(top_posts_div);
    };

    async function posts_div_maker(
        pids, noncer, sid, d, show_author, n)
    {
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
        console.log(JSON.stringify(pids));
        //{post_id, timestamp, upvotes, downvotes}
        var id = pids[0][1];
        //var id = pids[0];
        //console.log(id);
        var post = await post_loader(
            noncer, sid, id);
        //console.log(post);
        var acc =
            await account_loader(
                noncer, sid, post.author_id);
        var author_name = acc.username;
        var s = "";
        if(show_author){
            var author_link = header_button(
                "author: " .concat(acc.username),
                async function(){
                    load_account_page(
                        acc, noncer, sid)
                })

            //var author_link = load_account_page(
            //    acc, noncer, sid);
            d.appendChild(author_link);
            s = s
                //.concat("author's balance: ")
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
            .concat(post.upvotes)
            .concat(" downvotes: ")
            .concat(post.downvotes)
            .concat("<br/>")
            .concat("<br/>");
        var post_p = document.createElement("span");
        post_p.onclick = function(){
            load_post_page(post, noncer, sid);
            //console.log("clicked on post ".concat(post.pid));
            //console.log("should load page for this post");
        };
        post_p.innerHTML = s;
        d.appendChild(post_p);

        /*
        if(post.comments.length > 0){
            var view_comments_button =
                button_maker2(
                    "view comments",
                    function(){
                        //load posts post.comments
                        console.log("load comments");
                        return(0);
                    });
            d.appendChild(view_comments_button);
            //console.log(r);
        };
        */
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
            pids,//.map(function(x){return(x[1])}),
                     noncer, sid,
            this_account_posts_div,
            false, show_posts_in_batches_of);
        clear_page();
        topdiv.appendChild(this_account_div);
        topdiv.appendChild(this_account_posts_div);
    };
    function load_post_page(post, noncer, sid){
        //pid, text, author_id, timestamp,
        //upvotes, downvotes, comments, parent

        //give a tool for upvoting and downvoting.
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
            make_comment_div.appendChild(br());
        };

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
                    console.log(r);
                    comment_text.value = "";
                    //we should probably reload the comments on this page?
                    console.log("reload comments on this page. todo.");
                    var post2 = await post_loader(
                        noncer, sid, post.pid);
                    load_post_page(post2, noncer, sid);
                });
        make_comment_div.appendChild(comment_button);

        //todo. time when this was posted.

        //an interface for making comments
        clear_page();
        topdiv.appendChild(post_div);
        topdiv.appendChild(make_comment_div);
        topdiv.appendChild(comments_div);
       
    }
    //const urlParams = new URLSearchParams(window.location.search);
    //var pubkey = urlParams.get('pubkey');
    //pubkey = pubkey.replace(/\ /g, "+");


})();
