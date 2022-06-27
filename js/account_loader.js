var accounts_memoized = {};
async function account_loader(
    noncer, sid, id, type){
    var acc = accounts_memoized[id];
    if((!(acc)) ||
       (type === "refresh") ||
       ((!(type === "cached")) &&
        ((timestamp() - acc.timestamp) > 60000))){
        return(await account_loader2(
            noncer, sid, id));
    };
    return(acc.val);
}

async function account_loader2(noncer, sid, id){
    if(noncer === undefined){
        //slow free version, if you have no coin-hours.
        var request = ["x", 4, id];
        var acc = await rpc.apost(request);
        if(acc === "error"){
            return("error");
        } else {
            acc = acc[1];
            return(decode_acc(id, acc));
        };
    }
    var n = noncer.check();
    var tx = ["balance", keys.pub(), n, sid, id];
    //console.log(JSON.stringify(tx));
    var stx = keys.sign(tx);
    var r = await rpc.signed(stx);
    if(r === "error"){
        delete accounts_memoized[id];
        return(["error", "account deleted"]);
    };
    var acc = decode_acc(id, r);
    accounts_memoized[id] =
        {timestamp: timestamp(),
         val: acc};
    return(acc);
};
function decode_acc(id, r){
    var data = {
        id: id,
        username: atob(r[2]),
        description: atob(r[3]),
        pubkey: atob(r[1]),
        veo: r[5],
        coins: r[5] - r[6] - r[9] - r[10],
        coin_hours: r[7],
        timestamp: timestamp()
    };
    return(data);
}

var posts_memoized = {};
async function post_loader(
    noncer, sid, id, type){
    var post = posts_memoized[id];
    if((!(post)) ||
       (type === "refresh") ||
       ((!(type === "cached")) &&
        ((timestamp() - post.timestamp) > 60000))){
        return(await post_loader2(
            noncer, sid, id));
    };
    return(post.val);
};
async function post_loader2(
    noncer, sid, id){
    if(!id){
        console.log("failing to look up post with invalid id");
        return(0);
    };
    if(noncer === undefined){
        //slow free version, if you have no coin-hours.
        var request = ["x", 3, id];
        var post = await rpc.apost(request);
        if(post === "error"){
            return("error");
        } else {
            post = post[1];
            return(decode_post(post));
        } 
    }
    var n = noncer.check();
    //lookup a post by id.
    var tx = ["x", keys.pub(), noncer.check(),
              sid, 14, id];
    console.log(JSON.stringify(tx));
    var stx = keys.sign(tx);
    var post = await rpc.signed(stx);
    if(post === "error"){
        delete posts_memoized[id];
        return(["error", "post deleted"]);
    };
    post = post[1];
    //{post, pid, text, author, timestamp, upvotes, downvotes, comments, parent}
    var post = decode_post(post);
    posts_memoized[id] =
        {timestamp: timestamp(),
         val: post};
    return(post);
};
function decode_post(post){
    //console.log(JSON.stringify(post));
    var text = atob(post[2])
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/\n/, "<br>")
        .replace(/\n/, "<br>")
        .replace(/\n/, "<br>")
        .replace(/\n/, "<br>")
        .replace(/\n/, "<br>")
        .replace(/\n/, "<br>");
    var data =
        {pid: post[1],
         text: text,
         author_id: post[3],
         server_timestamp: post[4],
         upvotes: post[5],
         downvotes: post[6],
         comments: post[7].slice(1),
         parent: post[8]};
    return(data);
};
