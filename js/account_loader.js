async function account_loader(
    noncer, sid, id){
    if(noncer === undefined){
        //slow free version, if you have no coin-hours.
        var request = ["x", 4, id];
        var acc = await rpc.apost(request);
        acc = acc[1];
        return(decode_acc(id, acc));
    }
    var n = noncer.check();
    var tx = ["balance", keys.pub(), n, sid, id];
    console.log(tx);
    var stx = keys.sign(tx);
    var r = await rpc.signed(stx);
    return(decode_acc(id, r));
};
function decode_acc(id, r){
    var data = {
        id: id,
        username: atob(r[2]),
        description: atob(r[3]),
        pubkey: atob(r[1]),
        coins: r[5],
        coin_hours: r[7],
        timestamp: timestamp()
    };
    return(data);
}

async function post_loader(
    noncer, sid, id){
    if(noncer === undefined){
        //slow free version, if you have no coin-hours.
        var request = ["x", 3, id];
        var post = await rpc.apost(request);
        post = post[1];
        return(decode_post(post));
    }
    var n = noncer.check();
    //lookup a post by id.
    var tx = ["x", keys.pub(), noncer.check(),
              sid, 14, id];
    console.log(tx);
    var stx = keys.sign(tx);
    var post = await rpc.signed(stx);
    //{post, pid, text, author, timestamp, upvotes, downvotes, comments, parent}
    return(decode_post(post));
};
function decode_post(post){
    var data =
        {pid: post[1],
         text: atob(post[2]),
         author_id: post[3],
         server_timestamp: post[4],
         upvotes: post[5],
         downvotes: post[6],
         comments: post[7].slice(1),
         parent: post[8]};
    return(data);
};
