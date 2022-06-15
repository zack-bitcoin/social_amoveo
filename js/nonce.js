/*
for rememering your id in the server's database, and the nonce for making signed api requests.
 */

async function nonce_builder(pub){
    var id = await rpc.apost(["x", 2, pub]);
    id = id[1];
    console.log("id is ");
    console.log(id);
    var nonce = await rpc.apost(["x", 1, id]);
    nonce = nonce+1;
    function check(){
        nonce = nonce + 1;
        return(nonce);
    }
    return({id: id,
            check: check});
};
