async function send_dm_div_maker(to, noncer, sid, refresh){
    var div = document.createElement("div");

    var title = document.createElement("h3");
    title.innerHTML = "send a new message to "
        .concat(to.username);
    div.appendChild(title);
    var message_area =
        document.createElement("textarea");
    message_area.rows = 6;
    message_area.cols = 
        Math.min(window.innerWidth / 10,
                 45);
    div.appendChild(message_area);

    var send_message_button =
        button_maker2(
            "send message",
            async function(){
                var msg = message_area.value;
                /*
                var keys1 = keys_function1(false);
                var keys2 = keys_function1(false);
                var S2 = keys1.raw_sign(
                    serialize(btoa(msg)));
                var Pub1 = keys1.pub();
                var S1 = keys.raw_sign(serialize(
                    btoa(Pub1)));
                var Message2 = [
                    keys.pub(),
                    Pub1,
                    S1, S2];
                var emsg = keys2.encrypt(
                    Message2, to.pubkey);
                */
                var tx = ["x", keys.pub(),
                          noncer.check(),
                          sid,
                          10, to.id, btoa(msg),
                        //00012345678
                          20000000000];
                console.log(tx);
                var stx = keys.sign(tx);
                console.log(stx);
                var r = await rpc.signed(stx);
                console.log("sent a message");
                message_area.value = "";
                if(refresh){
                    refresh(msg);
                };
            });

    div.appendChild(br());
    div.appendChild(send_message_button);

    return(div);
};
