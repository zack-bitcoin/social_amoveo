function encryption_main() {
    function aes_ctr(key) {
        return(new aesjs.ModeOfOperation.ctr(key, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]));
    };
    function bin_enc(key,bin){
        var ac = aes_ctr(key);
        return Array.from(ac.encrypt(bin));
    };
    function bin_dec(key, bin){
        var ac = aes_ctr(key);
        return Array.from(ac.decrypt(bin));
    };
    function shared(key, pub) {
	pub = pub.getPublic();
        var k = key.derive(pub);
        return hex2array(k.toString(16));
    }
    function hex2array(x) {
        return string_to_array(fromHex(x));
    }
    function multi_send(m, to_pubs, fromKey){
        var tos = to_pubs.map(function(x){
            return(keys.ec().keyFromPublic(
                toHex(atob(x)), "hex"));
        });
        var from_pub = btoa(fromHex(
            fromKey.getPublic("hex")));
        var entropy = shared(
            keys.make(), fromKey);
        var newkey = keys.make();
        var eph_pub = hex2array(
            newkey.getPublic("hex"));
        var sig = btoa(array_to_string(sign(
            btoa(array_to_string(eph_pub)),
            fromKey)));
        var msg = ["msg", sig, m, btoa(from_pub)];

        var shared_secrets = tos.map(function(x){
            return(shared(newkey, x));
        });
        var encoded_entropy = shared_secrets.map(
            function(x){
                var r = [];
                for(var i = 0; i<32; i++){
                    r[i] = (x[i] ^ entropy[i]);
                }
                return(btoa(array_to_string(r)));
            });
        var emsg = bin_enc(
            entropy, string_to_array(
                JSON.stringify(msg)));
        return ["emsg", btoa(array_to_string(eph_pub)), to_pubs, encoded_entropy, btoa(array_to_string(emsg))];
    };
    function multi_get(emsg, my_key) {
        var eph_pub = atob(emsg[1]);
        var eph_key = keys.ec().keyFromPublic(toHex(eph_pub), 'hex');
        var ss = shared(my_key, eph_key);
        var to_pubs = emsg[2];
        var encoded_entropy = emsg[3];
        var encrypted = emsg[4];
        var encoded_entropy =
            string_to_array(atob(entropy_grab(
                encoded_entropy, to_pubs,
                btoa(fromHex(my_key.getPublic(
                    "hex"))))));
        var ss = shared(my_key, eph_key);
        var entropy = [];
        for(var i = 0; i<32; i++){
            entropy[i] = (encoded_entropy[i] ^
                          ss[i]);
        };
        var msg = JSON.parse(array_to_string(bin_dec(entropy, string_to_array(atob(emsg[4])))));
        var fromkey = keys.ec().keyFromPublic(toHex(atob(atob(msg[3]))), 'hex');
        var b = verify(emsg[1], msg[1], fromkey);
        if (b) { return msg[2]
        } else { throw("encryption get error");
               };
        //console.log(JSON.stringify([btoa(fromHex(my_key.getPublic("hex"))), to_pubs]));
    };
    function entropy_grab(es, tos, me){
        if(tos[0] === me){
            return(es[0]);
        };
        return(entropy_grab(
            es.slice(1), tos.slice(1), me));
    }
    function send(m, to_pub, fromkey) {
        var to = keys.ec().keyFromPublic(
            toHex(atob(to_pub)), "hex");
        var from_pub = btoa(fromHex(fromkey.getPublic("hex")));
        var newkey = keys.make();
        var eph_pub = hex2array(newkey.getPublic("hex"));
        //var eph_priv = hex2array(newkey.getPrivate("hex"));
	console.log("signing on ");
	console.log(btoa(array_to_string(eph_pub)));
	console.log("signing with ");
	console.log(fromkey.getPublic("hex"));
        var sig = btoa(array_to_string(sign(
            btoa(array_to_string(eph_pub)),
            fromkey)));
        var msg = ["msg", sig, m, btoa(from_pub)];
        var ss = shared(newkey, to);
	console.log("send message ");
	console.log(JSON.stringify(msg));
        var emsg = bin_enc(ss, string_to_array(JSON.stringify(msg)));
        return ["emsg", btoa(array_to_string(eph_pub)), btoa(array_to_string(emsg))];
    };
    function get(emsg, my_key) {
        var eph_pub = atob(emsg[1]);
	//console.log("get eph pub ");
	//console.log(eph_pub);
	//console.log(atob(emsg[1]));
        var eph_key = keys.ec().keyFromPublic(toHex(eph_pub), 'hex');
        var ss = shared(my_key, eph_key);
        var msg = JSON.parse(array_to_string(bin_dec(ss, string_to_array(atob(emsg[2])))));
	console.log("msg");
	console.log(JSON.stringify(msg));
        var fromkey = keys.ec().keyFromPublic(toHex(atob(atob(msg[3]))), 'hex');
	console.log("verify message received ");
	console.log(emsg[1]);
        var b = verify(emsg[1], msg[1], fromkey);
        if (b) { return msg[2]
        } else { throw("encryption get error");
        }
    };
    function assert_eq(x, y) {
        if (!(JSON.stringify(x) == JSON.stringify(y))) {
            console.log("failed assert_eq");
            console.log(JSON.stringify(x));
            console.log(JSON.stringify(y));
            throw("failed assert_eq");
        };
    }
    function test() {
        var pub1_64 = "BNFsD42eXjwHd4PyP4lODu+aybYjmVJF0bA0UYNcJ2/cELBl5Z6IA639jUl1km+8YAD3aL3of+SqLI8emuhsa2c=";
        var priv1_64 = "wruxx99+2hK4cT+j1SkJhV6VzBxgbl11iHxnq6ghASA=";
        var pub2_64 = "BA7HtKYPIvUwQjmUqNQ0UMsxHu+KtISveg45Jl+tl/y6OMgtyC3rE4/YrEHLtTprCPsxcus5CbhmlWo9IDKfnzo=";
        var priv2_64 = "4a6E2IK3hhhP2dK8xGYaUqg23Fk/n/Ms2VuORKC5Xvo=";
        var key1 = keys.ec().keyFromPrivate(toHex(atob(priv1_64)), "hex");
        var key2 = keys.ec().keyFromPrivate(toHex(atob(priv2_64)), "hex");
        var sm = send([-6, 1, 2, 3], btoa(fromHex(key2.getPublic("hex"))), key1);
        var sm2 = ["emsg","QlBKTURaYTZHTEdBc2FuM2Y5c3pjR0tja29KblVoLyt3NE92c21kT0hDa3pEcjlESlRDVmhHTFlqNWdINnhSYmszSlFkbzdRZ2ttZHByQVlVbDBiZkpBPQ==","ejFEWmM0TDEyY1g3Q3F0Q0ZZK3NETHptTld4UFhTeFpKSVAwUk9BZkZqWFVLRGUwQkdDMGl2ZFQ2Rk9IT0ZtTHJPSGRwbit0bWpKYzNjYzlKdEFLQW5aY3kxRVBmekNTSTRONWN4RDhFbzg3dEdWSUwzKytSbDdaZ1JWZE5STUp5MEhrUDJIZmVmSWZaQjV2VW9YTWhuYytKSVB3M0hmVFBlbjFUM29qdmxsanNBM3l6OC8vNGU5eWpKeDIwV0pHMnBFV3BmWEJYZDVJZklmeG53QWJTUGNhMTRGNE8rN1hYRjA0bks2U0ZQckZkYzgrUGxFZUZqbmxKRG9YOTMwenE3MHcrMDZjeElMV096RDE2bCtlSldZbzg5OGhSWUxHUlJvRTFGSE5XcjV3WDBCeWNjL3creWhCbDl3dkpsckM="];//This was generated by the erlang version of encrypter.
        //console.log("sms 2 are ");
        //console.log(JSON.stringify(sm));
        //console.log(JSON.stringify(sm2));
        var got = get(sm, key2);
        console.log("got");
        console.log(JSON.stringify(got));
        test2();
    };
    function test2() {
        var key = hash([1]);
        console.log(key);//same as hash:doit(<<1>>) from erlang.
        var textBytes = [1,2,3];
        var eb = bin_enc(key, textBytes);
        assert_eq(eb, [100, 131, 24]);
        assert_eq(bin_dec(key, eb), [1, 2, 3]);
        var fromKey = keys.make();
        var toKey = keys.make();
        var message = [-6, 1,2,3];
        var sm = send(message, btoa(fromHex(toKey.getPublic("hex"))), fromKey);
        assert_eq(get(sm, toKey), message);
        var masterPub64 = "BLDdkEzI6L8qmIFcSdnH5pfNAjEU11S9pHXFzY4U0JMgfvIMnwMxDOA85t6DKArhzbPJ1QaNBFHO7nRguf3El3I=";
        var master = keys.ec().keyFromPublic(toHex(atob(masterPub64)), 'hex');
        console.log("encryption test passed.");
    }
    function test_shared() {
        var Key1 = keys.make();
        var Key2 = keys.make();
	//var ss1 = shared(Key1, Key2.getPublic());
	var ss1 = shared(Key1, Key2);
	var ss2 = shared(Key2, Key1);
	console.log(JSON.stringify(ss1));
	console.log(JSON.stringify(ss2));
	assert_eq(ss1, ss2);
	return "success";
    }
    //test();
    function test3(){
        var key1 = keys.make();
        var key2 = keys.make();
        var to1 = btoa(fromHex(
            key1.getPublic("hex")));
        var to2 = btoa(fromHex(
            key2.getPublic("hex")));
        var fromKey = keys.make();
        var emsg = multi_send(
            "hi", [to1, to2], fromKey);
        return([multi_get(emsg, key1),
                multi_get(emsg, key2),emsg]);
    };
    return {get: get, send: send, test: test, test2: test2, test_shared: test_shared, test3:test3};
}
var encryption_object = encryption_main();



var secrets_object = (function () {
    var db = {};
    function add(code, ss, amount){
	db[code] = [ss, amount];
    }
    function dump() {
	db = {};
    }
    function read(code) {
	return db[code];
    }
    function check() {
	return db;
    }
    return {add: add, dump: dump, read: read, check: check};
})();
