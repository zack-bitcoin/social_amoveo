var rpc = (function() {
    function url(port, ip) {
        return "http://".concat(ip).concat(":").
            concat(port.toString()).concat("/"); }
    async function main2(cmd, ip, port) {
        if (ip == undefined){
            //ip = "159.89.87.58";
            //ip = "0.0.0.0";
            ip = document.URL.split("/")[2].split(":")[0];
        }
        if (port == undefined){
            //port = get_port();
            port = 8095;
        }
        var u = url(port, ip);
        return atalk(cmd, u);//use up to 10 seconds for this request
    }
    async function signed(cmd) {
        ip = document.URL.split("/")[2].split(":")[0];
        port = 8095;
        var u = url(port, ip).concat("signed/");
        return atalk(cmd, u);
    };
    async function atalk(cmd, u) {
        return new Promise(function(resolve, reject){
            let xmlhttp = new XMLHttpRequest();
            xmlhttp.open("POST", u);
            xmlhttp.onload = function() {
                if (this.status >= 200 && this.status < 300) {
                    resolve(JSON.parse(xmlhttp.response)[1]);
                } else {
                    reject({
                        status: this.status,
                        statusText: xmlhttp.statusText
                    });
                }
            };
            xmlhttp.onerror = function () {
                reject({
                    status: this.status,
                    statusText: xmlhttp.statusText
                });
            };
            xmlhttp.send(JSON.stringify(cmd));
        });
    };
    return {
        apost: main2,
        signed: signed
    };
})();
