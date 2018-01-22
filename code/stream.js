const fs = require('fs');
const Twit = require("twit");
const client = new Twit({});
let data;
let filename = "data";
let filepath = __dirname + "/" + filename;
let exists = true;
let lines = 0;
let num = -1;

function createStream() {
    data = fs.createWriteStream(filename + "-" + num + ".json", {
        flags: "w",
        encoding: "utf8",
        mode: 0744
    });
    data.on("error", (error) => {
        console.log("ERROR: " + error);
        num++;
        createStream();
    });
    data.write("{ \"json\": [\n");
}

function listenForData() {
    const stream = client.stream('statuses/filter', { track: tags });

    stream.on('tweet', function (tweet) {
        text = JSON.stringify(tweet);
        if(lines > 50000) {
            data.write("\n]}");
            data.end();
            num++;
            createStream();
            lines = 0;
        } else if(lines > 0) {
            data.write(",\n"); // New line.
        }
        data.write(text);
        lines++;
    });
}

while(exists) {
    num++;
    try {
        fs.statSync(filepath + "-" + num + '.json');
    } catch(err) {
        if(err.code == 'ENOENT') {
            // file does not exist
            createStream();
            exists = false;
        }
    }
}

tags = ["#Election2016", "#NotMyPresident", "#ElectionNight",
        "#ElectionFinalThoughts", "#FuckTrump", "#Trump", "#Clinton",
        "#MAGA", "#MakeAmericaGreatAgain", "#AmericaFirst", "#draintheswamp"];
listenForData();
