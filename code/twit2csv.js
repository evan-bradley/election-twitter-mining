const fs = require("fs");
const readline = require('readline');
const emojiRegex = require('emoji-regex');
const emojiDict = require("emoji-dictionary");
const emojiList = require("emojis-list");
const emojiWords = require("emojis-keywords");
const trimEmoji = require("trim-emoji");
const gemoji = require("gemoji");
const emojiHash = {};
const tcoRegex = /^((http[s]?|ftp):\/)?\/?([^:\/\s]+)((\/\w+)*\/)([\w\-\.]+
[^#?\s]+)(.*)?(#[\w\-]+)?$/g;
let statuses;
let startFile = parseInt(process.argv[2], 10);
let endFile = parseInt(process.argv[3], 10);
let date = 0;
let stream;
let buf = "";
let i = startFile;
let lineCount = 0;

const hashtagList = [
    "election2016", "notmypresident", "electionnight",
    "trump", "clinton", "maga",
    "makeamericagreatagain", "americafirst", "draintheswamp"
    ];

const emojiStringToArray = function (str) {
    split = str.split(/([\uD800-\uDBFF][\uDC00-\uDFFF])/);
    arr = [];
    for (let i = 0; i < split.length; i++) {
        char = split[i];
        if (char !== "") {
            arr.push(char);
        }
    }
    return arr;
};

// Set csv.hashtags = each hashtag shared by this tweet and the list.
// csv will need to be an object whose write method is transparent to
// the number of files it is writing to. The assumption being that a
// tweet with two hashtags will end up in both sets.
// Probably like this:
// csv.clear()
// csv.add(hashtag)
// csv.write(data)
const csv = (function() {
    const writeStreams = {};
    let current = {};

    hashtagList.forEach((hashtag) => {
        writeStreams[hashtag] = fs.createWriteStream(__dirname + "/data-" + hashtag + ".csv", {
            flags: "a",
            encoding: "utf8",
            mode: 0744
        });
        writeStreams[hashtag].write("time,hashtags,emojis,text,retweet,bio,location\n");
    });

    function clear() {
        current = {};
    }

    function add(tag) {
        current[tag] = writeStreams[tag];
    }

    function write(data) {
        Object.keys(current).forEach((tag) => {
            current[tag].write(data);
        });
    }

    function close() {
        Object.keys(writeStreams).forEach((tag) => {
            writeStreams[tag].end();
        });
    }

    CSV = {
        clear: clear,
        add: add,
        write: write,
        close: close
    };

    return CSV;

})();

function convertToCSV(data) {
    if(data.lang !== "en") {
        return;
    }

    let tweet = data;
    if(typeof tweet.extended_tweet !== "undefined") {
        tweet.text = tweet.extended_tweet.full_text;
        tweet.entities = tweet.extended_tweet.entities;
    }
    if(typeof tweet.retweeted_status !== "undefined") {
        if(typeof tweet.retweeted_status.extended_tweet !== "undefined") {
            tweet.text = tweet.retweeted_status.extended_tweet.full_text;
            tweet.entities = tweet.retweeted_status.extended_tweet.entities;
        } else {
            tweet.text = tweet.retweeted_status.text;
            tweet.entities = tweet.retweeted_status.entities;
        }
    }
    let time = tweet.created_at.split(" ");
    let month = time[1];
    if(month === "Nov") {
        month = "11";
    } else if(month === "Dec") {
        month = "12";
    }

    let tweetInfo = "\"2016-" + month + "-" + time[2] + " " + time[3] + "\",";
    let text = tweet.text.replace(/(?:\r\n|\r|\n)/g, " ");
    //text.replace(/[ ]{2,}/, "");
    tweetInfo += "";
    tweet.entities.hashtags.forEach((hashtag) => {
        text = text.replace("#" + hashtag.text, "");
        tweetInfo += hashtag.text + " ";
        csv.clear();
        hashtagList.forEach((tag) => {
            if(hashtag.text.toLowerCase() === tag) {
                csv.add(tag);
            }
        });
    });
    if(tweet.entities.hashtags.length > 0) {
        tweetInfo = tweetInfo.substring(0, tweetInfo.length - 1); // Remove extra space.
    }
    tweetInfo += ",";

    let textAtoms = text.split(" ");
    tweetInfo += "";
    let emojiTable = {};
    textAtoms.forEach((atom) => {
        if(emojiRegex().test(atom)) {
            text = text.replace(atom, "");
            let emojis = emojiStringToArray(atom);
            //console.log(emojis);
            emojis.forEach((emoji) => {
                if(emojiRegex().test(emoji)) {
                    //let name = emojiDict.getName(emoji);
                    //console.log(emoji);
                    if(gemoji.unicode[emoji] !== undefined) {
                        let name = gemoji.unicode[emoji].name;
                        if(typeof emojiTable[name] === "undefined") {
                            if(name !== "undefined") {
                                //console.log(emoji, name);
                                tweetInfo += name + " ";
                                emojiTable[name] = 1;
                            }
                        }
                    }
                }
            });
        }
    });

    if(emojiTable.length > 0) {
        tweetInfo = tweetInfo.substring(0, tweetInfo.length - 1); // Remove extra space.
    } else {
        tweetInfo += "\"\"";
    }

    tweetInfo += ",";
    tweet.entities.urls.forEach((url) => {
        text = text.replace(url.url, "");
    });
    
    text = text.replace(/"/g, "");
    text = text.replace(/@/g, "");
    text = text.replace(/,/g, "");
    text = text.replace(/&amp;/g, "");
    //text = text.replace(/'/g, "");
    text = text.replace(/https:\/\/t.co\/[a-zA-Z0-9]{0,}/g, "");
    text = text.replace(/\./g, "");
    text = text.replace(/\//g, "");
    text = text.replace(/\s\s+/g, " ");
    tweetInfo +=  "" + text + ",";

    if(tweet.retweeted_status !== undefined) {
        tweetInfo += 'yes,';
    } else {
        tweetInfo += 'no,';
    }

    if(tweet.user.description !== null) {
        let bio = tweet.user.description.replace(/(?:\r\n|\r|\n)/g, " ");
        bio = bio.replace(/"/g, "");
        bio = bio.replace(/@/g, "");
        bio = bio.replace(/\./g, "");
        bio = bio.replace(/&amp;/g, "");
        bio = bio.replace(/,/g, "");
        //bio = bio.replace(/'/g, "");
        bio = bio.replace(/\//g, "");
        bio = bio.replace(/\s\s+/g, " ");
        tweetInfo += "" + bio + "";
    } else {
        tweetInfo += "\"\"";
    }
    tweetInfo += ",";

    if(tweet.user.location !== null) {
        let loc = tweet.user.location.replace(/(?:\r\n|\r|\n)/g, " ");
        loc = loc.replace(/"/g, "");
        loc = loc.replace(/@/g, "");
        loc = loc.replace(/\./g, "");
        loc = loc.replace(/,/g, "");
        loc = loc.replace(/&amp;/g, "");
        tweetInfo += "" + loc + "";
    } else {
        tweetInfo += "\"\"";
    }
    tweetInfo += "\n";
    csv.write(tweetInfo);
}

function createReader() {
    for(let i = 0; i < emojiList.length; i++) {
        emojiHash[emojiList[i]] = emojiWords[i];
    }
    readline.createInterface({
        input: fs.createReadStream(__dirname + "/data-" + i + ".json", {flags: 'r', encoding: 'utf-8'}),
        terminal: false
    }).on('line', function(line) {
        lineCount++;
        if(line.substring(0,3) !== "{\"c") {
            return;
        }

        try {
            if(line.substring(line.length - 1, line.length) === ",") {
                convertToCSV(JSON.parse(line.substr(0,line.length-1)));
            } else {
                convertToCSV(JSON.parse(line));
            }
        } catch(e) {
            console.log("On line " + lineCount);
            console.log(e);
        }
    }).on("close", () => {
        console.log("Finished reading file data-" + i + ".json");
        lineCount = 0;
        i++;
        if(i <= endFile) {
            createReader();
        }
    });
}

createReader();
