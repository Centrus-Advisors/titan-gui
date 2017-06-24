const purifier = require("root-require")("./server/lib/routePurifier");
const database = require("root-require")("./server/lib/database");
const { Future } = require("ramda-fantasy");
const csv = require("root-require")("./server/lib/csv");

const dbName = "database.json";

module.exports = req => {
    switch (req.method) {
    case "GET":
        return database
                .loadDb(dbName)
                .chain(csv.stringify)
                .map(content => purifier.respond.custom({ content }));
    case "POST":
        return database
                .save(dbName, req.body)
                .map(() => purifier.respond.json({ content: req.body }));
    default:
        return Future.of(
                purifier.respond.custom({
                    status: 405
                })
            );
    }
};
