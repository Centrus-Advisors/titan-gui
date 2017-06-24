const purifier = require("root-require")("./server/lib/routePurifier");
const database = require("root-require")("./server/lib/database");
const { Future } = require("ramda-fantasy");
const csv = require("root-require")("./server/lib/csv");

const dbName = "database.json";

const filterFromDate = fromDateString => db => {
    if (!fromDateString) {
        return db;
    }
    const fromDate = new Date(fromDateString);
    return db.filter(v => new Date(v.createdAt) >= fromDate);
};

const filterToDate = toDateString => db => {
    if (!toDateString) {
        return db;
    }
    const toDate = new Date(toDateString);
    return db.filter(v => new Date(v.createdAt) < toDate);
};

module.exports = req => {
    switch (req.method) {
    case "GET":
        return database
                .loadDb(dbName)
                .map(filterFromDate(req.query.fromDate))
                .map(filterToDate(req.query.toDate))
                .chain(csv.stringify)
                .map(content => content || "No Content")
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
