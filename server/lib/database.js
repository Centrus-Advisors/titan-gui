const fs = require("fs");
const { Future } = require("ramda-fantasy");
const { curry } = require("ramda");

const readFile = filePath =>
    Future((reject, resolve) =>
        fs.readFile(
            filePath,
            (err, data) => (err ? reject(err) : resolve(data))
        )
    );

const writeFile = filePath => fileContent =>
    Future((reject, resolve) => {
        fs.writeFile(
            filePath,
            fileContent,
            err => (err ? reject(err) : resolve())
        );
    });

// Loads the entire database
const loadDb = dbName => readFile(dbName).map(JSON.parse);

// Overrides the entire database
const writeDb = curry((dbName, dbContent) =>
    writeFile(dbName)(JSON.stringify(dbContent, null, 2))
);

const save = curry((dbName, record) => {
    const newRecord = Object.assign({}, record, { createdAt: new Date() });

    return loadDb(dbName)
        .map(db => db.concat([newRecord]))
        .chain(writeDb(dbName));
});

module.exports = {
    loadDb,
    save
};
