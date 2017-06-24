const csvparse = require("csv-parse");
const csvstringify = require("csv-stringify");
const fs = require("fs");
const { Future } = require("ramda-fantasy");
const { encodeTable, decodeTable } = require("./schemas");

const readFile = filePath =>
    Future((reject, resolve) =>
        fs.readFile(filePath, (err, data) => (err ? reject(err) : resolve(data)))
    );

const writeFile = filePath => fileContent =>
    Future((reject, resolve) => {
        fs.writeFile(filePath, fileContent, err => (err ? reject(err) : resolve()));
    });

const parseCsv = input =>
    Future((reject, resolve) =>
        csvparse(input, (err, output) => (err ? reject(err) : resolve(output)))
    );

const stringifyCsv = input =>
    Future((reject, resolve) => {
        csvstringify(input, (err, output) => (err ? reject(err) : resolve(output)));
    });

const getAll = (schema, filePath) => readFile(filePath).chain(parseCsv).chain(decodeTable(schema));

const save = (schema, filePath, newRow) =>
    getAll(schema, filePath)
        .map(rows => rows.concat([newRow]))
        .chain(encodeTable(schema))
        .chain(stringifyCsv)
        .chain(writeFile(filePath));

module.exports = {
    getAll,
    save
};
