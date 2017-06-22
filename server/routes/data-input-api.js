const purifier = require("root-require")("./server/lib/routePurifier");
const database = require("root-require")("./server/lib/database");
const { TYPES, decodeTable, encodeTable } = require("root-require")("./server/lib/schemas");

const TABLESCHEMA = [
    { name: "Name", type: TYPES.STRING(true, 50) },
    { name: "Salary", type: TYPES.FLOAT(true) }
];

const tableName = "untitled.csv";

const getAllRows = () =>
    database.getAll(TABLESCHEMA, tableName).map(content => purifier.respond.json({ content }));

module.exports = req => {
    switch (req.method) {
    case "GET":
        return getAllRows();
        // case "POST":
        //     return;
    default:
        return purifier.respond.custom({
            status: 405
        });
    }
};
