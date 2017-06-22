const parse = require("csv-parse");
const fs = require("fs");
const { Future, Either } = require("ramda-fantasy");
const { traverse } = require("ramda");

/**
 * You first need to create a formatting function to pad numbers to two digits…
 **/
function twoDigits(d) {
    if (d >= 0 && d < 10) return `0${d.toString()}`;
    if (d > -10 && d < 0) return `-0${(-1 * d).toString()}`;
    return d.toString();
}

/**
 * …and then create the method to output the date string as desired.
 * Some people hate using prototypes this way, but if you are going
 * to apply this to more than one Date object, having it as a prototype
 * makes sense.
 **/
const toSQLTimestamp = date =>
    `${date.getUTCFullYear()}-${twoDigits(1 + date.getUTCMonth())}-${twoDigits(
        date.getUTCDate()
    )} ${twoDigits(date.getUTCHours())}:${twoDigits(date.getUTCMinutes())}:${twoDigits(
        date.getUTCSeconds()
    )}`;

const toSQLDate = date =>
    `${date.getUTCFullYear()}-${twoDigits(1 + date.getUTCMonth())}-${twoDigits(date.getUTCDate())}`;

// ============================================================================

// Decode
const TYPES = {
    INT: required => ({
        encode: val => {
            if (typeof val !== "number") {
                return required ? Either.Left("Int value required and not set.") : Either.Right("");
            }
            return Either.Right(toString(val));
        },
        decode: val => {
            const parsed = parseInt(val, 10);
            if (!required || !isNaN(parsed)) {
                return isNaN(parsed) ? Either.Right(null) : Either.Right(parsed);
            }

            return Either.Left(`Cannot convert "${val}" to Int.`);
        }
    }),
    FLOAT: required => ({
        encode: val => {
            if (typeof val !== "number") {
                return required ? Either.Left("Int value required and not set.") : Either.Right("");
            }
            return Either.Right(toString(val));
        },
        decode: val => {
            const parsed = parseFloat(val, 10);
            if (!required || !isNaN(parsed)) {
                return isNaN(parsed) ? Either.Right(null) : Either.Right(parsed);
            }

            return Either.Left(`Cannot convert "${val}" to Float.`);
        }
    }),
    STRING: (required, maxLen) => ({
        encode: val =>
            required && !val
                ? Either.Left("String value required and not set.")
                : Either.Right(val),
        decode: val => {
            if (required && !val.length) {
                return Either.Left("String value required but nothing was given");
            }

            if (val.length > maxLen) {
                return Either.Left(`String value exeeds maximum length of ${maxLen}: ${val}`);
            }

            return Either.Right(val);
        }
    }),
    TIMESTAMP: required => ({
        encode: val =>
            val instanceof Date
                ? Either.Right(toSQLTimestamp(val))
                : Either.Left("String value required and not set."),
        decode: val => {
            const parsed = new Date(val);
            if (!required || !isNaN(parsed.valueOf())) {
                return isNaN(parsed) ? Either.Right(null) : Either.Right(parsed);
            }

            return Either.Left(`TimeStamp value of "${val}" is not valid for this field`);
        }
    }),
    DATE: required => ({
        encode: val =>
            val instanceof Date
                ? Either.Right(toSQLDate(val))
                : Either.Left("String value required and not set."),
        decode: val => {
            const parsed = new Date(val);
            if (!required || !isNaN(parsed.valueOf())) {
                return isNaN(parsed) ? Either.Right(null) : Either.Right(parsed);
            }

            return Either.Left(`Date value of "${val}" is not valid for this field`);
        }
    })
};

// ============================================================================
const TABLESCHEMA = [
    { name: "Name", type: TYPES.STRING(true, 50) },
    { name: "Salary", type: TYPES.FLOAT(true) }
];

const parseCell = (cellSchema, value) => {
    if (!cellSchema) {
        return Either.Left(`No schema found for cell with value "${value}"`);
    }

    const parsed = cellSchema.type.decode(value);
    return parsed.bimap(err => `${cellSchema.name}: ${err}`, a => a);
};

// Either [val]
const parseRow = (schema, row) => {
    const arrayOfEithers = row.map((cell, cellIndex) =>
        parseCell(schema[cellIndex], cell).bimap(err => `${cellIndex}: ${err}`, a => a)
    );
    return traverse(Either.of, a => a, arrayOfEithers);
};

const conformToSchema = (schema, rows) => {
    const arrayOfEithers = rows.map((row, rowIndex) =>
        parseRow(schema, row).bimap(err => `${rowIndex}:${err}`, a => a)
    );
    return traverse(Either.of, a => a, arrayOfEithers);
};

// ============================================================================
const readFile = filePath =>
    Future((reject, resolve) =>
        fs.readFile(filePath, (err, data) => (err ? reject(err) : resolve(data)))
    );

const parseCsv = input =>
    Future((reject, resolve) =>
        parse(input, (err, output) => (err ? reject(err) : resolve(output)))
    );
const removeHeaders = input => input.slice(1);

const getAll = filePath =>
    readFile(filePath).chain(parseCsv).map(removeHeaders).chain(rows =>
        Future((reject, resolve) => {
            const output = conformToSchema(TABLESCHEMA, rows);
            output.either(reject, resolve);
        })
    );

const add = () => {};

module.exports = {
    getAll,
    add
};
