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
    `${date.getFullYear()}-${twoDigits(1 + date.getMonth())}-${twoDigits(
        date.getDate()
    )} ${twoDigits(date.getHours())}:${twoDigits(date.getMinutes())}:${twoDigits(
        date.getSeconds()
    )}`;

const toSQLDate = date =>
    `${date.getFullYear()}-${twoDigits(1 + date.getMonth())}-${twoDigits(date.getDate())}`;

// ============================================================================

// Decode
const TYPES = {
    INT: required => ({
        encode: val => {
            if (typeof val !== "number") {
                return required ? Either.Left("Int value required and not set.") : Either.Right("");
            }
            return Either.Right(val.toString());
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
            return Either.Right(val.toString());
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

// =============              SCHEMA PROCESSING                  ==============
// ============================================================================

const TABLESCHEMA = [
    { name: "Name", type: TYPES.STRING(true, 50) },
    { name: "Salary", type: TYPES.FLOAT(true) }
];

const processCell = (method, cellSchema, value) => {
    if (!cellSchema) {
        return Either.Left(`No schema found for cell with value "${value}"`);
    }

    const parsed = cellSchema.type[method](value);
    return parsed.bimap(err => `${cellSchema.name}: ${err}`, a => a);
};

// Either [val]
const processRow = (method, schema, row) => {
    const arrayOfEithers = row.map((cell, cellIndex) =>
        processCell(method, schema[cellIndex], cell).bimap(err => `${cellIndex}: ${err}`, a => a)
    );
    return traverse(Either.of, a => a, arrayOfEithers);
};

const eitherToFuture = e => Future((reject, resolve) => e.bimap(reject, resolve));
// Future
const processTable = (method, schema, rows) => {
    const arrayOfEithers = rows.map((row, rowIndex) =>
        processRow(method, schema, row).bimap(err => `${rowIndex}:${err}`, a => a)
    );

    const processed = traverse(Either.of, a => a, arrayOfEithers);
    return eitherToFuture(processed);
};

const addHeaders = schema => rows => [schema.map(s => s.name)].concat(rows);

const removeHeaders = input => input.slice(1);

const decodeTable = schema => rows => processTable("decode", schema, removeHeaders(rows));

const encodeTable = schema => rows => processTable("encode", schema, rows).map(addHeaders(schema));

// ============================================================================

module.exports = {
    decodeTable,
    encodeTable,
    TYPES
};
