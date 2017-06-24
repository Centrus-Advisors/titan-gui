const csvparse = require("csv-parse");
const csvstringify = require("csv-stringify");
const { Future } = require("ramda-fantasy");

const parse = input =>
    Future((reject, resolve) =>
        csvparse(input, (err, output) => (err ? reject(err) : resolve(output)))
    );

const stringify = input =>
    Future((reject, resolve) => {
        csvstringify(
            input,
            { header: true },
            (err, output) => (err ? reject(err) : resolve(output))
        );
    });

module.exports = {
    parse,
    stringify
};
