const winston = require("winston");

const debugMode =
    process.env.DEBUG &&
    process.env.DEBUG.toString().trim().toLowerCase() === "true";

winston.add(winston.transports.File, {
    filename: `${process.cwd()}/_env/server.log`,
    handleExceptions: true,
    json: false,
    timestamp: () => new Date(),
    formatter: options => {
        // Return string will be passed to logger.
        const timestamp = options.timestamp();
        const level = options.level.toUpperCase();
        const message = options.message ? options.message : "";
        const meta = options.meta && Object.keys(options.meta).length
            ? `\n\t${JSON.stringify(options.meta)}`
            : "";
        return `${timestamp} ${level} ${message} ${meta}`;
    }
});

winston.remove(winston.transports.Console);
winston.add(winston.transports.Console, {
    level: debugMode ? "silly" : "verbose",
    handleExceptions: true,
    json: false,
    /* eslint-disable complexity */
    formatter: options => {
        /* eslint-enable complexity */
        // TODO: Test this formatter
        const message = options.message || "";
        const level = `[${options.level.toUpperCase()}]`;

        let stack;
        if (options.meta && options.meta.stack && options.meta.stack.join) {
            stack = options.meta.stack.join("\n");
        } else if (options.meta && options.meta.stack) {
            stack = options.meta.stack;
        } else {
            stack = "";
        }
        return options.level === "error"
            ? `\n    ${level} ${message} \n\n ${stack}`
            : `${level} ${message} ${stack}`;
    }
});
winston.handleExceptions();

module.exports = winston;
