/* eslint-disable global-require, import/no-dynamic-require */
// Load environment variables
const express = require("express");
const morgan = require("morgan");
const bodyParser = require("body-parser");
const exphbs = require("express-handlebars");
const routes = require("./routes");
const path = require("path");
const logger = require("root-require")("./server/lib/logger");
const http = require("http");

module.exports = (options = {}) => {
    const app = express();

    app.use(bodyParser.json()); // for parsing application/json
    app.use(bodyParser.urlencoded({ extended: true }));

    // SETUP VIEW ENGINE
    app.engine(
        ".hbs",
        exphbs({
            extname: ".hbs",
            defaultLayout: "main",
            layoutsDir: path.join(`${__dirname}/views/layouts`),
            partialsDir: path.join(`${__dirname}/views/partials`)
        })
    );
    app.set("view engine", ".hbs");
    app.set("views", path.join(`${__dirname}/views/templates`));

    // LOG REQUESTS (this must come before routes)
    if (options.logRequests) {
        const morganConfig = {
            stream: {
                write: m => logger.info(m)
            }
        };

        app.use(morgan("combined", morganConfig));
    }

    const httpServer = http.Server(app);

    // SETUP ROUTES (this must be the last addition to app)
    routes(app);

    return httpServer;
};
