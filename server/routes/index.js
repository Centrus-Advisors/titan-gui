/* eslint-disable no-param-reassign*/
const purifier = require("root-require")("./server/lib/routePurifier");

// Load files in this folder apart from "index.js"
const routes = require("require-dir-all")(".", {
    recursive: true,
    indexAsParent: true
});
const express = require("express");

module.exports = app => {
    // This route allows us to see which commit is currently running
    app.get("/version", purifier.route(routes.version));
    app.get("/", (req, res) => res.redirect("/data-input"));
    app.get("/data-input", purifier.route(routes["data-input"]));

    // Static data
    app.use("/assets", express.static(`${__dirname}/../assets`));
};
