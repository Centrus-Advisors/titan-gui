/* eslint-disable quote-props */
// List all available tasks

// Load environment variables
// Environment variables loaded here are accessible in all gulp tasks
require("dotenv").config({ path: "./_env/.env" });
const organiser = require("gulp-organiser");
const path = require("path");

// Front-end stuff
const src = "./server/.assets-precompile";
const dest = "./server/assets";

organiser.registerAll("./tasks", {
    "build-elm": {
        "input-form": {
            watch: path.join(src, "elm/**/*"),
            src: path.join(src, "elm/InputForm/Main.elm"),
            dest: path.join(dest, "elm"),
            moduleName: "input-form",
            ext: "js"
        },
        "download-form": {
            watch: path.join(src, "elm/**/*"),
            src: path.join(src, "elm/DownloadForm/Main.elm"),
            dest: path.join(dest, "elm"),
            moduleName: "download-form",
            ext: "js"
        }
    },
    "jasmine-test-node": {
        src: ["tests/**/*"]
    },
    build: {
        src: ".",
        tasks: ["build-elm"]
    },
    watch: {
        src: ".",
        taskNames: ["build-elm"]
    }
});
