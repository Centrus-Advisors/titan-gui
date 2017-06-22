const gulp = require("gulp");
const path = require("path");
const organiser = require("gulp-organiser");
// const execSync = require("child_process").execSync;
const shell = require("gulp-shell");

module.exports = organiser.register(task => {
    const { ext = "js", src, dest, moduleName = path.parse(task.src).name } = task;

    const output = path.join(dest, `${moduleName}.${ext}`);
    gulp.task(task.name, shell.task(`elm-make --yes ${src} --output=${output}`));
});
