/* eslint-disable global-require, no-console */
const gulp = require("gulp");
const organiser = require("gulp-organiser");
const { Future } = require("ramda-fantasy");
const shell = require("gulp-shell");

const runCommand = (command, options) =>
    Future((reject, resolve) => {
        const task = shell.task(command, options);

        task(err => (err ? reject(err) : resolve()));
    });

module.exports = organiser.register(task => {
    gulp.task(task.name, cb =>
        task.tasks
            .map(t => `gulp ${t}`)
            .map(c => runCommand(c, { showStack: true }))
            .reduce((acc, t) => acc.chain(() => t))
            .fork(
                err => {
                    console.log("ERROR IN BUILD");
                    console.log(err);
                    cb(err);
                },
                () => cb()
            )
    );
});
