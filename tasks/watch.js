/* eslint-disable no-console */
//
//
// Select which tasks to watch
//
//
const gulp = require("gulp");
const organiser = require("gulp-organiser");

const shallowTask = aTask =>
    Object.assign({}, aTask, {
        watch: aTask.tasks.map(t => t.watch || t.src).reduce((acc, v) => acc.concat(v), [])
    });

module.exports = organiser.register((task, allTasksConfig) => {
    const toWatch = allTasksConfig
        .map(t => (t.tasks.length > 1 ? t.tasks.concat(shallowTask(t)) : t.tasks))
        .reduce((acc, ts) => acc.concat(ts), []);

    toWatch.forEach(t => {
        gulp.task(`watch:${t.name}`, () => {
            console.log(`watching ${t.name}`);
            gulp.watch(t.watch || t.src, [t.name]);
        });
    });
});
