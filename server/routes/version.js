const exec = require("child_process").exec;
const { Future } = require("ramda-fantasy");
const purifier = require("root-require")("./server/lib/routePurifier");

const runCommand = command =>
    Future((reject, resolve) => {
        exec(
            command,
            (err, stdout, stderr) =>
                err ? reject({ error: err, message: stderr }) : resolve(stdout)
        );
    });

module.exports = () =>
    runCommand(`cd ${process.cwd()} && git log | head -n 5`).map(info =>
        purifier.respond.custom({
            content: info
        })
    );
