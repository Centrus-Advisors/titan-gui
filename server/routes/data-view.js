const purifier = require("root-require")("./server/lib/routePurifier");
const Future = require("ramda-fantasy").Future;

module.exports = () =>
    Future.of(
        purifier.respond.render({
            locals: {
                title: "Data View"
            },
            template: "data-view"
        })
    );
