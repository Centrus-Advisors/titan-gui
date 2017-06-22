require("dotenv").config({ path: "./_env/.env" });
const Future = require("ramda-fantasy").Future;
const logger = require("./server/lib/logger");
const app = require("./server")({
    logRequests: true
});

const PORT = 8080;
const server = app.listen(PORT, () =>
    logger.info(`Centrus CRM listening on port ${PORT}`)
);

// Make sure we finish connections on SIGTERM
process.on("SIGTERM", () => {
    logger.info("Shutting server down...");
    Future((reject, resolve) => server.close(resolve))
        .map(() => logger.info("Shutdown complete."))
        .fork(
            err => {
                logger.error(err);
                process.exit(1);
            },
            () => process.exit(0)
        );
});
