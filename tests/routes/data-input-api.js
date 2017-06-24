/* eslint-env jasmine*/
const request = require("supertest");
const { curry } = require("ramda");
const { Future } = require("ramda-fantasy");
const database = require("root-require")("./server/lib/database");
const appFabric = require("root-require")("./server");

const mockDatabase = content => {
    database.loadDb = () => Future.of(content);
    database.save = curry((dbName, record) =>
        database.loadDb(dbName).map(db => db.concat(record))
    );
};

const date1 = new Date("2017-06-01").toISOString();
const date2 = new Date("2017-06-05").toISOString();
const date3 = new Date("2017-06-10").toISOString();

const mockData = [
    {
        name: "John",
        createdAt: date1
    },
    {
        name: "Karl",
        createdAt: date2
    },
    {
        name: "James",
        createdAt: date3
    }
];

describe("/data-input-api ", () => {
    let app;

    beforeAll(() => {
        mockDatabase(mockData);
        app = appFabric();
    });

    describe("GET", () => {
        it("returns everything if no 'from' or to 'date' is specified", done => {
            request(app)
                .get("/data-input-api")
                .expect(200)
                .end((err, response) => {
                    expect(response.text).toBe(
                        `name,createdAt
John,2017-06-01T00:00:00.000Z
Karl,2017-06-05T00:00:00.000Z
James,2017-06-10T00:00:00.000Z
`
                    );
                    return err ? done.fail(err) : done();
                });
        });

        it("returns all items after 'fromDate' if 'toDate' is not specified", done => {
            request(app)
                .get("/data-input-api?fromDate=2017-06-02")
                .expect(200)
                .end((err, response) => {
                    expect(response.text).toBe(
                        `name,createdAt
Karl,2017-06-05T00:00:00.000Z
James,2017-06-10T00:00:00.000Z
`
                    );
                    return err ? done.fail(err) : done();
                });
        });

        it("returns all items after 'toDate' if 'fromDate' is not specified", done => {
            request(app)
                .get("/data-input-api?toDate=2017-06-09")
                .expect(200)
                .end((err, response) => {
                    expect(response.text).toBe(
                        `name,createdAt
John,2017-06-01T00:00:00.000Z
Karl,2017-06-05T00:00:00.000Z
`
                    );
                    return err ? done.fail(err) : done();
                });
        });

        it("returns all items between 'fromDate' and 'toDate' when both are specified", done => {
            request(app)
                .get("/data-input-api?toDate=2017-06-09&fromDate=2017-06-02")
                .expect(200)
                .end((err, response) => {
                    expect(response.text).toBe(
                        `name,createdAt
Karl,2017-06-05T00:00:00.000Z
`
                    );
                    return err ? done.fail(err) : done();
                });
        });

        it("returns 'No Content' if result should be empty", done => {
            request(app)
                .get("/data-input-api?toDate=2017-06-09&fromDate=2017-06-10")
                .expect(200)
                .end((err, response) => {
                    expect(response.text).toBe("No Content");
                    return err ? done.fail(err) : done();
                });
        });
    });
});
