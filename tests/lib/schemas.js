/* eslint-env jasmine */
const { TYPES, encodeTable, decodeTable } = require("root-require")("./server/lib/schemas");

describe("The schemas module", () => {
    const completeTypes = {
        DATE: TYPES.DATE(true),
        TIMESTAMP: TYPES.TIMESTAMP(true),
        FLOAT: TYPES.FLOAT(true),
        INT: TYPES.INT(true),
        STRING: TYPES.STRING(true, 50)
    };

    const SAMPLESCHEMA = [
        { name: "DATE field", type: completeTypes.DATE },
        { name: "TIMESTAMP field", type: completeTypes.TIMESTAMP },
        { name: "FLOAT field", type: completeTypes.FLOAT },
        { name: "INT field", type: completeTypes.INT },
        { name: "STRING field", type: completeTypes.STRING }
    ];

    it("correctly encodes DATE", done => {
        const data = new Date("Thu Jun 22 2017 20:07:26 GMT+0000 (GMT)");
        completeTypes.DATE.encode(data).either(done.fail, val => {
            expect(val).toBe("2017-06-22");
            done();
        });
    });

    it("correctly encodes TIMESTAMP", done => {
        const data = new Date("Thu Jun 22 2017 20:07:26 GMT+0100 (BST)");
        completeTypes.TIMESTAMP.encode(data).either(done.fail, val => {
            expect(val).toBe("2017-06-22 20:07:26");
            done();
        });
    });

    it("correctly encodes STRING", done => {
        const data = "Thu Jun 22 2017 20:07:26 GMT+0100 (GMT)";
        completeTypes.STRING.encode(data).either(done.fail, val => {
            expect(val).toBe(data);
            done();
        });
    });

    it("correctly encodes INT", done => {
        const data = 10;
        completeTypes.INT.encode(data).either(done.fail, val => {
            expect(val).toBe("10");
            done();
        });
    });
    it("correctly encodes FLOAT", done => {
        const data = 5.9;
        completeTypes.FLOAT.encode(data).either(done.fail, val => {
            expect(val).toBe("5.9");
            done();
        });
    });

    it("is able to decode what it encodes", done => {
        const sampletable = [
            [new Date("2017-06-22"), new Date("2017-06-22 20:07:26"), 29.5, 30, "Some Value"]
        ];

        let firstEncoding;

        encodeTable(SAMPLESCHEMA)(sampletable)
            .map(v => (firstEncoding = v))
            .chain(decodeTable(SAMPLESCHEMA))
            .chain(encodeTable(SAMPLESCHEMA))
            .fork(done.fail, result => {
                expect(JSON.stringify(result)).toBe(JSON.stringify(firstEncoding));
                done();
            });
    });
});
