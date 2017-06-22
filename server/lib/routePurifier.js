const assert = require("assert");

/**
 *  THIS FILE AIMS TO MAKE ROUTES SOMEHOW PURE
 *
 *      Version 1.0
 */

/**
const typesExamples = [
    {
        type: "redirect",
        path: "/something"
        headers: { "key": "value" },
    }, {
        type: "render",
        status: "200", // optional, default 200
        headers: { "key": "value" },
        template: "dashboard",
        locals: {
          title: "Dashboard"
        }
    }, {
        type: "custom",
        status: "200", // optional, default 200
        headers: { "key": "value" },
        content: {} // content can be of any type
    }, {
        type: "next",
        headers: { "key": "value" },
        content: {} // content can be of any type
    }, {
        type: "json",
        headers: { "key": "value" },
        content: {} // content must be an object
    }

];

**/

// const allowedFields = {
//     // eslint-disable-line
//     type: "redirect", // what response type this is
//     path: "/login", // for redirection only
//     status: 200,
//     template: "dashboard", // to be used with render
//     locals: {}, // to be used with render
//     content: {},
//     error: new Error("Bugger!"),
//     flash: ["info", "Something happened"] // flash card to be displayed
// };

const types = {
    REDIRECT: "redirect",
    RENDER: "render",
    NEXT: "next",
    CUSTOM: "custom",
    JSON: "json"
};

const redirect = response => Object.assign({}, response, { type: types.REDIRECT });

const render = response => Object.assign({}, response, { type: types.RENDER });

const custom = response => Object.assign({}, response, { type: types.CUSTOM });

const next = response => Object.assign({}, response, { type: types.NEXT });

const json = response => Object.assign({}, response, { type: types.JSON });

const sendResponse = (routeHandler, req, res, nxt, err) =>
    routeHandler(req, res, err)
        // Add flash card
        .map(response => {
            if (response.flash !== undefined) {
                assert(Array.isArray(response.flash), "response.flash must be an array");
                req.flash(response.flash[0], response.flash[1]);
            }

            return response;
        })
        // Set response status
        .map(response => {
            if (response.status !== undefined) {
                res.status(response.status);
            }

            return response;
        })
        // Set response headers
        .map(response => {
            res.set(response.headers || {});
            return response;
        })
        /* eslint-disable complexity */
        .fork(nxt, ({ type, template, path, locals, content, error }) => {
            /* eslint-enable complexity */
            const allLocals = Object.assign(res.locals, locals);

            switch (type) {
            case types.REDIRECT:
                res.redirect(path);
                break;
            case types.NEXT:
                nxt(error);
                break;
            case types.RENDER:
                res.render(template, allLocals);
                break;
            case types.CUSTOM:
                res.send(content);
                break;
            case types.JSON:
                res.json(content);
                break;
            default:
                nxt(`Invalid response type: ${type}`);
            }
        });

const route = routeHandler => (req, res, nxt) => sendResponse(routeHandler, req, res, nxt);

const middleware = routeHandler => (err, req, res, nxt) =>
    sendResponse(routeHandler, req, res, nxt, err);

module.exports = {
    route,
    middleware,
    respond: {
        redirect,
        render,
        custom,
        next,
        json
    }
};
