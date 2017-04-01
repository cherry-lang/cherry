"use strict";

var _Prelude = require("./Js/Prelude.js");

function _plus() {
    if (arguments.length === 2.0) {
        return function (x, y) {
            return (0, _Prelude.add)(x, y);
        }(arguments[0], arguments[1]);
    }
}

function _slash() {
    if (arguments.length === 2.0) {
        return function (x, y) {
            return (0, _Prelude.divide)(x, y);
        }(arguments[0], arguments[1]);
    }
}
Object.defineProperty(exports, "__esModule", { value: true });
exports._slash = _slash;
exports._plus = _plus;
exports.add = _Prelude.add;

