"use strict";

var _Prelude = require("./Prelude.js");

var _Mod = require("./Sub/Mod.js");

function greet() {
    if (arguments.length === 1.0) {
        return function (name) {
            return (0, _Prelude.add)((0, _Mod.ms)(name), name);
        }(arguments[0]);
    }
}
Object.defineProperty(exports, "__esModule", { value: true });
exports.greet = greet;
exports.ms = _Mod.ms;

