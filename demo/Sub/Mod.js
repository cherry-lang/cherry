"use strict";

function ms() {
    if (arguments.length === 1.0) {
        return function (x) {
            return 'Mr.';
        }(arguments[0]);
    }
}
Object.defineProperty(exports, "__esModule", { value: true });
exports.ms = ms;

