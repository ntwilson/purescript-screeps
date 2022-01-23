
exports.memory = function () { return Memory }
exports.deleteCreepByName = function (name) { return function() { delete Memory.creeps[name]; } }
