exports.err_not_in_range = ERR_NOT_IN_RANGE;
exports.resource_energy = RESOURCE_ENERGY;
exports.statusCodeEq = function(statusA, statusB) { return statusA === statusB; }
exports.payloadEq = function(payloadA, payloadB) { return payloadA === payloadB; }
exports.setMemImpl = function(mem, creep) { creep.memory = mem; }
