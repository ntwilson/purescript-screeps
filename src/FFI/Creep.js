exports.err_not_in_range = ERR_NOT_IN_RANGE;
exports.ok = OK;
exports.find_hostile_creeps = FIND_HOSTILE_CREEPS;
exports.find_my_creeps = FIND_MY_CREEPS;
exports.statusCodeEq = function(statusA, statusB) { return statusA === statusB; }
exports.setMemImpl = function(mem, creep) { creep.memory = mem; }
exports.getRawMem = function(creep) { return creep.memory; }
