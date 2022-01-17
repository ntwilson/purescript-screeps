exports.resource_energy = RESOURCE_ENERGY;
exports.resourceEq = function(a, b) { return a == b; }
exports.freeCapacityImpl = function(resource, store) { return store.getFreeCapacity(resource); }
exports.usedCapacityImpl = function(resource, store) { return store.getUsedCapacity(resource); }
