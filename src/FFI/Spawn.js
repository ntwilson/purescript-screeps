exports.isStructureSpawn = function(struct) { return struct instanceof StructureSpawn; } 
exports.work = WORK;
exports.move = MOVE;
exports.carry = CARRY;
exports.spawnOk = OK;
exports.spawnStatusEq = function (a, b) { return a == b; }
exports.spawnCreepImpl = function(parts, name, memory, spawn) {
  return spawn.spawnCreep(parts, name, memory);
};