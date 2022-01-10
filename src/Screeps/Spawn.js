exports.isStructureSpawn = function(struct) { return struct instanceof StructureSpawn; } 

exports.spawnCreepImpl = function(parts, name, memory, spawn) {
  return spawn.spawnCreep(parts, name, memory);
};