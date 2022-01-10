exports.getFreeCapacity = function(store) { 
  return function() { return store.getFreeCapacity(); } 
}