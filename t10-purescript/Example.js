exports.add = function(a) {
  return function(b) {
    return a + b + "";
  };
}
