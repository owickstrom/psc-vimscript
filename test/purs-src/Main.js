exports.addInt = function (x) {
  return function (y) {
    return x + y;
  };
}
exports.echo = function (x) {
  return function () {
    console.log(x);
  };
}
