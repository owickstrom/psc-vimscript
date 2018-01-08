exports.echo = function (x) {
  return function () {
    console.log(x);
  };
}
