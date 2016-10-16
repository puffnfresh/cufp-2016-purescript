exports.sizeBar = function(state) {
  return function() {
    document.getElementById('bar').style.width =
      (state.current / state.high) * 100 + '%';
  };
};
