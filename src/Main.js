exports.updateProgress = function(state) {
  return function() {
    document.getElementById("progress").style.width = (100 * (state.score / state.high)) + '%';
  };
};
