var progressText;

var ProgressText = Class.create();
ProgressText.prototype = {
  initialize: function() {
    this.text = 'Now Loading';
    this.interval = 250;
    this.countMax = 5;
  },

  start: function(element) {
    this.stop();
    document.body.style.cursor = 'wait';
    if ( element ) {
      this.element = element;
      this.count = 0;
      this.changeText();
      this.intervalId = setInterval(this.changeText.bind(this), this.interval);
    }
  },

  stop: function() {
    document.body.style.cursor = 'auto';
    if ( this.intervalId ) {
      clearInterval(this.intervalId);
      this.intervalId = null;
      this.element.innerHTML = '';
    }
  },

  changeText: function() {
    var bufferArray = [this.text];
    for (i = 0; i < this.count; i++) {
      bufferArray.push(' .');
    }
    this.element.innerHTML = bufferArray.join('');
    this.count++;
    if (this.count > this.countMax) {
      this.count = 0;
    }
  }
}
