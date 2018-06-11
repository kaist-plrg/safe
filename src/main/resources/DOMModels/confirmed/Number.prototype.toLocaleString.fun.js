function toLocaleString () {
    if (!@SameValue(typeof this,'number') && !@SameValue(@Class(this), 'Number')){
      throw new TypeError();
    }
    return @ToString(this);
  }
  