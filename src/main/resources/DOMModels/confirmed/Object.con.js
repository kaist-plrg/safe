function Object (value) {
    if (value === null || value === undefined) return {};
    else return @ToObject(value);
  }
  