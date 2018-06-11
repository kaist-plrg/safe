function fromCharCode () {
    // Returns a String value containing as many characters as the number of arguments. Each argument specifies one character of the resulting String, with the first argument specifying the first character, and so on, from left to right. An argument is converted to a character by applying the operation ToUint16 (9.7) and regarding the resulting 16-bit integer as the code unit value of a character. If no arguments are supplied, the result is the empty String.
    // Need ToUint16;
    // Need unicode-16 -> char translation
    return @StrTop
  }
  