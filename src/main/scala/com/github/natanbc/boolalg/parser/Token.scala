package com.github.natanbc.boolalg.parser

case class Token(kind: TokenKind, pos: (Int, Int), value: String) {
  def expect(kind: TokenKind): Unit = {
    if(kind != this.kind) {
      unexpected(s"token of type $kind")
    }
  }
  
  def unexpected(msg: String): Nothing = {
    throw new IllegalArgumentException(s"Expected $msg, got ${this.kind} ($value) at line ${pos._1}, column ${pos._2}")
  }
}