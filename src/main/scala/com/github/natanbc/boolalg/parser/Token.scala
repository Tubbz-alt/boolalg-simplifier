package com.github.natanbc.boolalg.parser

case class Token(kind: TokenKind, pos: (Int, Int), value: String)
